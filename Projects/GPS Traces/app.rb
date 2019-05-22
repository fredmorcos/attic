# some potential performance improvements:

# - support json minification in response contents (seems the ruby
#   json module already generates minified json by default)
# - support gzipped json request (would be a bad idea? due to
#   carefully crafted gzip bombs) and response contents
# - do some caching, probably at the mongodb/mongoid level?
# - do some caching with an in-memory db (redis? memcached?)

# some issues:

# - probably should not expose the internal mongodb IDs and instead
#   either use UUIDs for traces or use a hash of the mongodb ID

# - ID values keep being incremented even if a trace hasn't been
#   committed to the database (due to whatever reason). I tried to
#   solve this issue with mongo sessions and transactions to make the
#   max_id++ and commit operations atomic, but I keep an error
#   "Transaction numbers are only allowed on a replica set member or
#   mongos (20)"

# some notes:

# - I hope that mongoid uses tokens when querying the database and
#   accessing documents fields (lazy evaluation) rather than eagerly
#   fetching data, because doing it eagerly would mean that something
#   like `traces = Trace.all` would load up the entire database into
#   memory only to later access a few fields

# - The pattern of `begin ... rescue halt ... end` is repetitive, but
#   I am unsure whether factoring it out into a separate function
#   would break the halt mechanism from sinatra

# - It is probably not a good idea to "catch all exceptions" but since
#   this is only happening in the top-level layer of the API (the only
#   layer so far), then it's probably okay

# - It is probably better to implement a points collection and a
#   traces collection that are separate, and implement a many-to-many
#   relationship between them. This might give mongodb the opportunity
#   to deduplicate points. However I'd prefer to keep it simple and I
#   don't think it's a major performance or space improvement (if any
#   at all), it's just a more sophisticated db design.

# TODO: write tests
# TODO: find a clean way to use int IDs instead of DB IDs
# TODO: support pagination with params in GET /traces
# DONE: return JSON objects with error descr next to halt errcode
# DONE: make sure DB contents persist after DB restarts

require 'rubygems'
require 'bundler/setup'

require 'sinatra/base'
require 'mongo'
require 'haversine'

require './err_handlers.rb'
require './points_helpers.rb'
require './dist_helpers.rb'
require './elev_helpers.rb'

# The web-app class
class GPSTraces < Sinatra::Base
  Mongo::Logger.logger.level = Logger::FATAL

  @@client = Mongo::Client.new(['127.0.0.1:27017'], database: 'gpstraces')
  @@db = @@client.database
  @@traces = @@client[:traces]

  disable :sessions
  disable :static
  disable :run

  # stream all available traces, if a DB or JSON generation error
  # occurs, return error 500
  get '/traces' do
    content_type :json

    traces = {}
    @@traces.find.map do |doc|
      traces[doc['_id'].to_s] = doc['points']
    end

    begin
      JSON.generate traces
    rescue StandardError => e
      print_exc e
      halt 500, err('Error generating JSON')
    end
  end

  # get the trace with ID :id in the response contents. if the ID does
  # not exist, respond with 404, if a DB or JSON generation error
  # occurs, return error 500
  get '/traces/:id' do |id|
    content_type :json

    begin
      trace = @@traces.find(_id: BSON::ObjectId(id))

      if trace.count.zero?
        halt 404, err('ID does not exist')
      elsif trace.count > 1
        halt 500, err('Database is in an invalid state')
      end

      trace = trace.map { |doc| doc }.first
    rescue StandardError => e
      print_exc e
      halt 404, err('Unknown database error')
    end

    res = add_distances(trace[:points])
    halt 500, err(res.error) if res.err?

    needs_store = !res.already_valid?

    res = add_elevations(res.points)
    halt 500, err(res.error) if res.err?

    needs_store |= !res.already_valid?

    if needs_store
      # store points back into the db with the distances
      @@traces.update_one({ _id: id }, { '$set': { points: res.points } })
    end

    begin
      JSON.generate res.points
    rescue StandardError => e
      print_exc e
      halt 500, err('Error generating JSON')
    end
  end

  # this creates a trace with the request contents and returns the trace
  # object in the response contents, return a 400 error when the
  # incoming JSON content is invalid and return a 500 error when DB or
  # JSON generation errors occur
  post '/traces' do
    content_type 'text/plain'

    begin
      points = JSON.parse(request.body.read,
                          {
                            allow_nan: false,
                            symbolize_names: true
                          })
    rescue StandardError => e
      print_exc e
      halt 400, err('Invalid JSON in request contents')
    end

    res = valid_points(points)
    halt 500, err(res.error) if res.err?

    res = add_distances(res.points)
    halt 500, err(res.error) if res.err?

    res = add_elevations(res.points)
    halt 500, err(res.error) if res.err?

    begin
      res = @@traces.insert_one(points: res.points)
    rescue StandardError => e
      print_exc e
      halt 500, err('Could not save trace')
    end

    res.inserted_id.to_s
  end

  # this updates a trace with ID :id with the request contents and
  # returns the trace object in the response contents, if the ID does
  # not exist, respond with 404, and if the incoming JSON content is
  # invalid, return a 400 error. if DB or JSON generation errors occur
  # then respond with error 500
  put '/traces/:id' do |id|
    content_type 'text/plain'

    begin
      points = JSON.parse(request.body.read,
                          {
                            allow_nan: false,
                            symbolize_names: true
                          })
    rescue StandardError => e
      print_exc e
      halt 400, err('Invalid JSON in request contents')
    end

    res = valid_points(points)
    halt 500, err(res.error) if res.err?

    begin
      trace = @@traces.find(_id: BSON::ObjectId(id))

      if trace.count.zero?
        halt 404, err('ID does not exist')
      elsif trace.count > 1
        halt 500, err('Database is in an invalid state')
      end

      res = add_distances(res.points)
      halt 500, err(res.error) if res.err?

      res = add_elevations(res.points)
      halt 500, err(res.error) if res.err?

      trace.update_one('$set': { points: res.points })
    rescue StandardError => e
      print_exc e
      halt 404, err('Unknown database error')
    end

    ''
  end

  # this deletes a trace with ID :id from the database and returns the
  # deleted trace object in the response contents, if the ID does not
  # exist, respond with 404, return error 500 for DB and JSON generation
  # errors
  delete '/traces/:id' do |id|
    content_type 'text/plain'

    begin
      tmp = @@traces.find(_id: BSON::ObjectId(id))

      if tmp.count.zero?
        halt 404, err('ID does not exist')
      elsif tmp.count > 1
        halt 500, err('Database is in an invalid state')
      end

      trace = tmp.map { |doc| doc }.first
    rescue StandardError => e
      print_exc e
      halt 404, err('Unknown database error')
    end

    begin
      tmp.delete_one
    rescue StandardError => e
      print_exc e
      halt 500, err('Could not delete trace')
    end

    res = add_distances(trace[:points])
    halt 500, err(res.error) if res.err?

    res = add_elevations(res.points)
    halt 500, err(res.error) if res.err?

    ''
  end

  run! if app_file == $0
end
