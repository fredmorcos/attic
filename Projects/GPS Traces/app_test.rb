require 'rubygems'
require 'bundler/setup'

require 'test/unit'
require 'rack/test'
require './app'

# TODO: This should probably use a JSON matcher to compare results
#       with expectations.

# TODO: The tests here are somewhat basic. Probably should be
#       rewritten in a session-wise fashion.

# Test the GPSTraces web-app
class AppTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    GPSTraces
  end

  # helper to make sure response bodies have the right type of
  # contents when things should be going well
  def check_valid_trace(body)
    contents = JSON.parse body
    assert_kind_of(Array, contents, 'Response contents is not an Array')
    assert !contents.empty?

    contents.each do |p|
      assert p.key? 'latitude'
      assert p['latitude'].is_a? Float
      assert p.key? 'longitude'
      assert p['longitude'].is_a? Float
      assert p.key? 'distance'
      assert p['distance'].is_a? Float
      assert p.key? 'elevation'
      assert p['elevation'].is_a? Integer
    end

    assert last_response.ok?
    contents
  end

  # post something then get it and make sure it's the same thing
  def test_post_then_get
    header 'Content-Type', 'application/json'

    data = File.read 'sample-traces/0.json'
    post('/traces', data, 'CONTENT_TYPE': 'application/json')
    id = last_response.body

    get "/traces/#{id}"
    check_valid_trace last_response.body
  end

  # try to submit invalid data
  def test_post_invalid
    header 'Content-Type', 'application/json'

    json = JSON.generate [{ latitude: 'wrong', longitude: 'wrong' }]
    post('/traces', json, 'CONTENT_TYPE': 'application/json')

    contents = JSON.parse last_response.body
    assert_kind_of(Hash, contents, 'Response contents is not a hash')
    assert contents.key? 'description'
    assert(!last_response.ok?)
  end

  # post a bunch of valid files and make sure the server doesn't break
  def test_post_sample_traces
    header 'Content-Type', 'application/json'

    (0..4).each do |i|
      data = File.read "sample-traces/#{i}.json"
      post('/traces', data, 'CONTENT_TYPE': 'application/json')
      assert last_response.ok?
    end
  end

  # post a trace, then get its ID, then put another trace into its
  # place which should end up with the same ID
  def test_put
    header 'Content-Type', 'application/json'

    data = File.read 'sample-traces/0.json'
    post('/traces', data, 'CONTENT_TYPE': 'application/json')

    contents = last_response.body
    contents_id = contents['_id']

    data = File.read 'sample-traces/1.json'
    put("/traces/#{contents_id}", data, 'CONTENT_TYPE': 'application/json')
    contents = last_response.body

    assert_equal contents_id, contents['_id']
  end

  # post a trace, get its id and delete it, then try to get it and it
  # should fail
  def test_del
    header 'Content-Type', 'application/json'

    data = File.read 'sample-traces/0.json'
    post('/traces', data, 'CONTENT_TYPE': 'application/json')

    id = last_response.body

    delete "/traces/#{id}"
    assert last_response.ok?

    get "/traces/#{id}"

    contents = JSON.parse last_response.body
    assert_kind_of(Hash, contents, 'Response contents is not a hash')
    assert contents.key? 'description'
    assert(!last_response.ok?)
  end
end
