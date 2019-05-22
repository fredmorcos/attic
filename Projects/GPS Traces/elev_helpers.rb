require 'http-cookie'
require 'rest-client'

URL = 'https://codingcontest.runtastic.com/api/elevations/bulk'.freeze

# returned when a set of points are invalid because some of them have
# the elevation property and some don't
class ElevResultInvalid
  attr_reader :error

  def initialize(error)
    @error = error
  end

  def err?
    true
  end
end

# returned when a set of points are valid and already have the
# elevation property set on all of them
class ElevResultAlreadyValid
  attr_reader :points

  def initialize(points)
    @points = points
  end

  def err?
    false
  end

  def already_valid?
    true
  end
end

# returned when a set of points are valid but don't already have the
# elevation property set on all of them, meaning that the procedure
# did add them for us. this is useful to know whether to store the
# updated points back into the database or not.
class ElevResultValid
  attr_reader :points

  def initialize(points)
    @points = points
  end

  def err?
    false
  end

  def already_valid?
    false
  end
end

# takes a list of points and inserts the elevations. return
# ElevResultAlreadyValid if elevations are already in place,
# ElevResultValid if elevations are not already in place but have been
# calculated and inserted, and ElevResultInvalid if only some points
# have elevations and some don't (this points to a state mismatch
# issue)
def add_elevations(points)
  points_elev = points.select { |p| p.key? :elevation }

  return ElevResultAlreadyValid.new(points) \
    if points_elev.length == points.length

  return ElevResultInvalid.new('Only some points with elevation') \
    unless points_elev.empty?

  points_json = []

  points.each do |p|
    points_json += [{ longitude: p[:longitude], latitude: p[:latitude] }]
  end

  params = { content_type: :json, accept: :json }
  contents = JSON.generate points_json
  response = RestClient.post(URL, contents, params)

  elevations = JSON.parse(response.body,
                          {
                            allow_nan: false,
                            symbolize_names: true
                          })

  return ElevResultInvalid.new('Invalid response from elevations server') \
    unless elevations.is_a? Array

  return ElevResultInvalid.new('Invalid number of elevations from server') \
    unless elevations.length == points.length

  points.each_index do |idx|
    return ElevResultInvalid.new('Elevation must be an integer') \
      unless elevations[idx].is_a? Integer

    points[idx][:elevation] = elevations[idx]
  end

  ElevResultValid.new(points)
end
