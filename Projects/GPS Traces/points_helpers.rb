# returned when points are valid
class PointsResultValid
  attr_reader :points

  def initialize(points)
    @points = points
  end

  def err?
    false
  end
end

# returned when points are invalid
class PointsResultInvalid
  attr_reader :error

  def initialize(error)
    @error = error
  end

  def err?
    true
  end
end

# make sure `points` is an Array, and validate each point. returns
# [true, <sanitized points>] if points is valid and [false, <error
# message>] when `points` is invalid.
def valid_points(points)
  return PointsResultInvalid.new('Invalid trace, must be an array') \
    unless points.is_a? Array

  return PointsResultInvalid.new('Cannot add an empty trace') \
    if points.empty?

  new_points = []

  points.each do |p|
    res = valid_point(p)
    return res if res.err?

    new_points += [res.points]
  end

  PointsResultValid.new(new_points)
end

# make sure `point` is valid, return [true, <sanitized point>] if so,
# otherwise return [false, <error message>]
def valid_point(point)
  return PointsResultInvalid.new('Invalid point object, must be a hash') \
    unless point.is_a? Hash

  return PointsResultInvalid.new('Invalid object: no latitude property') \
    unless point.key? :latitude

  return PointsResultInvalid.new('Invalid non-float latitude property') \
    unless point[:latitude].is_a? Float

  return PointsResultInvalid.new('Invalid object: no longitude property') \
    unless point.key? :longitude

  return PointsResultInvalid.new('Invalid non-float longitude property') \
    unless point[:longitude].is_a? Float

  PointsResultValid.new({ latitude: point[:latitude],
                          longitude: point[:longitude] })
end
