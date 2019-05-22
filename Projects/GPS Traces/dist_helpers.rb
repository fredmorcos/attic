# returned when a set of points are invalid because some of them have
# the distance property and some don't
class DistResultInvalid
  attr_reader :error

  def initialize(error)
    @error = error
  end

  def err?
    true
  end
end

# returned when a set of points are valid and already have the
# distance property set on all of them
class DistResultAlreadyValid
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
# distance property set on all of them, meaning that the procedure did
# add them for us. this is useful to know whether to store the updated
# points back into the database or not
class DistResultValid
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

# takes a list of points and inserts the distances. return
# DistResultAlreadyValid if distances are already in place,
# DistResultValid if distances are not already in place but have been
# calculated and inserted, and DistResultInvalid if only some points
# have distances and some don't (this points to a state mismatch
# issue)
def add_distances(points)
  points_dist = points.select { |p| p.key? :distance }

  return DistResultAlreadyValid.new(points) \
    if points_dist.length == points.length

  return DistResultInvalid.new('Only some points with distance') \
    unless points_dist.empty?

  points.each_index do |idx|
    if idx.zero?
      points[0][:distance] = 0.0
    else
      src = points[idx - 1]
      dst = points[idx]
      points[idx][:distance] = \
        src[:distance] + \
        Haversine.distance(src[:latitude],
                           src[:longitude],
                           dst[:latitude],
                           dst[:longitude]).to_meters
    end
  end

  DistResultValid.new(points)
end
