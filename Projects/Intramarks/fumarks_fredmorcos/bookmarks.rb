require 'mongo'

include Mongo

class Bookmark
  attr_accessor :url
  attr_accessor :title
  attr_accessor :tags
  attr_accessor :shortcut

  def initialize (url, title, tags, shortcut)
    @url      = url
    @title    = title
    @shortcut = shortcut
    @tags     = nil

    if tags.respond_to? :each   # tags is an array
      @tags = tags
    else                        # tags is a string
      @tags = []
      tags.split(",").each do |x|
        @tags.push(x.strip) if x.strip != ""
      end
    end
  end

  def tags_contain? (str)
    @tags.select { |tag| tag.include? str }
  end

  def to_s
    return <<EOS
Bookmark:
  URL      -> #{@url}
  TITLE    -> #{@title}
  TAGS     -> #{@tags.join ","}
  SHORTCUT -> #{@shortcut ? @shortcut : "None"}
EOS
  end

  def serialize
    { "url"      => @url,
      "title"    => @title,
      "tags"     => @tags,
      "shortcut" => @shortcut
    }
  end

  def self.deserialize (hash)
    self.create hash["url"], hash["title"], hash["tags"], hash["shortcut"]
  end

  def self.create (url, title, tags, shortcut)
    die "Error: URL not given",   true, true if not url
    die "Error: TITLE not given", true, true if not title
    die "Error: TAGS not given",  true, true if not tags

    Bookmark.new url, title, tags, shortcut
  end
end

class BookmarksList
  attr_reader :collection

  def initialize
    begin
      @client     = MongoClient.new('localhost', 27017)
      @db         = @client['fumarks-db']
      @collection = @db['bookmarks']
    rescue ConnectionFailure => e
      die "Error: #{e}", true, true
    end
  end

  def has_bookmark_shortcut? (shortcut)
    if shortcut.nil? then return false end
    @collection.find({ "shortcut" => shortcut }).count > 0 ? true : false
  end

  def has_bookmark_url? (url)
    @collection.find({ "url" => url }).count > 0 ? true : false
  end

  def add_bookmark (bm)
    if (bm.respond_to? :serialize) &&
        (has_bookmark_url?(bm.url) == false) &&
        (has_bookmark_shortcut?(bm.shortcut) == false)
      @collection.insert(bm.serialize)
    end
  end

  def to_s
    res = ""
    @collection.find.each { |x| res += Bookmark.deserialize(x).to_s }
    res
  end
end
