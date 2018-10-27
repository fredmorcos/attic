require_relative '../bookmarks'
require 'test/unit'

class TestBookmarksList < Test::Unit::TestCase
  def test_simple
    puts "Creating dummy bookmarks..."
    bm1 = Bookmark.create "google.com", "Google Homepage", "search", "ggl"
    bm2 = Bookmark.create "google.com", "Google Homepage", "google,search", nil
    bm3 = Bookmark.create "twitter.com", "Twitter", "social,twitter", "twt"
    bm4 = Bookmark.create "nottwitter.com", "NotTwit", "twitter,not", "twt"

    puts "Creating bookmark list..."
    bm_list = BookmarksList.new

    puts "Clearing bookmarks table..."
    bm_list.collection.remove

    puts "Adding dummy bookmarks to bookmark list..."
    assert_equal 0, bm_list.collection.find.count

    bm_list.add_bookmark bm1
    assert_equal 1, bm_list.collection.find.count

    begin
      bm_list.add_bookmark bm2
    rescue StorageError
      assert_equal 1, bm_list.collection.find.count
    end

    bm_list.add_bookmark bm3
    assert_equal 2, bm_list.collection.find.count

    begin
      bm_list.add_bookmark bm4
    rescue StorageError
      assert_equal 2, bm_list.collection.find.count
    end

    puts "Done"
  end
end
