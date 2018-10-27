#!/usr/bin/env ruby

require_relative 'bookmarks'
require_relative 'errors'

def main
  begin
    raise "No command given" if ARGV.length == 0
    cmd = ARGV[0]
    bookmarks_list = BookmarksList.new

    case cmd
    when 'add'
      url      = ARGV[1]
      title    = ARGV[2]
      tags     = ARGV[3]
      shortcut = ARGV[4]

      bm = Bookmark.create url, title, tags, shortcut
      bookmarks_list.add_bookmark bm
    else
      die "Error: Unknown command `#{cmd}'", true, true
    end

    puts bookmarks_list
  rescue RuntimeError => e
    die "Error: #{e}", true, true
  rescue DBConnectionError, StorageError => e
    die "Error: #{e}", false, true
  end
end

if __FILE__ == $0
  main
end
