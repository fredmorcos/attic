#!/usr/bin/env ruby

require_relative 'bookmarks'

def die (msg, usage, quit)
  if msg != nil
    $stderr.puts msg
    $stderr.puts ""
  end

  print_usage if usage
  abort if quit
end

def print_usage
  $stderr.puts <<EOS
Usage: fumarks.rb COMMAND

Commands:
  add URL TITLE TAGS [SHORTCUT]

Notes:
  TAGS is a list of tag strings sparated by commas
  SHORTCUT is an optional argument

Examples:
  fumarks.rb add "google.com" "Google Homepage" "google,search" "ggl"
EOS
end

def main
  die "Error: No command given", true, true if ARGV.length == 0

  cmd            = ARGV[0]
  bookmarks_list = BookmarksList.new

  case cmd
  when 'add'
    url      = ARGV[1]
    title    = ARGV[2]
    tags     = ARGV[3]
    shortcut = ARGV[4]

    bookmarks_list.add_bookmark(Bookmark.create url, title, tags, shortcut)
  else
    die "Error: Unknown command `#{cmd}'", true, true
  end

  puts bookmarks_list
end

if __FILE__ == $0
  main
end
