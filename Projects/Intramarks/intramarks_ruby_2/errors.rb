class DBConnectionError < Exception; end
class StorageError < Exception; end

def die (msg, usage, quit)
  $stderr.puts msg if msg.nil? == false

  if usage
    $stderr.puts ""
    print_usage
  end

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
