# DONE: It's probably safer to use an interpolated JSON-formatted
# string instead to avoid having a JSON generation error while
# handling a JSON generation error (eg, JSON.generate(description:
# msg))
def err(msg)
  "{\"description\":\"#{msg}\"}"
end

def print_exc(exc)
  puts "Exception: #{exc}"
  puts "  - Message: #{exc.inspect}"
  puts "  - Backtrace: #{exc.backtrace}"
end
