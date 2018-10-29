#!/bin/env lua

local function main (argv)
   local detach = false
   local safe   = false
   local keep   = false
   local dname  = nil
   local addr   = nil
   local port   = nil

   local skip = false

   if #argv == 0 then
      error("No arguments given, see -h")
   end

   for i = 1, #argv do
      local arg = argv[i]

      if skip == true then
         skip = false
      else
         if arg == "-h" then
            local usage = "Usage: backyd [-h -d -s -k -a ADDR] -p PORT -r DIR"

            io.stderr
               :write(usage)
               :write("\n")
               :write("gekki/1")

            os.exit(0)
         elseif arg == "-d" then
            detach = true
         elseif arg == "-s" then
            safe = true
         elseif arg == "-k" then
            keep = true
         elseif arg == "-r" then
            dname = argv[i + 1]
            skip = true
         elseif arg == "-a" then
            addr = argv[i + 1]
            skip = true
         elseif arg == "-p" then
            port = argv[i + 1]
            skip = true
         else
            error("Unrecognized argument: " + arg)
            os.exit(1)
         end
      end
   end

   if dname == nil then
      error("No root directory given, see -h")
      os.exit(1)
   end

   if port == nil then
      error("No port given, see -h")
      os.exit(1)
   end
end

main(arg)
