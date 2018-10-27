-- print("Hello World")
-- -- print("args = " .. ...)
--
-- function factorial (n)
--    local a = 2
--    if n == 0 then return 1
--    else return n * factorial(n - 1)
--    end
-- end
--
-- -- print("a = " .. a)
--
-- print("Enter a number:")
-- number = io.read("*n")
-- print("Result:" .. factorial(number))
--
-- print("a = " .. a)
--
-- line = io.read()
-- n = tonumber(line)
--
-- if n == nil then
--    error(line .. " is not a valid number")
-- else
--    print("n * 2 = " .. n * 2)
-- end
--
a = {}
a.x = 5

print(a["x"])
k = "x"
print(a[k])
print(a.x)
print(a)
a.y = 6
print(#a)

for i = 1, 1000 do
   a[i] = i * 2
end

print(a[20])
print(#a)

print(2 ~= 3)

y = false or 4
print(y)
y = y or 5
print(y)
y = y or 6
print(y)

polyline =
   { color = "blue",
     thickness = 2,
     npoints = 4,
     [2] = { x = 0, y = 0 },
     [1] = { x = 0, y = 1 }
   }

print(polyline[1].x .. ", " .. polyline[1].y)

x, y = 0, 1
x, y = y, x                                  -- swap

print(x .. ", " .. y)

for k, v in pairs(polyline) do
   print(tostring(k) .. " = " .. tostring(v))
end
