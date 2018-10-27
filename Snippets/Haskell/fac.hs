fac n = if n == 2 then 2 else n * fac (n - 1)
main = print (fac 100000)