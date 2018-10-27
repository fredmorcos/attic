import Control.Parallel

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = a `par` print (a) where a = fib 20