factorial n = factorialWorker n 1 where
factorialWorker n res | n > 1     = factorialWorker (n - 1) (res * n)
                      | otherwise = res
