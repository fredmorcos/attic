module Main where

areaTriangleTrig a b c = c * height / 2
                 where
                 cosa   = (b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c)
                 sina   = sqrt (1 - cosa ^ 2)
                 height = b * sina

areaTriangleHeron a b c = result
                 where
                 result = sqrt (s * (s - a) * (s - b) * (s - c))
                 s      = (a + b + c) / 2
