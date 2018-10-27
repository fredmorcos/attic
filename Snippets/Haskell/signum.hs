-- mySigNum :: Num a => a -> a
mySigNum x =
         if x < 0 then
            -1
         else if x > 0 then
            1
         else
            0

mySigNum2 0 = 0
mySigNum2 x =
          if x < 0 then
             -1
          else if x > 0 then
             1
          else
             0

func x =
     case x of
          0 -> 1
          1 -> 5
          2 -> 2
          _ -> -1
