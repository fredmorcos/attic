import Data.Complex

main :: IO ()
main = do let res = dft (map (\x -> x :+ 0) [1..10])
       	  putStrLn $ show res

dft :: RealFloat a => [Complex a] -> [Complex a]
dft x = map (\(_, k) -> singleDFT k idxList) idxList
  where idxList = zip x [0..]

singleDFT :: RealFloat a => Int -> [(Complex a, Int)] -> Complex a
singleDFT k x = sum $ map (\(e, n) -> (e *) $ fCoeff k n $ length x) $ x

fCoeff :: RealFloat a => Int -> Int -> Int -> Complex a
fCoeff k n n_tot = exp (0.0 :+ ((-2.0) * pi * (fromIntegral $ k * n) / (fromIntegral n_tot)))
