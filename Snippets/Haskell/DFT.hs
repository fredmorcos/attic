import Data.Complex

main :: IO ()
main = do let res = dft $ enumerate $ map (\x -> x :+ 0) [1..100]
       	  putStrLn $ show res

enumerate :: Integral b => [a] -> [(a, b)]
enumerate l = zip l [0..]

dft :: (RealFloat a, Integral b) => [(Complex a, b)] -> [Complex a]
dft x = map (\(_, k) -> singleDFT k x) x

singleDFT :: (RealFloat a, Integral b) => b -> [(Complex a, b)] -> Complex a
singleDFT k x = sum $ map (\(e, n) -> (e *) $ fCoeff k n $ length x) $ x

fCoeff :: (RealFloat a, Integral b) => b -> b -> b -> Complex a
fCoeff k n n_tot = exp (0.0 :+ ((-2.0) * pi * (fromIntegral $ k * n) /
       	   	       	       	       	      (fromIntegral n_tot)))
