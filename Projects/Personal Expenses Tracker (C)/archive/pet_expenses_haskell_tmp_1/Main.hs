import qualified Data.Map as M

main :: IO ()
main = interact parseExpensesFile

parseExpensesFile :: String -> String
parseExpensesFile f = show $ calcRatePerDay $ sanityCheck $ map words $ lines f

calcRatePerDay :: [(String, Double, String)] -> Double
calcRatePerDay xl = (sum $ map (\(_, v) -> v) dayRateList) / (fromIntegral $ length dayRateList)
  where dayRateMap = foldl (\a (d, v, _) -> M.insertWith (+) d v a) M.empty xl
        dayRateList = M.toList dayRateMap

{--
 -- 1. check if element is a list |x| == 0 or 3
 -- 2. convert 1st element to string, 2nd to double, 3rd to string
 --}
sanityCheck :: [[String]] -> [(String, Double, String)]
sanityCheck [] = []
sanityCheck xl = map parseElem $ filter (\x -> length x > 0) xl
  where parseElem e = if length e == 3 then
                        (head e,
                         read (head $ drop 1 e) :: Double,
                         head $ drop 2 e)
                      else
                        error "Bad element."
