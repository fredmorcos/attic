import           Control.Monad      (liftM)
import           Data.List          (isPrefixOf)
import           System.Environment (getArgs)

parseConsecArg :: [String] -> (Maybe Bool, [String])
parseConsecArg [] = (Just False, [])
parseConsecArg a@(x:xs)
  | "-" `isPrefixOf` x = if x == "-c" then (Just True, xs) else (Nothing, a)
  | otherwise = (Just False, a)

uniq :: Eq a => [a] -> [a] -> [a]
uniq acc [] = reverse acc
uniq acc (x:xs) = if x `elem` acc then uniq acc xs else uniq (x:acc) xs

uniqc :: Eq a => [a] -> [a] -> [a]
uniqc acc [] = reverse acc
uniqc acc [x] = uniqc (x:acc) []
uniqc acc (x:y:xs) = if x == y then uniqc acc lst else uniqc (x:acc) lst
  where lst = y:xs

interactFiles :: [FilePath] -> (String -> String) -> IO ()
interactFiles files f = mapM readFile files >>= putStr . f . unlines

main :: IO ()
main = do (consecUniq, args) <- liftM parseConsecArg getArgs
          case consecUniq of
            Nothing -> fail "Unrecognized option, only -c allowed."
            Just c -> (if null args then interact else interactFiles args) $
                      unlines . (if c then uniqc [] . lines else uniq [] . lines)
          return ()
