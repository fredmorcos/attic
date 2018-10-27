import           Control.Monad      (liftM)
import           Data.List          (isPrefixOf)
import           System.Environment (getArgs)

newtype CmdLineParserT a = CmdLineParser ([String] -> (a, [String]))

runCmdLineParser :: CmdLineParserT a -> [String] -> (a, [String])
runCmdLineParser (CmdLineParser p) = p

instance Monad CmdLineParserT where
  return v = CmdLineParser (\xs -> (v, xs))
  p >>= f = CmdLineParser (\xs -> let (v, xs') = runCmdLineParser p xs
                                  in runCmdLineParser (f v) xs')

data ParameterT = TrailingNewline | Help | Version | Unrecognized String
                deriving Show

data ConfigT = Config { trailingNewline  :: Bool
                      , backslashEscapes :: Bool
                      , displayHelp      :: Bool
                      , displayVersion   :: Bool
                      }

defaultConfig :: ConfigT
defaultConfig = Config { trailingNewline  = True
                       , backslashEscapes = False
                       , displayHelp = False
                       , displayVersion = False
                       }

cmdLinePeek :: CmdLineParserT (Maybe String)
cmdLinePeek = CmdLineParser peek
  where peek [] = (Nothing, [])
        peek l@(x:_) = Just (x, l)

cmdLineItem :: CmdLineParserT String
cmdLineItem = CmdLineParser (\(x:xs) -> (x, xs))

cmdLineParser :: CmdLineParserT (Maybe ParameterT)
cmdLineParser = do
  a <- cmdLinePeek
  case a of
    "-n"        -> ret TrailingNewline
    "-h"        -> ret Help
    "--help"    -> ret Help
    "-V"        -> ret Version
    "--version" -> ret Version
    _           -> return (if "-" `isPrefixOf` a
                           then Just $ Unrecognized a
                           else Nothing)
  where ret v = cmdLineItem >> return (Just v)

main :: IO ()
main = do args <- getArgs
          print $ runCmdLineParser cmdLineParser args
-- getArgs >>= putStrLn . unwords
