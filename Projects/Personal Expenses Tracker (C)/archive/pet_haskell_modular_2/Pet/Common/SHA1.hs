module Pet.Common.SHA1 (sha1sum) where

import Pet.Common.Errors
import Pet.Common.Process

sha1sum :: String -> IO CommandReturn
sha1sum str = do
  let cmd = "sha1sum"
  execRes <- executeCommand cmd [] str
  case execRes of
    Right err -> return $ Right err
    Left  out -> case parseSHA1 out of
      Nothing -> return $ Right $ cannotParse cmd
      Just cs -> return $ Left cs

parseSHA1 :: String -> Maybe String
parseSHA1 "" = Nothing
parseSHA1 s  = let cs = words s in
  if cs == [] then Nothing else Just $ cs !! 0
