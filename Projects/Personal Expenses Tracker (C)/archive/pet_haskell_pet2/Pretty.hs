module Pretty where

import StringUtils

class PrettyShow a where
  pShow :: a -> String
  pShowList :: [a] -> String

  -- Implementation
  pShowList = unlines' . map pShow
