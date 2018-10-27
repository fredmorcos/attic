module Fig where

data FIGDoc = FIGDoc { version          :: Float
                     , orientation      :: FIGOrientation
                     , justification    :: FIGJustification
                     , units            :: FIGUnits
                     , paperSize        :: FIGPaperSize
                     , magnification    :: Float
                     , multiPage        :: FIGMultiPage
                     , transparentColor :: Int
                     , comment          :: String
                     , resolution       :: Int
                     , coordSystem      :: FIGCoordSystem
                     } deriving (Show)

data FIGOrientation   = Landscape | Portrait    deriving (Show)
data FIGJustification = Center    | FlushLeft   deriving (Show)
data FIGUnits         = Metric    | Inches      deriving (Show)
data FIGPaperSize     = Letter    | Legal     |
                        Ledger    | Tabloid   |
                        A         | B         |
                        C         | D         |
                        E         | A4        |
                        A3        | A2        |
                        A1        | A0        |
                        B5                      deriving (Show)
data FIGMultiPage     = Single    | Multiple    deriving (Show)
data FIGCoordSystem   = LowerLeft | UpperLeft   deriving (Show)

-- data FIGObject = Color | 

main :: IO ()
main = interact transFIG

readToken :: String -> String
readToken [] = 

transFIG :: String -> String
transFIG s = show $ parseFIG s

parseFIG :: String -> FIGDoc
parseFIG s = parseVersion s

-- parseFIG x = FIGDoc { version = 3.2
--                     , orientation = Landscape
--                     , justification = Center
--                     , units = Metric
--                     , paperSize = A4
--                     , magnification = 4.5
--                     , multiPage = Single
--                     , transparentColor = 0
--                     , comment = "Foobar"
--                     , resolution = 1200
--                     , coordSystem = LowerLeft
--                     }
