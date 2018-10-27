-- Reads FIG and outputs Raphael JSON

module Main where
import Graphics.Fig

main :: IO ()
main = do
  input <- getContents
  either fail succeed $ parse "stdin" input
  where succeed = putStr . transformFig

transformFig :: Fig -> String
transformFig (Fig {fig_header  = h,
                   fig_colors  = _,
                   fig_objects = o}) =
  "{\n" ++ header ++ transformObjects o ++ "\n}\n"
  where header = if headerIn /= ""
                 then headerIn ++ ",\n"
                 else ""
                 where headerIn = transformHeader h

transformHeader :: Header -> String
transformHeader _ = ""

transformObjects :: [Commented Object] -> String
transformObjects [] = ""
transformObjects (x:[]) = transformObject x
transformObjects (x:xs) = transformObject x ++ ",\n" ++ transformObjects xs

transformObject :: Commented Object -> String
transformObject (Comment _ o) = transformObjectHelper o

transformObjectHelper :: Object -> String
transformObjectHelper
  (Text { text_sub_type   = subtype
        , text_color      = color
        , text_depth      = depth
        , text_pen_style  = penstyle
        , text_font       = font
        , text_font_size  = fontsize
        , text_angle      = angle
        , text_font_flags = fontflags
        , text_height     = height
        , text_length     = len
        , text_x          = x
        , text_y          = y
        , text_string     = text
        }) = "{\n" ++
             (keyval "font-size"   fontsize)             ++
             (keyval "text"        text)                 ++
             (keyval "text-anchor" (textAnchor subtype)) ++
             (keyval "stroke"      (show color))         ++
             (keyval "x"           x)                    ++
             (keyval "y"           y)                    ++
             (keyval "type"        "text")               ++
             "}"
transformObjectHelper (Arc arcLine frontArrow backArrow) =
  "{\n" ++
  transformArcLine arcLine  ++ "\n" ++
  transformArrow frontArrow ++
  transformArrow backArrow  ++
  "}"
transformObjectHelper
  (Spline splineLine frontArrow backArrow x y) = "spline"
transformObjectHelper
  (Ellipse { ellipse_common    = common
           , ellipse_direction = direction
           , ellipse_angle     = angle
           , ellipse_center_x  = centerX
           , ellipse_center_y  = centerY
           , ellipse_radius_x  = radiusX
           , ellipse_radius_y  = radiusY
           , ellipse_start_x   = startX
           , ellipse_start_y   = startY
           , ellipse_end_x     = endX
           , ellipse_end_y     = endY
           }) = "ellipse"
transformObjectHelper
  (Compound compoundLine objects) = "compound"
transformObjectHelper
  (Polyline polylineLine frontArrow backArrow pic x) = "polyline"

transformArrow :: (Maybe Arrow) -> String
transformArrow Nothing  = ""
transformArrow (Just a) = "arrow\n"

transformArcLine :: ArcLine -> String
transformArcLine
  (ArcLine { arc_common    = common
           , arc_cap_style = capStyle
           , arc_direction = direction
           , arc_center_x  = centerX
           , arc_center_y  = centerY
           , arc_x1        = x1
           , arc_y1        = y1
           , arc_x2        = x2
           , arc_y2        = y2
           , arc_x3        = x3
           , arc_y3        = y3
           }) = "arcline"

transformCommon :: Common -> String
transformCommon
  (Common { sub_type       = subType
          , line_style     = lineStyle
          , line_thickness = lineThickness
          , pen_color      = penColor
          , fill_color     = fillColor
          , depth          = depth
          , pen_style      = penStyle
          , area_fill      = areaFill
          , style_val      = styleVal
          }) = "common"

key :: String -> String
key s = (show s) ++ ": "

val :: (Show a) => a -> String
val s = (show s) ++ ", "

keyval :: (Show b) => String -> b -> String
keyval k v = (key k) ++ (val v) ++ "\n"

textAnchor :: Integer -> String
textAnchor 0 = "left"
textAnchor 1 = "center"
textAnchor 2 = "right"
textAnchor _ = error "Unknown value for Text Anchor (Text Subtype)"
