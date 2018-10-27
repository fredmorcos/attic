module GeometryTest where

import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

main = putStrLn $ "area of a cube 2 = " ++ (show $ Cube.area 2)
