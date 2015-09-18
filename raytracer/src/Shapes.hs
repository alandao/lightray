module Shapes (
  Shape
) where

import Linear.V3

data Shape =    Plane   { planePoint    :: V3 Double
                        , planeNormal   :: V3 Double }
            |   Sphere  { sphereCenter  :: V3 Double
                        , sphereRadius :: Double }
