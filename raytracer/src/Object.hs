module Object where
-- ^The 'Object' module deals with objects in the "world".
-- By world, we mean any objects that conribute to the image.

import Linear.V3
import Data.Colour

data Shape =    Plane   { planePoint    :: V3 Double
                        , planeNormal   :: V3 Double }
            |   Sphere  { sphereCenter  :: V3 Double
                        , sphereRadius :: Double }


data Object = Object    { objectShape :: Shape
                        , objectColour :: Colour Double}

--we'll add linear transformations later
