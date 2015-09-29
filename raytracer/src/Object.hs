{-# LANGUAGE TemplateHaskell #-}
module Object where
-- ^The 'Object' module deals with objects in the "world".
-- By world, we mean any objects that conribute to the image.

import Control.Lens
import Data.Colour
import Linear.V3

data Shape =    Plane   { _planePoint    :: V3 Double
                        , _planeNormal   :: V3 Double }
            |   Sphere  { _sphereCenter  :: V3 Double
                        , _sphereRadius :: Double }


data Object = Object    { _objectShape :: Shape
                        , _objectColour :: Colour Double}

makeLenses ''Shape
makeLenses ''Object
--we'll add linear transformations later
