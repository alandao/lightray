{-# LANGUAGE TemplateHaskell #-}
module World where
-- ^The 'World' module deals with everything required
-- to render one image. We construct a world, modify
-- it by adding new objects or lights to it,
-- and render it to an image or screen.

import Data.Colour
import Control.Lens
import Object
import Ray


data World = World  { _worldViewPlane :: ViewPlane
                    , _worldBackgroundColor :: Colour Double
                    , _worldObjects :: [Object]
                    }

data ViewPlane = ViewPlane  { _viewPlaneWidth :: Int
                            , _viewPlaneHeight :: Int
                            , _viewPlaneSize :: Double
                            , _viewPlaneGamma :: Double}

makeLenses ''World
makeLenses ''ViewPlane

--render :: World -> Image
