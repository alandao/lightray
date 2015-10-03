{-# LANGUAGE TemplateHaskell #-}
module World where
-- ^The 'World' module deals with everything required
-- to render one image. We construct a world, modify
-- it by adding new objects or lights to it,
-- and render it to a frame

import Data.Colour
import Control.Lens
import Object
import Ray
import Codec.Picture
import Linear.V3



data World = World  { _worldBackgroundColor :: Colour Double
                    , _worldObjects :: [Object]
                    }

data ViewPlane = ViewPlane  { _viewPlaneWidth :: Int
                            , _viewPlaneHeight :: Int
                            , _viewPlaneSize :: Double
                            , _viewPlaneGamma :: Double
                            }
data Camera = OrthogonalCamera  { _camViewPlane :: ViewPlane
                                , _camPosition :: V3 Double
                                , _camAngle :: V3 Double
                                }
            | PerspectiveCamera

makeLenses ''World
makeLenses ''ViewPlane
makeLenses ''Camera

render :: World -> Camera -> Image PixelRGB8
render world camera@(OrthogonalCamera {}) = generateImage

traceColor :: World -> Ray -> PixelRGB8
traceColor world ray = _objectColour (minimumBy dist (zip objects collisions))
  where
    colorToPixel c = (toSRGB24 c) PixelRGB8
      where rgbOfColour = zipWith ()
    dist :: (Object, Maybe [Double]) -> (Object, Maybe [Double]) -> Ordering
    dist x y = compare (fmap minimum (snd x)) (fmap minimum (snd y))
    collisions = fmap (hitDistances ray) (fmap _objectShape objects)
    objects = _worldObjects world

--helper functions
dist :: (Object)
