{-# LANGUAGE TemplateHaskell #-}
module World where
-- ^The 'World' module deals with everything required
-- to render one image. We construct a world, modify
-- it by adding new objects or lights to it,
-- and render it to a frame

--import Data.Colour
import Data.Colour.SRGB.Linear
import Data.Colour.SRGB(toSRGB24)
import Data.Foldable(minimumBy)
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
render world camera@(OrthogonalCamera {}) = undefined

--If there are no collisions, display background color.
--otherwise, display the color from closest shape
colorFrom :: World -> Ray -> PixelRGB8
colorFrom world ray
    | all (\x -> x == Nothing) collisions = colorToPixel $ _worldBackgroundColor
                                          world
    | otherwise = colorToPixel $ _objectColour $ fst $
                minimumBy dist (zip objects collisions)
  where
    colorToPixel c = PixelRGB8 r g b
      where r = channelRed channels
            g = channelGreen channels
            b = channelBlue channels
            channels = toSRGB24 c
--    dist :: (Object, Maybe [Double]) -> (Object, Maybe [Double]) -> Ordering
    dist x y = compare (fmap minimum (snd x)) (fmap minimum (snd y))
    collisions = fmap (hitDistances ray) (fmap _objectShape objects)
    objects = _worldObjects world

imgXYtoRayOrig :: Int -> Int -> Camera -> V3 Double 
