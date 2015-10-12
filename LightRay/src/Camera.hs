{-# LANGUAGE TemplateHaskell #-}

module Camera where

import World
import Ray
import Control.Lens
import Codec.Picture (Image, Pixel8, PixelRGB8(..), generateImage)
import Data.Colour.SRGB ( channelRed, channelGreen, channelBlue, toSRGB24)
import Linear.V3 (V3(..))

data ViewPlane = ViewPlane  { _viewPlaneWidth :: Int
                            , _viewPlaneHeight :: Int
                            , _viewPlaneSize :: Double
                            , _viewPlaneGamma :: Double
                            }
data Camera = OrthogonalCamera  { _camViewPlane :: ViewPlane
                                , _camPosition :: V3 Double
                                , _camEulerAngle :: V3 Double
                                }
            | PerspectiveCamera

makeLenses ''ViewPlane
makeLenses ''Camera

render :: World -> Camera -> Image PixelRGB8
render world camera = generateImage (colorAt world camera) width height
    where
        width = _viewPlaneWidth $ viewplane
        height = _viewPlaneHeight $ viewplane
        viewplane = _camViewPlane camera

colorAt :: World -> Camera -> Int -> Int -> PixelRGB8
colorAt world camera x y = PixelRGB8 red green blue
    where
        red = channelRed rgbTuple
        green = channelGreen rgbTuple
        blue = channelBlue rgbTuple
        rgbTuple = toSRGB24 $ trace world ray
        --atm ray goes straight into z dimension
        ray = Ray.ray (eyeRayOrig x y camera) (V3 0 0 1)


eyeRayOrig :: Int -> Int -> Camera -> V3 Double
eyeRayOrig x y camera = case camera of
                                OrthogonalCamera {} -> anchor + delta
    where
        --will revise this later to support rotating.
        --anchor is at topleft of image. atm viewplane is at 0
        delta = V3 (s * (fromIntegral x)) (s * (fromIntegral (-y))) 0
        anchor = camPos + (V3 ((fromIntegral (-width))*s/2.0)
                ((fromIntegral height)*s/2.0) 0)
        camPos = _camPosition camera
        s = _viewPlaneSize viewplane
        width = _viewPlaneWidth viewplane
        height = _viewPlaneHeight viewplane
        viewplane = _camViewPlane camera
