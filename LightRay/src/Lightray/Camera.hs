{-# LANGUAGE TemplateHaskell #-}

module Lightray.Camera where

import Lightray.World
import Lightray.Ray
import Control.Lens
import Codec.Picture (Image, Pixel8, PixelRGB8(..), generateImage)
import Data.Colour.SRGB ( channelRed, channelGreen, channelBlue, toSRGB24)
import Linear.V3 (V3(..), cross)
import Linear.Metric (normalize)
import Linear.Vector((*^))

data ViewPlane = ViewPlane  { _viewPlaneWidth :: Int
                            , _viewPlaneHeight :: Int
                            , _viewPlaneSize :: Double
                            , _viewPlaneGamma :: Double
                            }
data Camera = OrthogonalCamera  { _camViewPlane :: ViewPlane
                                , _camPosition :: V3 Double
                                , _camLookPoint :: V3 Double
                                , _camUpVector :: V3 Double
                                }
            | PerspectiveCamera { _camViewPlane :: ViewPlane
                                , _camPosition :: V3 Double
                                , _camLookPoint :: V3 Double
                                , _camUpVector :: V3 Double
                                , _camViewPlaneDistance :: Double
            }

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
        ray = primaryRay row col camera --convert juicypixel to ONB coordinates
        row = (camera^.camViewPlane^.viewPlaneHeight - 1) - y
        col = x


primaryRay :: Int -> Int -> Camera -> Ray
primaryRay row col camera@(PerspectiveCamera {}) = ray camPos dir
    where
        dir = (xv *^ u) + (yv *^ v) - ((_camViewPlaneDistance camera) *^ w)
        xv = size * ((fromIntegral col) - (fromIntegral width)/2)
        yv = size * ((fromIntegral row) - (fromIntegral height)/2)
        w = normalize (camPos - camLookAt)
        u = normalize (cross (camera^.camUpVector) w)
        v = cross w u
        width = camera^.camViewPlane^.viewPlaneWidth
        height = camera^.camViewPlane^.viewPlaneHeight
        camPos = camera^.camPosition
        camLookAt = camera^.camLookPoint
        size = camera^.camViewPlane^.viewPlaneSize
