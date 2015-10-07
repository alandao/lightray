{-# LANGUAGE TemplateHaskell #-}

module Camera where

import World
import Ray
import Control.Lens
import Codec.Picture (Image, PixelRGB8)
import Linear.V3 (V3)

data ViewPlane = ViewPlane  { _viewPlaneWidth :: Int
                            , _viewPlaneHeight :: Int
                            , _viewPlaneSize :: Double
                            , _viewPlaneGamma :: Double
                            }
data Camera = OrthogonalCamera  { _camViewPlane :: ViewPlane
                                , _camPosition :: V3 Double
                                --, _camAngle :: V3 Double
                                }
            | PerspectiveCamera

makeLenses ''ViewPlane
makeLenses ''Camera

render :: World -> Camera -> Image PixelRGB8
render world camera = undefined

imgXYtoRayOrig :: Int -> Int -> Camera -> V3 Double
imgXYtoRayOrig x y camera = case camera of
                                OrthogonalCamera {} -> rayOrigOrthogonal
    where
        --will revise this later to support rotating.
        rayOrigOrthogonal = anchor + (V3 (x*s) (y*s) 0)
        --anchor is at topleft of image.
        anchor = camPos + (V3 (-width*s/2) (height*s/2) 0)
        camPos = _camPosition camera
        s = _viewPlaneSize viewplane
        width = _viewPlaneWidth viewplane
        height = _viewPlaneHeight viewplane
        viewplane = _camViewPlane camera
