module Main where

import Camera
import World
import GeometricShape
import Data.Colour.Names
import Codec.Picture (writeBitmap)
import System.Directory (getCurrentDirectory)

--temp imports
import Linear.V3 (V3(..))

world = World { _worldBackgroundColor = azure, _worldObjects = objects}
objects = [Object { _objectShape = Sphere {_sphereCenter = V3 0 0 4, _sphereRadius = 2},
                    _objectColour = red}]

camera = OrthogonalCamera  { _camViewPlane = viewplane
                                , _camPosition = V3 0 0 0
                                , _camEulerAngle = V3 0 0 0
                                }

viewplane = ViewPlane { _viewPlaneWidth = 100
                            , _viewPlaneHeight = 100
                            , _viewPlaneSize = 0.5
                            , _viewPlaneGamma = 2.6
                            }
main :: IO ()
main = do
    dir <- getCurrentDirectory
    writeBitmap (dir ++ "\\test.bmp") (render world camera)
