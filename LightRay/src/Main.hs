module Main where

import Lightray.Camera
import Lightray.World
import Lightray.GeometricShape
import Data.Colour.Names
import Codec.Picture (writeBitmap)
import System.Directory (getCurrentDirectory)

--temp imports
import Linear.V3 (V3(..))

world = World { _worldBackgroundColor = blue, _worldObjects = objects}
objects = [ Object { _objectShape = Sphere {_sphereCenter = V3 0 0 10001,
            _sphereRadius = 10000}, _objectColour = pink },
            Object { _objectShape = Sphere {_sphereCenter = V3 (-10010) 0 0, _sphereRadius = 10000}, _objectColour = green},
            Object { _objectShape = Sphere {_sphereCenter = V3 0 10010 0, _sphereRadius = 10000}, _objectColour = red}]

camera = PerspectiveCamera { _camViewPlane = viewplane
                    , _camPosition = V3 0 0 0
                    , _camLookPoint = V3 0 0 1
                    , _camUpVector = V3 0 1 0
                    , _camViewPlaneDistance = 2.0
}

viewplane = ViewPlane { _viewPlaneWidth = 300
                            , _viewPlaneHeight = 300
                            , _viewPlaneSize = 0.2
                            , _viewPlaneGamma = 2.6
                            }
main :: IO ()
main = do
    dir <- getCurrentDirectory
    writeBitmap (dir ++ "\\test.bmp") (render world camera)
