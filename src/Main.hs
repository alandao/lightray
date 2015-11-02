module Main where

import Lightray.Types.Camera
import Lightray.Types.World
import Lightray.Types.GeometricShape
import Lightray.Render
import Data.Colour.Names
import Codec.Picture (writeBitmap)
import System.Directory (getCurrentDirectory)

--temp imports, will write interface later.
import Linear.V3 (V3(..))

world = World { _worldBackgroundColor = blue, _worldObjects = objects}
objects = [ Object { _objectShape = Plane {_planePoint = V3 0 (-2) 0,
            _planeNormal = V3 0 1 0}, _objectColour = white },
            Object { _objectShape = Plane {_planePoint = V3 2 0 0, _planeNormal =
            V3 1 0 0}, _objectColour = green},
            Object { _objectShape = Plane {_planePoint = V3 (-2) 0 0, _planeNormal = V3 1 0 0}, _objectColour = red},
            Object { _objectShape = Plane {_planePoint = V3 0 0 (-4), _planeNormal =
            V3 0 0 1}, _objectColour = white},
            Object { _objectShape = Plane {_planePoint = V3 0 2 0, _planeNormal =
            V3 0 1 0}, _objectColour = white},
            Object { _objectShape = Sphere {_sphereCenter = V3 (0.5) (0.5) (-1),
            _sphereRadius = 1.0}, _objectColour = cyan},
            Object { _objectShape = Sphere {_sphereCenter = V3 (-0.2) (0) (-2),
            _sphereRadius = 0.8 }, _objectColour = darkkhaki}]

camera = PerspectiveCamera { _camViewPlane = viewplane
                    , _camPosition = V3 0 0 0
                    , _camLookPoint = V3 0 0 (-1)
                    , _camUpVector = V3 0 1 0
                    , _camViewPlaneDistance = 20
}

viewplane = ViewPlane { _viewPlaneWidth = 640
                            , _viewPlaneHeight = 360
                            , _viewPlaneSize = 0.2
                            , _viewPlaneGamma = 2.6
                            }
main :: IO ()
main = do
    dir <- getCurrentDirectory
    writeBitmap (dir ++ "\\test.bmp") (render world camera)
