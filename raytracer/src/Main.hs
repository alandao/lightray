module Main where

import World
import Linear.V3
import Object
import Data.Colour.Names as Colour


--test world
world = World { _worldViewPlane = viewplane
              , _worldBackgroundColor = Colour.black
              , _worldObjects = objects}

viewplane = ViewPlane { _viewPlaneWidth = 100
                      , _viewPlaneHeight = 100
                      , _viewPlaneSize = 1
                      , _viewPlaneGamma = 1}

objects = [ Object      { objectShape = Sphere { sphereCenter = V3 0 5 0
                                              , sphereRadius = 1}
                        , objectColour = Colour.cyan } ]


main :: IO ()
main = do
  putStrLn "hello world"
