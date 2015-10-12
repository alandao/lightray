{-# LANGUAGE TemplateHaskell #-}
module GeometricShape where

import Control.Lens
import Linear.V3 (V3)

data GeometricPrimitive =       Plane   { _planePoint    :: V3 Double
                                        , _planeNormal   :: V3 Double }
                        |       Sphere  { _sphereCenter  :: V3 Double
                                        , _sphereRadius :: Double }


makeLenses ''GeometricPrimitive
