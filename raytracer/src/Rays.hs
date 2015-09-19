module Rays where

import Linear.Metric
import Linear.V3
import Shapes

kEpsilon = 0.000001

data Ray = Ray { rayOrigin :: V3 Double
               , rayDirection :: V3 Double }

hitDistances :: Ray -> Shape -> Maybe [Double]
-- a plane should only return one hit point.
hitDistances ray plane@(Plane _ _)
    | isInfinite t = Nothing
    | t < kEpsilon = Nothing
    | otherwise = Just [t]
    where t = (dot ((planePoint plane) - (rayOrigin ray)) (planeNormal plane)) /
              (dot (rayDirection ray) (planeNormal plane))
