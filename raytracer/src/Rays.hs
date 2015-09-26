module Rays (
  ray
)
where

import Linear.Metric
import Linear.V3
import Shapes

kEpsilon = 0.000001

--rayDirection will always be normalized to 1.
data Ray = Ray { rayOrigin :: V3 Double
               , rayDirection :: V3 Double }
               deriving (Show)
ray :: V3 Double -> V3 Double -> Ray
ray orig dir = Ray {rayOrigin = orig, rayDirection = (normalize dir)}

hitDistances :: Ray -> Shape -> Maybe [Double]
-- a plane should only return one hit point.
hitDistances (Ray {rayOrigin = o, rayDirection = l})
  (Plane {planePoint = p, planeNormal = n})
    | isInfinite t = Just [kEpsilon]
    | t < kEpsilon = Nothing
    | otherwise = Just [t]
    where t = (dot (p - o) n) /
              (dot l n)
--hitDistances ray sphere@(Sphere _ _)
hitDistances (Ray {rayOrigin = o, rayDirection = l})
 (Sphere {sphereCenter = c, sphereRadius = r}) = undefined
