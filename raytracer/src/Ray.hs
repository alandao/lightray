{-# LANGUAGE TemplateHaskell #-}
module Ray where
-- ^The 'Ray' module deals with rays that collide with objects.

import Control.Lens
import Linear.Metric
import Linear.V3
import Data.List( nub )
import Object

kEpsilon = 0.000001

--_rayDirection will always be normalized to 1.
data Ray = Ray { _rayOrigin :: V3 Double
               , _rayDirection :: V3 Double }
               deriving (Show)

makeLenses ''Ray

ray :: V3 Double -> V3 Double -> Ray
ray orig dir = Ray {_rayOrigin = orig, _rayDirection = (normalize dir)}

hitDistances :: Ray -> Shape -> Maybe [Double]
-- a plane should only return one hit point.
hitDistances (Ray {_rayOrigin = o, _rayDirection = l})
  (Plane {_planePoint = p, _planeNormal = n})
    | isInfinite t = Just [kEpsilon]
    | t < kEpsilon = Nothing
    | otherwise = Just [t]
    where t = (dot (p - o) n) /
              (dot l n)
hitDistances (Ray {_rayOrigin = o, _rayDirection = l})
 (Sphere {_sphereCenter = c, _sphereRadius = r})
  | hits == Just [] = Nothing
  | otherwise = hits
 where x = dot l l
       y = 2 * (dot l (o - c))
       z = (dot (o - c) (o - c)) - (r * r)
       hits = fmap (filter (>kEpsilon)) $ realQuadraticRoots x y z


realQuadraticRoots :: Double -> Double -> Double -> Maybe [Double]
realQuadraticRoots 0 _ _ = Nothing
realQuadraticRoots a b c
  | discriminant < 0 = Nothing
  | otherwise = Just (nub [formula (+), formula (-)])
  where
    formula sign = ((-b) `sign` (sqrt discriminant)) / (2*a)
    discriminant = (b*b) - (4*a*c)
