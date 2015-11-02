{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Lightray.Intersection where

import Lightray.Types.GeometricShape
import Lightray.Types.Ray
import Control.Lens
import Linear.Metric ( dot, normalize )
import Linear.V3 (V3)
import Data.List (nub)


kEpsilon = 0.000001




hitDistances :: Ray -> GeometricPrimitive -> Maybe [Double]
-- a plane should only return one hit point.
hitDistances (rayView -> RayView {_rayViewOrigin = o, _rayViewDirection = l})
  (Plane {_planePoint = p, _planeNormal = n})
    | isInfinite t = Nothing
    | t < kEpsilon = Nothing
    | otherwise = Just [t]
    where
        t = (dot (p - o) n) / (dot l n)
hitDistances (rayView -> RayView {_rayViewOrigin = o, _rayViewDirection = l})
 (Sphere {_sphereCenter = c, _sphereRadius = r})
  | hits == Just [] = Nothing
  | otherwise = hits
 where x = dot l l
       y = dot (2*(o - c)) l
       z = (dot (o - c) (o - c)) - (r * r)
       hits = fmap (filter (>kEpsilon)) $ realQuadraticRoots x y z

--helper functions

realQuadraticRoots :: Double -> Double -> Double -> Maybe [Double]
realQuadraticRoots 0 _ _ = Nothing
realQuadraticRoots a b c
  | discriminant < 0 = Nothing
  | otherwise = Just (nub [formula (+), formula (-)])
  where
    formula sign = ((-b) `sign` (sqrt discriminant)) / (2*a)
    discriminant = (b*b) - (4*a*c)
