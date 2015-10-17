{-# LANGUAGE TemplateHaskell #-}
module Lightray.Ray where
-- ^The 'Ray' module deals with rays that collide with objects.

import Lightray.GeometricShape
import Lightray.World
import Control.Lens
import Linear.Metric ( dot, normalize )
import Linear.V3 (V3)
import Data.Colour.SRGB (Colour)
import Data.Foldable (minimumBy)
import Data.List (nub)

import Debug.Trace


kEpsilon = 0.000001

--_rayDirection will always be normalized to 1.
data Ray = Ray { _rayOrigin :: V3 Double
               , _rayDirection :: V3 Double }
               deriving (Show)

makeLenses ''Ray

ray :: V3 Double -> V3 Double -> Ray
ray orig dir = Ray {_rayOrigin = orig, _rayDirection = (normalize dir)}

--If there are no collisions, display background color.
--otherwise, return the color from closest shape
trace :: World -> Ray -> Colour Double
trace world ray
    | all (\x -> x == Nothing) collisions = world^.worldBackgroundColor
    | otherwise = _objectColour $ minimumBy (compareObject ray) (world^.worldObjects)
    where
        collisions = fmap (hitDistances ray) $ fmap _objectShape (world^.worldObjects)

compareObject r a b = distCompare (fmap minimum hitDistA)
            (fmap minimum hitDistB)
    where
        -- in the case of distCompare, Nothing is ALWAYS greater than Just anything
        distCompare Nothing (Just _) = GT
        distCompare (Just _) Nothing = LT
        distCompare x y = compare x y
        hitDistA = hitDistances r (a^.objectShape)
        hitDistB = hitDistances r (b^.objectShape)



hitDistances :: Ray -> GeometricPrimitive -> Maybe [Double]
-- a plane should only return one hit point.
hitDistances (Ray {_rayOrigin = o, _rayDirection = l})
  (Plane {_planePoint = p, _planeNormal = n})
    | isInfinite t = Nothing
    | t < kEpsilon = Nothing
    | otherwise = Just [t]
    where t = (dot (p - o) n) /
              (dot l n)
hitDistances (Ray {_rayOrigin = o, _rayDirection = l})
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
