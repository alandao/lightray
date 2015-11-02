{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Lightray.Types.Ray (Ray, RayView(..), ray, rayView) where
--we dont export the type constructor since we want the
--user to always use the "ray" func to create rays.
import Linear.V3 (V3)
import Linear.Metric (normalize)
import Control.Lens

--_rayDirection will always be normalized to 1.
data Ray = Ray { _rayOrigin :: V3 Double
               , _rayDirection :: V3 Double }
               deriving (Show)
data RayView = RayView { _rayViewOrigin :: V3 Double
                       , _rayViewDirection :: V3 Double }

makeLenses ''Ray
makeLenses ''RayView

rayView :: Ray -> RayView
rayView (Ray {_rayOrigin = o, _rayDirection = l}) = RayView {_rayViewOrigin = o,
                                                    _rayViewDirection = l}

ray :: V3 Double -> V3 Double -> Ray
ray orig dir = Ray {_rayOrigin = orig, _rayDirection = (normalize dir)}
