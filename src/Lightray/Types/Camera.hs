{-# LANGUAGE TemplateHaskell #-}
module Lightray.Types.Camera where

import Linear.V3 (V3)
import Control.Lens

data ViewPlane = ViewPlane  { _viewPlaneWidth :: Int
                            , _viewPlaneHeight :: Int
                            , _viewPlaneSize :: Double
                            , _viewPlaneGamma :: Double
                            }
data Camera = OrthogonalCamera  { _camViewPlane :: ViewPlane
                                , _camPosition :: V3 Double
                                , _camLookPoint :: V3 Double
                                , _camUpVector :: V3 Double
                                }
            | PerspectiveCamera { _camViewPlane :: ViewPlane
                                , _camPosition :: V3 Double
                                , _camLookPoint :: V3 Double
                                , _camUpVector :: V3 Double
                                , _camViewPlaneDistance :: Double
            }

makeLenses ''ViewPlane
makeLenses ''Camera
