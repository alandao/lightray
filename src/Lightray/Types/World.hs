{-# LANGUAGE TemplateHaskell #-}
module Lightray.Types.World where

import Lightray.Types.GeometricShape
import Control.Lens
import Data.Colour.SRGB (Colour(..))

data Object = Object    { _objectShape :: GeometricPrimitive
                        , _objectColour :: Colour Double}

instance Show Object where
    show x = "Object: " ++ (show (_objectShape x))
data World = World  { _worldBackgroundColor :: Colour Double
                    , _worldObjects :: [Object]
                    }

makeLenses ''Object
makeLenses ''World
