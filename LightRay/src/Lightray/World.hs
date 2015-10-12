{-# LANGUAGE TemplateHaskell #-}
module Lightray.World where
-- ^The 'World' module deals with the state of the world. This
-- module is responsible for creating and updating world types.

import Lightray.GeometricShape
import Control.Lens
import Data.Colour.SRGB (Colour)


data Object = Object    { _objectShape :: GeometricPrimitive
                        , _objectColour :: Colour Double}

data World = World  { _worldBackgroundColor :: Colour Double
                    , _worldObjects :: [Object]
                    }

makeLenses ''Object
makeLenses ''World
