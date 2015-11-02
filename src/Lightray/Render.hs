{-# LANGUAGE TemplateHaskell #-}

module Lightray.Render where

import Lightray.Types.World
import Lightray.Types.Camera
import Lightray.Types.Ray
import Lightray.Intersection
import Control.Lens
import Codec.Picture (Image, Pixel8, PixelRGB8(..), generateImage)
import Data.Colour.SRGB ( channelRed, channelGreen, channelBlue, toSRGB24)
import Data.Foldable (minimumBy)
import Data.Colour.SRGB (Colour)
import Data.List (nub)
import Linear.V3 (V3(..), cross)
import Linear.Metric (normalize)
import Linear.Vector((*^))



render :: World -> Camera -> Image PixelRGB8
render world camera = generateImage (colorAt world camera) width height
    where
        width = _viewPlaneWidth $ viewplane
        height = _viewPlaneHeight $ viewplane
        viewplane = _camViewPlane camera

colorAt :: World -> Camera -> Int -> Int -> PixelRGB8
colorAt world camera x y = PixelRGB8 red green blue
    where
        red = channelRed rgbTuple
        green = channelGreen rgbTuple
        blue = channelBlue rgbTuple
        rgbTuple = toSRGB24 $ trace world ray
        ray = primaryRay row col camera --convert juicypixel to ONB coordinates
        row = (camera^.camViewPlane^.viewPlaneHeight - 1) - y
        col = x


primaryRay :: Int -> Int -> Camera -> Ray
primaryRay row col camera@(PerspectiveCamera {}) = ray camPos dir
    where
        dir = (xv *^ u) + (yv *^ v) - ((_camViewPlaneDistance camera) *^ w)
        xv = size * ((fromIntegral col) - (fromIntegral width)/2)
        yv = size * ((fromIntegral row) - (fromIntegral height)/2)
        w = normalize (camPos - camLookAt)
        u = normalize (cross (camera^.camUpVector) w)
        v = cross w u
        width = camera^.camViewPlane^.viewPlaneWidth
        height = camera^.camViewPlane^.viewPlaneHeight
        camPos = camera^.camPosition
        camLookAt = camera^.camLookPoint
        size = camera^.camViewPlane^.viewPlaneSize

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
