import Linear.Metric

kEpsilon = 0.000001

hitDistances :: Ray -> Shape -> Maybe [Double]
-- a plane should only return one hit point.
hitDistances ray plane@(Plane _ _)
    | isInfinite t = Nothing
    | t < kEpsilon = Nothing
    | otherwise = Just [t]
    where t = (dot ((planePoint plane) - (rayOrigin ray)) (planeNormal plane)) /
              (dot (rayDirection ray) (planeNormal plane))
