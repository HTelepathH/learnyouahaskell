module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea 
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * ( r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * ( r ^ 2)

cubeVolume :: Float -> Float
cubeVolume s = cuboidVolume s s s 

cubeArea :: Float -> Float
cubeArea s = cuboidArea s s s 

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = a * b * c

cuboidVolume:: Float -> Float -> Float -> Float
cuboidVolume a b c = 2 * ( a * b + a * c + b * c)
