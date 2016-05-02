module Numerical where

import Trampoline exposing (Trampoline (Done, Continue), trampoline)

import Point3D exposing (Point3D, multScalar, addPoint, divScalar)
type alias Time = Float

euler : (Point3D -> Point3D) --The function
     -> Point3D -- starting position
     -> Point3D -- result
euler f p0 = (f p0) `addPoint` p0  


rk4 : (Time -> Point3D -> Point3D) --The function
   -> Time --Delta time
   -> Time -- starting time
   -> Point3D -- starting position
   -> Point3D
rk4 f dt t0 p0 = 
    let
        -- functions
        add = addPoint
        mult = multScalar
        div = divScalar
        -- stuff
        k1 = (f t0 p0) `mult` dt
        k2 = (f (t0 + dt/2) (p0 `add` (k1 `div` 2))) `mult` dt
        k3 = (f (t0 + dt/2) (p0 `add` (k2 `div` 2))) `mult` dt
        k4 = (f (t0 + dt  ) (p0 `add` k3))           `mult` dt
    in
        p0 `add` ((k1 `add` (k2 `mult` 2) `add` (k3 `mult` 2) `add` k4) `div` 6)


generate : (a -> a) -> a -> Int -> List a
generate f v i = trampoline <|generate' f v i []

generate' : (a -> a) -> a -> Int -> List a -> Trampoline (List a)
generate' f v i vs = if i <= 0 then
        Done vs
    else
        let val = f v
        in  Continue (\() ->generate' f val (i-1) (val :: vs))

{-| Prepares 
    |-}
prepareFunction : (Point3D -> Point3D) -> Time -> Point3D -> Point3D
prepareFunction f dt p0 = f p0 `multScalar` dt

useEuler : (Point3D -> Point3D) -> Time -> Point3D -> Int -> List Point3D
useEuler f dt p0 max_i = 
    let numFun = euler (prepareFunction f dt)
    in generate numFun p0 max_i

--useRK4 : (Point3D -> Point3D) -> Time -> Time -> Point3D -> Int -> List Point3D
--useRK4 f dt t0 p0 max_i =
--    let numFun = rk (prepareFunction f) dt
--    in 