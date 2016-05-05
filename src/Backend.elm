module Backend where

import Signal exposing (Mailbox, mailbox, Signal, sampleOn, constant,foldp)
import Signal.Extra exposing ((<~), (~), keepWhen, deltas)
import List exposing (map, foldl)

import Mouse


--import Window
--import Mouse

import Input exposing (..)
import Math.Numerical exposing (..)
import Math.Point3D exposing (..)
import Math.Functions exposing (..)

-- Utility functions


import Time exposing (fps)


-- clock
fps_clock = fps 60

--- Ports, to connect with JS ---
{-| in_init is used to signal when the ThreeJS finishes loading |-}



updater : Time -> (Point3D,Point3D)
updater dt = ({x = 0, y = 10, z = 20}, {x = 0, y = 30, z = 0})

updater2 : Signal Float
updater2 =
    let
        funny dt state = (dt/16.6 * 0.3) + state
    in foldp funny 0.0 fps_clock


camera1 : Signal Point3D 
camera1 = 
    let f theta = {x = 100 * sin(theta), y = 30 + 100 * sin(theta), z = 100 * cos(theta) }
    in  f <~ (degrees <~ updater2)


-- Manual camera
calc_offset : ((number,number),(number,number)) -> (number,number)
calc_offset ((ox,oy),(nx,ny)) = (nx-ox,ny-oy)


dropN : Int -> a -> Signal a -> Signal a
dropN n val sig =
    let f _ count = 
            if count <= 0 then (0)
            else (count-1)
        caller = sampleOn sig <| constant 0
        forwardCheck x = x == 0 
        is_ok = forwardCheck <~ foldp f n caller
    in keepWhen is_ok val sig

dropOnce = dropN 1


mouse_offies = 
    let floaty (x,y) = ((toFloat x) / 300.0, (toFloat y) / 300.0)
    in floaty <~ dropN 2 (0,0) (calc_offset <~ (deltas Mouse.position))

accumulate_offsets : (Float, Float) -> Signal (Float, Float) -> Signal (Float, Float) 
accumulate_offsets state sig = 
    let f (nx, ny) (ox,oy) = (nx + ox, ny + oy)
    in  foldp f state sig

camera_manual' : (Float,Float) -- mouse offsets 
               -> Float        -- radius
               -> Float        -- y component
               -> Point3D      -- result
camera_manual' (off_x,off_y) r pos_y =
    let orig = {x = 100, y=0, z=0}
        rotated = rotateY off_x <| rotateZ (off_y) orig
    in {rotated | y = rotated.y + pos_y} 
    --in rotated
    --in  orig
    --let nx = r * (cos off_x)*(cos off_y) 
    --    ny = pos_y + r * (cos off_x)*(sin off_y)
    --    nz = r * (sin off_x)
    --in  {x = nx, y = ny, z = nz}

camera_manual : (Float, Float) -> Signal Float -> Signal Float -> Signal Bool -> Signal Point3D
camera_manual start r pos_y work  = 
    let offies = accumulate_offsets start <| keepWhen work (0,0) mouse_offies 
    in  camera_manual' <~ offies ~ r ~ pos_y




chooseFunction : Signal (Point3D -> Point3D)
chooseFunction = 
    let chooseFun what_f lorenzParams = case what_f of
        Lorenz -> lorenzFunction lorenzParams 
        Rossler -> rosslerFunction lorenzParams
    in chooseFun <~ functionsChoice.signal ~ Signal.dropRepeats lorenzParams 

startingPoint : Signal Point3D
startingPoint = 
    let chooseFun xSig ySig zSig = {x=xSig, y= ySig, z=zSig}
    in chooseFun <~ xSignal ~ ySignal ~ zSignal

lorenzParams : Signal FunParams
lorenzParams = 
    let createLorenz el1 el2 el3 = {p1 = el1, p2 = el2, p3 = el3}
    in  createLorenz <~ p1 ~ p2 ~ p3

--input ports - TO BE DEFINED.
getFurthestPoint : List Point3D -> Point3D
getFurthestPoint array = 
    let dist pt1 = distance3D pt1 {x=0,y=0,z=0}
        getFurthest pt1 pt2 = if (dist pt1) < (dist pt2)
            then pt2 
            else pt1
    in foldl getFurthest {x=0,y=0,z=1} array 

scalePoints points =
    let max_d  = distance3D {x=0,y=0,z=0} <| getFurthestPoint points
        d      = max_d / 160
    in map (flip divScalar d) points



calculatePoints : Method -> (Point3D -> Point3D) -> Point3D -> Time -> Time -> Int -> Bool -> List Point3D
calculatePoints method f start_pt dt t0 max_i dummy_val = 
    --let points = trampoline <| calculateIterations3D (prepareFunction f 0.005) 0 max_i start_pt []
    --let points = useEuler f 0.005 start_pt max_i
    let points = case method of
            RK4 -> useRK4 f dt t0 start_pt max_i
            Euler -> useEuler f dt start_pt max_i
    in scalePoints points