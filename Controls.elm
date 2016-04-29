module Controls where

import Signal exposing (..)
import Signal.Extra exposing ((<~), (~), combine,sampleWhen, keepWhenI)
import List exposing (sum,map,append)
import List
import Array
import Array exposing (Array)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input.Field exposing (..)
import Text exposing (fromString)

import String exposing (toInt)
import String
import Trampoline exposing (..)

import MyElements exposing (..)
-- Utility functions



-- Model

type alias FunctionRec = (Point3D -> Point3D)
type alias Point3D = {
    x : Float,
    y : Float,
    z : Float
}

distance3D : Point3D -> Point3D -> Float
distance3D pt1 pt2 =
    let dx = (pt1.x - pt2.x)^2
        dy = (pt1.y - pt2.y)^2
        dz = (pt1.z - pt2.z)^2
    in sqrt (dx + dy + dz)

type alias Function3D  = (Point3D -> Point3D)

type alias LorenzParams ={
    p1 : Float,
    p2 : Float,
    p3 : Float,
    dt : Float
}

lorenzFunction : LorenzParams -> Point3D -> Point3D
lorenzFunction {p1,p2,p3, dt} {x,y,z} =
    let nx = x + dt*(p1*(y - x))
        ny = y + dt*(x*(p2 - z) - y)
        nz = z + dt*(x*y - p3*z)
    in {x=nx,y=ny,z=nz}



calculateIterations3D : Function3D -> Int -> Int -> Point3D -> List Point3D -> Trampoline (List Point3D) 
calculateIterations3D function i     max_i pt list = 
    case max_i == i of
        False ->
            let new_pt = function pt
                new_list = new_pt :: list
            in Continue (\() -> calculateIterations3D function (i+1) max_i new_pt new_list)
        True -> Done <| List.reverse list

--calculateIterations3D : Function3D -> Int -> Int -> Point3D -> List Point3D -> (List Point3D) 
--calculateIterations3D function i     max_i pt list = 
--    case max_i == i of
--        False ->
--            let new_pt = function pt
--                new_list = new_pt :: list
--            in calculateIterations3D function (i+1) max_i new_pt new_list
--        True -> List.reverse list



--input ports - TO BE DEFINED.
getFurthestPoint : List Point3D -> Point3D
getFurthestPoint array = 
    let dist pt1 = distance3D pt1 {x=0,y=0,z=0}
        getFurthest pt1 pt2 = if (dist pt1) < (dist pt2)
            then pt2 
            else pt1
    in List.foldl getFurthest {x=0,y=0,z=1} array 


calculatePoints : Function3D -> Point3D -> Int -> Bool -> List Point3D
calculatePoints f start_pt max_i dummy_val = 
    let points = trampoline <| calculateIterations3D f 0 max_i start_pt []
        max_d  = distance3D {x=0,y=0,z=0} <| getFurthestPoint points
        d      = max_d / 160
        divBy d {x,y,z} = {x = x/d, y = y/d, z = z/d}
    in List.map (divBy d) points




getEvery : Int -> List a -> List a
getEvery n list = trampoline (getEvery' n 0 [] list) 

getEvery' : Int -> Int -> List a -> List a -> Trampoline (List a)
getEvery' n ix acc list =
    case list of
        l::ls -> case n < ix of
            False ->
                let new_acc = l::acc
                in Continue (\() -> getEvery' n 0 new_acc ls)
            True  -> Continue (\() -> getEvery' n (ix+1) acc ls)
        []     ->  Done acc

--- Ports, to connect with JS ---
port in_init : Signal Bool

port out_points  : Signal (List Point3D)
port out_points = calculatePoints <~ chooseFunction ~ startingPoint ~ iterations ~ in_init

--port delta_points : Signal Int

chooseFunction : Signal Function3D
chooseFunction = 
    let chooseFun what_f lorenzParams = case what_f of
        Lorenz -> lorenzFunction lorenzParams 
    in chooseFun <~ functionsChoice.signal ~ Signal.dropRepeats lorenzParams 

startingPoint : Signal Point3D
startingPoint = 
    let chooseFun xSig ySig zSig = {x=xSig, y= ySig, z=zSig}
    in chooseFun <~ xSignal ~ ySignal ~ zSignal

lorenzParams : Signal LorenzParams
lorenzParams = 
    let createLorenz el1 el2 el3 = {p1 = el1, p2 = el2, p3 = el3, dt = 0.005}
    in  createLorenz <~ p1 ~ p2 ~ p3


--------------
-- ELEMENTS --
--------------

startPosElement = 
    let desc = leftAligned <| Text.fromString "Starting position"
    in  flow down <~ (append [desc] <~ combine [xField, yField, zField])

paramsElement = 
    let desc = leftAligned <| Text.fromString "Parameters"
    in  flow down <~ (append [desc]  <~ combine [p1Field,p2Field,p3Field])
    

iterationsElement = 
    let desc = leftAligned <| Text.fromString "Iterations"
    in  flow down <~ (append [desc] <~ combine [ixField]) 

functionElement = 
    let desc = leftAligned <| Text.fromString "Chosen function"
    in  flow down [desc, functionButtons]
     



main = 
    let first_col = flow down [functionElement]
        rest_cols = flow right <~ combine [iterationsElement, paramsElement, startPosElement]
    in flow right <~ ( append [first_col] <~ combine [rest_cols]) 
