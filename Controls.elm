module Controls where

import Signal exposing (..)
import Signal.Extra exposing ((<~), (~), combine, keepThen)
import List exposing (intersperse, sum,map,append)
import List
import Array
import Array exposing (Array)
import Graphics.Element exposing (..)
--import Graphics.Collage exposing (..)
import Graphics.Input.Field exposing (..)
import Text exposing (fromString)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import String exposing (toInt)
import String
import Trampoline exposing (..)

import MyElements exposing (..)
import Numerical exposing (..)
import Point3D exposing (Point3D, divScalar)
-- Utility functions



-- Model

type alias FunctionRec = (Point3D -> Point3D)


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
    p3 : Float
}


rosslerFunction : LorenzParams -> Point3D -> Point3D
rosslerFunction {p1,p2,p3} {x,y,z} =
    let nx = -(y + z)
        ny = x + (p1*y)
        nz = p2 + x*z - (p3 * z)
    in {x=nx,y=ny,z=nz}

lorenzFunction : LorenzParams -> Point3D -> Point3D
lorenzFunction {p1,p2,p3} {x,y,z} =    
    let nx = p1*(y - x)
        ny = x*(p2 - z) - y
        nz = x*y - p3*z
    in {x=nx,y=ny,z=nz}

--input ports - TO BE DEFINED.
getFurthestPoint : List Point3D -> Point3D
getFurthestPoint array = 
    let dist pt1 = distance3D pt1 {x=0,y=0,z=0}
        getFurthest pt1 pt2 = if (dist pt1) < (dist pt2)
            then pt2 
            else pt1
    in List.foldl getFurthest {x=0,y=0,z=1} array 

scalePoints points =
    let max_d  = distance3D {x=0,y=0,z=0} <| getFurthestPoint points
        d      = max_d / 160
    in List.map (flip divScalar d) points



calculatePoints : Method -> Function3D -> Point3D -> Time -> Time -> Int -> Bool -> List Point3D
calculatePoints method f start_pt dt t0 max_i dummy_val = 
    --let points = trampoline <| calculateIterations3D (prepareFunction f 0.005) 0 max_i start_pt []
    --let points = useEuler f 0.005 start_pt max_i
    let points = case method of
            RK4 -> useRK4 f dt t0 start_pt max_i
            Euler -> useEuler f dt start_pt max_i
    in scalePoints points




--- Ports, to connect with JS ---
port in_init : Signal Bool

port out_points  : Signal (List Point3D)
port out_points = calculatePoints <~ methodChoice.signal ~ chooseFunction ~ startingPoint ~ delta_time ~ start_time ~ iterations ~ in_init

--port delta_points : Signal Int

chooseFunction : Signal Function3D
chooseFunction = 
    let chooseFun what_f lorenzParams = case what_f of
        Lorenz -> lorenzFunction lorenzParams 
        Rossler -> rosslerFunction lorenzParams
    in chooseFun <~ functionsChoice.signal ~ Signal.dropRepeats lorenzParams 

startingPoint : Signal Point3D
startingPoint = 
    let chooseFun xSig ySig zSig = {x=xSig, y= ySig, z=zSig}
    in chooseFun <~ xSignal ~ ySignal ~ zSignal

lorenzParams : Signal LorenzParams
lorenzParams = 
    let createLorenz el1 el2 el3 = {p1 = el1, p2 = el2, p3 = el3}
    in  createLorenz <~ p1 ~ p2 ~ p3


--------------
-- ELEMENTS --
--------------
br = constant <| Html.br [] []

div_style = style [("box-sizing", "border-box"),("width","25%"),("float","left") ]


startPosElement = 
    let desc = constant <| span [] [text "Starting position"]
        x_desc = constant <| text "x:"
        y_desc = constant <| text "y:"
        z_desc = constant <| text "z:"
    in  div [div_style] <~ combine [desc, br, x_desc, xField, br, y_desc, yField, br, z_desc, zField]

paramsElement = 
    let desc = constant <| span [] [text "Function Parameters"]
        p1_d = constant <| text "p1:"
        p2_d = constant <| text "p2:"
        p3_d = constant <| text "p3:"
    in  div [div_style] <~ combine [desc, br, p1_d, p1Field,br,p2_d, p2Field,br,p3_d, p3Field]



-- Check whether runge kutta is on.    
isRK4 : Method -> Bool
isRK4 method = RK4 == method 

iterationsElement = 
    let desc = constant <| span [] [text "Simulation"]
        ix_d = constant <| text "ix:"
        dt_d = constant <| text "dt:"
        t0_d = constant <| text "t_0:"
        t0_stuff = combine [br, t0_d, t0Field]
        rest = combine [desc, br, ix_d, ixField, br, dt_d, dtField] 
        --iffy sig = case sig of
            --RK4 ->  (rest ++ t0_stuff)
            --Euler -> rest 
    in  div [div_style] <~ ((++) <~ rest ~ (keepThen (isRK4 <~ methodChoice.signal) [] t0_stuff))

--methodChoiceSignal : Signal Method

--rest : List [Signal HTML]
--combine_rest : Signal [List HTML]

--Signal [List Html] -> Signal Method -> Signal [List Html]


functionElement = 
    let desc = constant <| span [] [text "Chosen function"]
        method_desc = constant <| text "Chosen method"
    in  div [div_style] <~ combine ([desc,br,functionsDropdown, br, method_desc,br] ++ methodRadios) 
     

main = 
    let first_col = div [] <~ combine [functionElement]
        rest_cols = div [] <~ combine [iterationsElement, paramsElement, startPosElement]
    in div [] <~  combine [first_col, rest_cols] 
