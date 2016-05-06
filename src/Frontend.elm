module Frontend where

import Signal exposing (Signal, constant)
import Signal.Extra exposing ((<~), (~), combine, keepThen, keepWhen)
import List exposing (map, concat)


import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, id, style, tabindex)
import Html.Events exposing (onFocus, onBlur)

import Window 
import Mouse

import Math.Numerical exposing (..)
import Math.Point3D exposing (Point3D, divScalar)
import Input exposing (..)

-- Utility functions


import Time exposing (fps)



--------------
-- ELEMENTS --
--------------
br = constant <| Html.br [] []


startPosElement : Signal (List Html) 
startPosElement = 
    let desc = constant <| span [] [text "Starting position"]
        x_desc = constant <| text "x:"
        y_desc = constant <| text "y:"
        z_desc = constant <| text "z:"
    in  combine [desc, br, x_desc, xField, br, y_desc, yField, br, z_desc, zField]

paramsElement : Signal (List Html) 
paramsElement = 
    let desc = constant <| span [] [text "Function Parameters"]
        p1_d = constant <| text "p1:"
        p2_d = constant <| text "p2:"
        p3_d = constant <| text "p3:"
    in  combine [desc, br, p1_d, p1Field,br,p2_d, p2Field,br,p3_d, p3Field]



-- Check whether runge kutta is on.    
isRK4 : Method -> Bool
isRK4 method = RK4 == method 

iterationsElement : Signal (List Html) 
iterationsElement = 
    let desc = constant <| span [] [text "Simulation"]
        ix_d = constant <| text "ix:"
        dt_d = constant <| text "dt:"
        t0_d = constant <| text "t0:"
        t0_stuff = combine [br, t0_d, t0Field]
        rest = combine [desc, br, ix_d, ixField, br, dt_d, dtField] 
        --iffy sig = case sig of
            --RK4 ->  (rest ++ t0_stuff)
            --Euler -> rest 
    in  (++) <~ rest ~ (keepThen (isRK4 <~ methodChoice.signal) [] t0_stuff)


functionElement : Signal (List Html) 
functionElement = 
    let desc = constant <| span [] [text "Function"]
        method_desc = constant <| text "Method"
    in  combine ([desc,br,functionsDropdown, br, method_desc,br] ++ methodRadios) 
     

div_style1 = style [ ("box-sizing", "border-box"),("width","25%"),("float","left")
                  , ("padding-left", "5px"), ("padding-right", "5px") ]

div_style2 = style [ ("box-sizing", "border-box"),("width","50%"),("float","left")
                  , ("padding-left", "5px"), ("padding-right", "5px") ]


positioning : Int -> List (List Html) -> List (List Html) -> Html
positioning width first_row second_row = 
    if width > 160*4 then
        let divs = map (div [class "wide", id "nav"]) <| concat [first_row, second_row] --Divs are next to each other
            glob_style = style [("max-width","720px")]
        in div [glob_style,id "controls"] divs
    else 
        let firsties = map (div [class "narrow", id "nav"]) first_row
            seconds  = map (div [class "narrow", id "nav"]) second_row
            own_style = style [ ("overflow", "auto"),   ("width", "100%"), ("height","50%")
                              , ("margin-top", "5px"),  ("margin-bottom", "5px")]
            glob_style = style [("max-width", "400px"), ("min-width", "340px")]
        in  div [glob_style, id "controls"] [div [own_style] firsties, div [own_style] seconds]

combine2 : List (List (Signal a)) -> Signal (List (List a))
combine2 ls = combine <| map combine ls

frontus = 
    let first_cols = combine [functionElement, iterationsElement]
        rest_cols = combine [paramsElement, startPosElement]
    in positioning <~ Window.width ~ first_cols ~ rest_cols
