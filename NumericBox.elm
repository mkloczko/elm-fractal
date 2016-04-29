module NumericBox where

import Html exposing (text,input, toElement)
import Html.Attributes as A exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Mailbox, mailbox)
import Graphics.Element exposing (Element, show, flow, down,right)
import Graphics.Collage exposing (..)
import String exposing (toFloat, toInt)
import Signal exposing (Signal, Message)
import Signal.Extra exposing ((<~), (~))

{-| Creates a float number HTML input based on floats and converts it to Element |-}
floatBox : (Float -> Message) ->Float -> Float -> Float -> Float -> Element
floatBox sender min max step val = let
    listener1 = on "input" targetValue (\str -> sender <| Result.withDefault 0.0 <| String.toFloat str )
    attribs_size = [A.style [("width", "89%"),("height", "100%")]]
    attribs = [listener1, A.step <| toString step, A.type' <| "number", A.min <| toString min, A.max <| toString max, A.value <| toString val]
    the_element = toElement 100 22 <| input (attribs ++ attribs_size) []
    in
    the_element

{-| Creates an integer number HTML input and converts it to Element |-}
intBox : (Int -> Message) ->Int -> Int -> Int -> Int -> Element
intBox sender min max step val = let
    listener1 = on "input" targetValue (\str -> sender <| Result.withDefault 0 <| String.toInt str )
    attribs_size = [A.style [("width", "89%"),("height", "100%")]]
    attribs = [listener1, A.step <| toString step, A.type' <| "number", A.min <| toString min, A.max <| toString max, A.value <| toString val]
    the_element = toElement 100 22 <| input (attribs ++ attribs_size) []
    in
    the_element

