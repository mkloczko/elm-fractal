module Input.Template where

import Html exposing (Html, text,input, select, option, toElement)
import Html.Attributes as A exposing (..)
import Html.Events exposing (on, targetValue, targetChecked)
import Signal exposing (Mailbox, mailbox)

import String exposing (toFloat, toInt)
import Signal exposing (Signal, Message)
import Signal.Extra exposing ((<~), (~))

{-| Creates a float number HTML input based on floats and converts it to Element |-}
floatBox : (Float -> Message) ->Float -> Float -> Float -> Float -> Html
floatBox sender min max step val = 
    let
        listener1 = on "input" targetValue (\str -> sender <| Result.withDefault 0.0 <| String.toFloat str )
        attribs_style = [A.id "input"]
        attribs = [listener1, A.step <| toString step, A.type' <| "number", A.min <| toString min, A.max <| toString max, A.value <| toString val]
        the_element = input (attribs ++ attribs_style) []
    in
        the_element

{-| Creates an integer number HTML input and converts it to Element |-}
intBox : (Int -> Message) ->Int -> Int -> Int -> Int -> Html
intBox sender min max step val = 
    let
        listener1 = on "input" targetValue (\str -> sender <| Result.withDefault 0 <| String.toInt str )
        attribs_style = [A.id "input"]
        attribs = [listener1, A.step <| toString step, A.type' <| "number", A.min <| toString min, A.max <| toString max, A.value <| toString val]
        the_element = input (attribs ++ attribs_style) []
    in
        the_element

radio : (a -> Message) -> a->  Bool -> Html
radio sender val is_on= 
    let
        listener1 = on "change" targetChecked (\_ -> sender val)
        attribs_style = [A.id "input"]
        attribs      = [listener1, A.type' "radio", A.checked is_on]
        the_element = input (attribs) []
    in the_element 

dropdown : (a -> Message) -> (String -> a) -> List (String, String) -> String -> Html
dropdown sender translator vals on_val =
    let
        listener1 = on "change" targetValue (\str -> sender <| translator str)
        attribs   = [listener1]
        attribs_style = [A.id "input"]
        options   = List.map (\(v,txt) -> option [A.value v, A.selected <| v == on_val] [text txt]) vals
        the_element = select attribs options
    in the_element