module Input where

import Signal exposing (..)
import Signal.Extra exposing ((<~), (~), combine, switchSample)
import String
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input.Field exposing (..)
import Graphics.Input exposing (..)

import Html exposing (Html)

import Input.Template exposing (..)


-- Common abstractions over 
{-|Creates a signal of the value and the element for intBox|-}
intSignalBox : Int -> Int -> Int -> Int -> (Signal Int, Signal Html)
intSignalBox min max step val = 
    let 
        the_mailbox = mailbox val
        the_element = intBox (message the_mailbox.address) min max step <~ the_mailbox.signal
    in 
        (the_mailbox.signal, the_element)

{-|Creates a signal of the value and the element for floatBox|-}
floatSignalBox : Float -> Float -> Float -> Float -> (Signal Float, Signal Html)
floatSignalBox min max step val = 
    let 
        the_mailbox = mailbox val
        the_element = floatBox (message the_mailbox.address) min max step <~ the_mailbox.signal
    in 
        (the_mailbox.signal, the_element)




(iterations, ixField) = intSignalBox   0 100000 100 10000

(dte_time, dteField) = floatSignalBox 0 2 0.0005 0.005
(dtrk_time, dtrkField) = floatSignalBox 0 2 0.0005 0.05
(start_time, t0Field) = floatSignalBox 0 5 0.05 0.5

(pl1, pl1Field) = floatSignalBox (-1) 100 0.0005 10
(pl2, pl2Field) = floatSignalBox (-1) 100 0.0005 28
(pl3, pl3Field) = floatSignalBox (-1) 100 0.0005 2.66 

(pr1, pr1Field) = floatSignalBox (-1) 100 0.0005 0.2 
(pr2, pr2Field) = floatSignalBox (-1) 100 0.0005 0.2 
(pr3, pr3Field) = floatSignalBox (-1) 100 0.0005 5.7 


(xSignal, xField) = floatSignalBox (-100) 100 0.2 1
(ySignal, yField) = floatSignalBox (-100) 100 0.2 1
(zSignal, zField) = floatSignalBox (-100) 100 0.2 1

--- Choose functions --- 

type Functions = Lorenz | Rossler

functionsChoice : Mailbox Functions
functionsChoice = mailbox Lorenz

funToText : String -> Functions
funToText str = case str of
    "Lorenz"  -> Lorenz
    "Rossler" -> Rossler
    _         -> Lorenz

functionsDropdown = 
    let
        sender = Signal.message functionsChoice.address 
        opts = [("Lorenz","Lorenz"), ("Rossler","Rossler")]
    in dropdown sender funToText opts <~ (toString <~ functionsChoice.signal)

--functionButtons = dropDown (Signal.message functionsChoice.address)
--    [ ("Lorenz", Lorenz)
--    ]

--- Choose derivatation method --- 

type Method = Euler | RK4

methodChoice : Mailbox Method
methodChoice = mailbox RK4

methodRadios = 
    let isEq a b = a == b
        rad1 = radio (Signal.message methodChoice.address) Euler <~ (isEq Euler <~ methodChoice.signal)
        name1 = Html.text "Euler"
        rad2 = radio (Signal.message methodChoice.address) RK4 <~ (isEq RK4 <~ methodChoice.signal)  
        name2 = Html.text "RK 4"
        --For debugging!
        --le_debug = Html.text <~ (toString <~ methodChoice.signal) 
    in  [rad1, constant name1, rad2, constant name2]



--Switch

switchMethod' : Method -> a -> a -> a
switchMethod' method v1 v2 = 
    case method of
        Euler -> v1
        RK4   -> v2

switchMethod s_v1 s_v2 = switchMethod' <~ methodChoice.signal ~ s_v1 ~ s_v2


switchFunctions' : Functions -> a -> a -> a
switchFunctions' functions v1 v2 =
    case functions of
        Lorenz  -> v1
        Rossler -> v2

switchFunctions s_v1 s_v2 = switchFunctions' <~ functionsChoice.signal ~ s_v1 ~ s_v2

---

delta_time = switchMethod dtrk_time dte_time
dtField    = switchMethod dtrkField dteField


p1 = switchFunctions pl1 pr1
p2 = switchFunctions pl2 pr2
p3 = switchFunctions pl3 pr3

p1Field = switchFunctions pl1Field pr1Field
p2Field = switchFunctions pl2Field pr2Field
p3Field = switchFunctions pl3Field pr3Field