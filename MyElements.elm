module MyElements where

import Signal exposing (..)
import Signal.Extra exposing ((<~), (~))
import String
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input.Field exposing (..)
import Graphics.Input exposing (..)

import NumericBox exposing (..)


-- Common abstractions over 
{-|Creates a signal of the value and the element for intBox|-}
intSignalBox : Int -> Int -> Int -> Int -> (Signal Int, Signal Element)
intSignalBox min max step val = 
    let 
        the_mailbox = mailbox val
        the_element = intBox (message the_mailbox.address) min max step <~ the_mailbox.signal
    in 
        (the_mailbox.signal, the_element)

{-|Creates a signal of the value and the element for floatBox|-}
floatSignalBox : Float -> Float -> Float -> Float -> (Signal Float, Signal Element)
floatSignalBox min max step val = 
    let 
        the_mailbox = mailbox val
        the_element = floatBox (message the_mailbox.address) min max step <~ the_mailbox.signal
    in 
        (the_mailbox.signal, the_element)




(iterations, ixField) = intSignalBox 0 100000 100 10000
(p1, p1Field) = floatSignalBox (-1) 100 0.0005 10
(p2, p2Field) = floatSignalBox (-1) 100 0.0005 28
(p3, p3Field) = floatSignalBox (-1) 100 0.0005 2.66

(xSignal, xField) = floatSignalBox (-100) 100 0.2 1
(ySignal, yField) = floatSignalBox (-100) 100 0.2 1
(zSignal, zField) = floatSignalBox (-100) 100 0.2 1

--- Choose functions --- 

type Functions = Lorenz 

functionsChoice : Signal.Mailbox Functions
functionsChoice = Signal.mailbox Lorenz

functionButtons = dropDown (Signal.message functionsChoice.address)
    [ ("Lorenz", Lorenz)
    ]

