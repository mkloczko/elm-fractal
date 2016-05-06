module Main where

import Signal exposing (Mailbox, mailbox, Signal, merge, constant,foldp,sampleOn)
import Signal.Extra exposing ((<~), (~), keepWhen, keepWhenI,combine)
import List exposing (map, foldl)

import Frontend exposing (frontus)
import Backend exposing (..)
import Input exposing (..)
import Math.Numerical exposing (..)
import Math.Point3D exposing (Point3D, distance3D, divScalar)
import Math.Functions exposing (..)

import Html exposing (text)

import Mouse
import Keyboard

port in_init : Signal Bool 
port in_focus : Signal Bool
port in_scroll : Signal Int

processScroll : Int -> Float
processScroll val = -(toFloat val) / 60.0


port out_points : Signal (List Point3D)
port out_points = calculatePoints <~ methodChoice.signal ~ chooseFunction ~ startingPoint ~ delta_time ~ start_time ~ iterations ~ in_init



sampleWhenI : Signal Bool -> Signal a -> Signal a
sampleWhenI sig_b sig_a = keepWhenI sig_b <| merge sig_a <| sampleOn sig_b sig_a
    --in  sig_a

and = foldl (&&) True 

negate : Signal Bool -> Signal Bool
negate sig = (\n -> not n) <~ sig
--Camera logic

canRotate = and <~ combine [in_focus, Mouse.isDown, negate Keyboard.shift]
canAdjustY = and <~ combine [in_focus, Mouse.isDown, Keyboard.shift]
canAdjustR = and <~ combine [in_focus, negate Keyboard.shift]

pos_r =
    --let off_r = (\x -> x*30) <~( keepWhen canAdjustR 0 (snd <~ mouse_offies))
    let off_r = (\x -> -x) <~( keepWhen canAdjustR 0 (processScroll <~ in_scroll) )
    in foldp (\x y -> max (x+y) 25 ) 100 off_r

pos_y = 
    let off_y = (\x -> x * 30) <~ (keepWhen canAdjustY 0 (snd <~ mouse_offies))
    in  foldp (\x y -> max (x + y) 0) 50 off_y

lookAtP3D = 
    let f val= {x=0,y=val,z=0}
    in f <~ pos_y

port out_camera_lookat : Signal (Point3D, Point3D)
--port out_camera_lookat = updater <~ fps_clock
port out_camera_lookat = (\p1 p2 -> (p1, p2)) <~ (sampleWhenI in_init <| camera_manual (0,0) pos_r pos_y canRotate) ~ lookAtP3D

main = frontus
--main = text <~ (toString <~ pos_r)