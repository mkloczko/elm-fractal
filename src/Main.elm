module Main where

import Signal exposing (Mailbox, mailbox, Signal, constant,foldp)
import Signal.Extra exposing ((<~), (~), keepWhen)
import List exposing (map, foldl)

import Frontend exposing (frontus)
import Backend exposing (..)
import Input exposing (..)
import Math.Numerical exposing (..)
import Math.Point3D exposing (Point3D, distance3D, divScalar)
import Math.Functions exposing (..)

port in_init : Signal Bool 
port in_focus : Signal Bool



camera1 : Signal Point3D 
camera1 = let
    f theta bool = if not bool then {x = 100 * sin(theta), y = 30 + 100 * sin(theta), z = 100 * cos(theta) }
        else {x = 100, y = 30, z = 100}
    in f <~ (degrees <~ updater2) ~ in_focus


port out_points  : Signal (List Point3D)
port out_points = calculatePoints <~ methodChoice.signal ~ chooseFunction ~ startingPoint ~ delta_time ~ start_time ~ iterations ~ in_init

port out_camera_lookat : Signal (Point3D, Point3D)
--port out_camera_lookat = updater <~ fps_clock
port out_camera_lookat = (\p1 -> (p1, {x = 0, y = 30, z = 0})) <~ camera1

main = frontus