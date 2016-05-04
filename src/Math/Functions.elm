module Math.Functions where

import Math.Point3D exposing (..)

type alias FunParams ={
    p1 : Float,
    p2 : Float,
    p3 : Float
}


rosslerFunction : FunParams -> Point3D -> Point3D
rosslerFunction {p1,p2,p3} {x,y,z} =
    let nx = -(y + z)
        ny = x + (p1*y)
        nz = p2 + x*z - (p3 * z)
    in {x=nx,y=ny,z=nz}

lorenzFunction : FunParams -> Point3D -> Point3D
lorenzFunction {p1,p2,p3} {x,y,z} =    
    let nx = p1*(y - x)
        ny = x*(p2 - z) - y
        nz = x*y - p3*z
    in {x=nx,y=ny,z=nz}

