module Math.Point3D where

type alias Point3D = {
  x: Float,
  y: Float,
  z: Float
  }

addScalar : Point3D -> Float -> Point3D
addScalar {x,y,z} v = {x = x + v, y = y+v, z = z + v}

addPoint : Point3D -> Point3D -> Point3D
addPoint p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y, z = p1.z + p2.z}

multScalar : Point3D -> Float -> Point3D
multScalar {x,y,z} v = {x = x * v, y = y*v, z = z * v}

divScalar : Point3D -> Float -> Point3D
divScalar {x,y,z} v = {x = x / v, y = y / v, z = z / v}

distance3D : Point3D -> Point3D -> Float
distance3D pt1 pt2 =
    let dx = (pt1.x - pt2.x)^2
        dy = (pt1.y - pt2.y)^2
        dz = (pt1.z - pt2.z)^2
    in sqrt (dx + dy + dz)
        
--params = {p1 = 28, p2 = 10, p3 = 2.66}
--start_pt = {x = 1, y = 0, z = 0}

--new_pt = rk4 (lorenzNumerical params) 0.05 0 start_pt