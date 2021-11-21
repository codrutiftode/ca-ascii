module Vector3 (Vector3, Angle, Normal,
    lenV, negV, scaleV, dotV, addV,
    rotateZ, rotateX,
    rotateVectorsZ, rotateVectorsX,
    intToVect)
where

type Angle = Float
type Normal = Vector3
type Vector3 = (Float, Float, Float)

-- Length of vector
lenV :: Vector3 -> Float
lenV (x,y,z) = sqrt(x*x + y*y + z*z)

-- Scalar multiplication for vectors
scaleV :: Float -> Vector3 -> Vector3
scaleV s (x,y,z) = (s*x, s*y, s*z)

-- Switches the direction of a vector
negV :: Vector3 -> Vector3
negV v = scaleV (-1.0) v  

-- Dot product for vectors
dotV :: Vector3 -> Vector3 -> Float
dotV (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

-- Adds two vectors
addV :: Vector3 -> Vector3 -> Vector3
addV (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

-- Rotate a vector around the z-axis
rotateZ :: Angle -> Vector3 -> Vector3
rotateZ a (x,y,z) = (x*c - y*s, x*s + y*c, z)
          where c = cos a
                s = sin a


-- Rotate a vector around the x-axis
rotateX :: Angle -> Vector3 -> Vector3
rotateX a (x,y,z) = (x, y*c - z*s, y*s + z*c)
          where c = cos a
                s = sin a


-- Rotates vectors around the z-axis
rotateVectorsZ :: Angle -> [Vector3] -> [Vector3]
rotateVectorsZ a = map (rotateZ a) 


-- Rotates vectors around the x-axis
rotateVectorsX :: Angle -> [Vector3] -> [Vector3]
rotateVectorsX a = map (rotateX a) 

-- Builds a vector from int tuple
intToVect :: (Int, Int, Int) -> Vector3
intToVect (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)
