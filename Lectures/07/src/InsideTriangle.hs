----------------
-- If you would like to extend the Shape library to triangles,
-- Barycentric coordinates is the key math needed.

type Triple = (Double, Double, Double) -- invariant: they sum to 1

barycentricCoordinates :: Vec -> Vec -> Vec -> Point -> Triple
barycentricCoordinates (V x1 y1) (V x2 y2) (V x3 y3) (V x y) =
  let invDenom = 1 / ((y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3))
      x' = x-x3; y' = y-y3;
      b1 = ( x' *(y2-y3) + y' *(x3-x2) ) * invDenom
      b2 = ( x' *(y3-y1) + y' *(x1-x3) ) * invDenom
      b3 = 1 - b1 - b2  
  in (b1, b2, b3) 

insideTriangle :: Vec -> Vec -> Vec -> Point -> Bool
insideTriangle p1 p2 p3 p = 
  let (b1, b2, b3) = barycentricCoordinates p1 p2 p3 p
  in all (0<=) [b1, b2, b3]
