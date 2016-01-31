module Examples.Shapes where
import Hatlab.Relations
import Hatlab.Plot

circle :: Double -> (Double, Double) -> Deep Relation
circle r' (x0, y0) = r (\x y -> (x-x0)*(x-x0)+(y-y0)*(y-y0) < r'*r') "Circle"

square :: (Double, Double) -> (Double, Double) -> Deep Relation
square (xlow, ylow) (xhigh, yhigh) = r (\x y -> (x <= xhigh) && (x >= xlow)
                                             && (y <= yhigh) && (y >= ylow)) "Square"

triangle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Deep Relation
triangle (x0, y0) (x1, y1) (x2, y2) = r fun "Triangle"
    where
        fun x y = (b1 == b2) && (b2 == b3)
            where
                sig (p1x, p1y) (p2x, p2y) (p3x, p3y) = (p1x-p3x)*(p2y-p3y)-(p2x-p3x)*(p1y-p3y)

                b1 = sig (x, y) (x0, y0) (x1, y1)  < 0
                b2 = sig (x, y) (x1, y1) (x2, y2)  < 0
                b3 = sig (x, y) (x2, y2) (x0, y0)  < 0
