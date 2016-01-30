import Hatlab.Plot
import Hatlab.ParametrizedCurves

circle r = Par (\t -> r*cos t) (\t -> r*sin t) (-2*pi, 2*pi) "Circle"

lissajous a b kx ky = Par (\t -> a*cos (kx*t)) (\t -> b*sin (ky*t)) (-2*pi, 2*pi) "Lissajous"
