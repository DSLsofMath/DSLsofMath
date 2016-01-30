import Hatlab.Plot
import Hatlab.ParametrizedCurves

circle r = Par (\t -> r*cos t) (\t -> r*sin t) (-2*pi, 2*pi) "Circle"

lissajous a b kx ky = Par (\t -> a*cos (kx*t)) (\t -> b*sin (ky*t)) (-2*pi, 2*pi) "Lissajous"

butterfly = polarCurve (\theta -> ((exp . sin) theta) - 2*(cos (4*theta))+(sin ((2*theta - pi)/24))^5) (-2*pi, 2*pi) "Butterfly"

circlePolar r = polarCurve (\_ -> r) (-2*pi, 2*pi) "Circle"
