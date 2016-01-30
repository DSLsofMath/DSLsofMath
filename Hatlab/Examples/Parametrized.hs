import Hatlab.Plot
import Hatlab.ParametrizedCurves

circle r = Par (\t -> r*cos t) (\t -> r*sin t) (-2*pi, 2*pi) "Circle"

lissajous a b kx ky = Par (\t -> a*cos (kx*t)) (\t -> b*sin (ky*t)) (-2*pi, 2*pi) "Lissajous"

butterfly = polarCurve (\theta -> ((exp . sin) theta) - 2*(cos (4*theta))+(sin ((2*theta - pi)/24))^5) (-30, 30) "Butterfly"

circlePolar r = polarCurve (\_ -> r) (-2*pi, 2*pi) "Circle"

cuteExample a b c d e j i = Par (\t -> i * cos (a*t) - cos (b*t) * sin (c*t)) (\t -> j*sin (d*t) - sin (e*t)) (-10, 10) "Cute"
