module Example exposing (..)

rec = 
    { name = "mn"
    , game = "Hades"
    }

rec1 = { rec | game = "IO" }

name = rec1.name

game = { rec | game = "IO" }.game

e = x.a.b.c

e = .c x.a.b

err1 = x .d

err2 = .c x.a.b .d.e

s = (a).x
p = .ac {}
q = x(.a)
r = (.a)x
t = (.a)

type alias Point =
    { x : Float
    , y : Float
    }

point x y = 
    Point x y
