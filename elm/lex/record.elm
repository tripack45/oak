module Example exposing (..)

rec = 
    { name = "yue"
    , game = "Hades"
    }

rec1 = { rec | game = "IO" }

name = rec1.name

game = { rec | game = "IO" }.game
