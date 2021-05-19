module Example exposing (..)

import Browser
import Events as E
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

deep1 = (((((1 + ((((2 + 3)))))))))

p = 1 + 1

t = 10

p = let x = (((10))) 
    in x

q = let x = 10
     in ((x + 1))

main =
  Browser.sandbox { init = 0, update = update, view = view }

update msg model =
    case msg of
      Increment ->
        (model + 1) + 2

      Decrement ->
        model - 1

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text ((String.fromInt model)) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

