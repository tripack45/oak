module Example exposing (..)

import Browser
import Html as H exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

type T a b c d  = A a
                | B a a 
                | C ()
                | D (List b)
                | E (a, List b, a -> c)
                | F (a -> b -> c)
                | G {x : Int}
                | H {d | x : Int | y : Int}
                | I {d | x : Html.Html | y : Int}
                | J {d | x : Int | y : H.Html}

type alias T2 = T Int

type alias T3 = { x : Int, y : String }

type alias T4 a = { a | x : Int, z : Int}

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

