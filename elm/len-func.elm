module Example exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  let 
    f y = 
      let 
        g model = 
          div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , div [] [ text (String.fromInt model) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
      in 
        g y
  in
    f model

viewX model =
  let 
    tt = div [] [ text (String.fromInt model) ]
    f y = 
      let 
        g model = 
          div []
            [ button [ onClick Decrement ] [ text "-" ]
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , tt
            , button [ onClick Increment ] [ text "+" ]
            ]
      in 
        g y
  in
    f model

