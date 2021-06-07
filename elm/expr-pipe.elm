main =
  Browser.sandbox { init = 0, update = update, view = view }

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  [ button [ onClick Decrement ] [ text "-" ]
  , div [] [ text (String.fromInt model) ]
  , button [ onClick Increment ] [ text "+" ]
  ] |>
  div []