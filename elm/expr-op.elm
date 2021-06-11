main =
  Browser.sandbox { init = 0, update = update, view = view }

update msg =
  \model ->
    case msg of
      Increment ->
        if 1 < 0 || 42 ^ 1 >= 42 && 3 /= 3 // 2 && 7 == 7 then
        model + 1 else model + 1

      Decrement ->
        model - 1

view model =
  [ button [ onClick Decrement ] <| (text "-") :: [] ++ []
  , div [] [ text <| String.fromInt model ]
  , button [ onClick Increment ] [ text "+" ]
  ] |>
  div []