update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1