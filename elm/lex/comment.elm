module Example exposing (..)

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

-- Hey.

{- I guess this should work -}
{-| guess doc -}
{- {- oh. -} -}

add a b =
    a + b
