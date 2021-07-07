module Example exposing (..)

add_record {a, b} =
    a + b

matching lst =
  case lst of
    a :: b :: _ -> 
      a + b
    a :: _ ->
      a
    [] ->
      0
