module Example exposing (..)

type T = T Int Int

f =
  case T 1 2 of
    T a b as c -> c

g = 
  case [[1],[2]] of
      x :: xs as y -> y
      [] -> []

l =
  case [T 1 2, T 3 4] of
    T a b :: _ -> a
    _ -> 0

ta =
  case [T 1 2, T 3 4] of
    T a b :: xs as t -> t
    _ -> []

type TL a = TL (List a) (List a)

taa =
  case [TL [1] [2], TL [3] [4]] of
    TL a b :: xs as t -> (b,t)
    _ -> ([],[])
