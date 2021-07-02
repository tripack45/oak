import Browser

main =
  Browser.sandbox { init = 0, update = update, view = view }

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

p = let x = 1 in x

q = let x = 1 in let y = x in let z = y in z

r = let x = let y = let z = 1 in z in y in x

a : Int
a = 1

b = (1, 2)
b : (Int, Int)

d = let c : Int 
        c = 1 
     in c

f : Int -> Int
f x = x


f0 : a -> Int
f0 x = 
  let g : b -> a
      g t = x
   in 0


x1 : a -> Int
x2 : a -> Int
(x1, x2) = let t : a -> Int 
               t = \x -> 1
            in (t, t)
 