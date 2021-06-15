x1 = 1 + 1

x2 = 1 + if True then 1 else 2

x3 = 1 + 2 * if True then 1 else 2

x4 = 1 + 2 * if True then 1 else 2 + 4

x5 =[ div [ 1 ] [ 2 ]
    , div [ 3 ] [ 4 ]
    ] ++
    if True then
    [ 5 ]
    else [ 6 ]