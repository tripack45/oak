module Level4.Model exposing (..)

buildBrick : String -> Int -> Int -> Int -> Block
buildBrick dir num x y =
    let
        comp = 1 --quantity 写起来手感不好，如果之后出问题了我立马改呜呜呜呜

        v1 =
            center |> Point3d.translateBy vec

        v2 =
            center |> Point3d.translateBy (Vector3d.reverse vec)
        in
        let
            this = Block3d.from v1 v2
            color = Color.darkCharcoal
            event = Nothing
            dir_ = R
        in
            Block this center color event dir_
