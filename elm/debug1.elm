module CollisionBlock exposing (..)

import Tools exposing (getBall)
import Model exposing (Ball, Brick, Block, Model, HitTime(..))

+ 
 
-- Status of collision
type Hit
    = Safe
    | X
    | Y
    | Corner
