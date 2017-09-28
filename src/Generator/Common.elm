module Generator.Common exposing (position)

import Constants.World exposing (worldWidth, worldHeight)
import Random exposing (Generator)
import Types exposing (..)


position : Generator XY
position =
    Random.map2 (,)
        (Random.int 0 (worldWidth - 1))
        (Random.int 0 (worldHeight - 1))
