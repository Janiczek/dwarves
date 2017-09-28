module Generator.Dwarf exposing (dwarf)

import Generator.Common exposing (..)
import Random exposing (Generator)
import Random.Extra as Random
import Types exposing (..)


dwarf : Generator (Int -> Dwarf)
dwarf =
    Random.map3 Dwarf
        position
        goal
        hitpoints


goal : Generator Goal
goal =
    [ ( 10, Sleep )
    , ( 10, Idle )
    , ( 3, FindFood )
    , ( 2, FindBeer )
    ]
        |> List.map
            (\( freq, goal ) ->
                ( freq, Random.constant (goal { ticks = 0 }) )
            )
        |> Random.frequency


hitpoints : Generator Int
hitpoints =
    Random.int 3 5
