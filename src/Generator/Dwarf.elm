module Generator.Dwarf exposing (dwarf)

import Constants.Goal exposing (..)
import Constants.World exposing (..)
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



-- COMPONENTS


goal : Generator Goal
goal =
    initialGoals
        |> List.map
            (\( freq, goal ) ->
                ( freq, Random.constant (goal { ticks = 0 }) )
            )
        |> Random.frequency


hitpoints : Generator Int
hitpoints =
    Random.int
        minStartingHealth
        maxStartingHealth
