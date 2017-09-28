module Update.Dwarf exposing (..)

import Generator.Dwarf.Goals exposing (..)
import Random exposing (Generator)
import Random.Extra as Random
import Types exposing (..)
import Update.Goal exposing (nextGoal)
import Update.Extra exposing (..)


updatedDwarfGenerator : World -> Dwarf -> Generator ( Dwarf, List Msg, Goal )
updatedDwarfGenerator world dwarf =
    Random.map3 (,,)
        (Random.constant dwarf)
        (actOnGoal dwarf world)
        (nextGoal dwarf world)


actOnGoal : Dwarf -> World -> Generator (List Msg)
actOnGoal dwarf world =
    case dwarf.goal of
        Sleep _ ->
            sleep

        FindFood _ ->
            dwarf
                |> moveTowardsFood world

        FindBeer _ ->
            dwarf
                |> moveTowardsBeer world

        Eat _ ->
            dwarf
                |> eatFood world

        Drink _ ->
            dwarf
                |> drinkBeer world

        Rage _ ->
            dwarf
                |> attackClosestDwarf world

        Idle _ ->
            dwarf
                |> moveRandomly
