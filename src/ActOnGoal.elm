module ActOnGoal exposing (actOnGoal)

import Helpers.WorldQuery exposing (..)
import Helpers.XY exposing (..)
import Random exposing (Generator)
import Random.Extra as Random
import Types exposing (..)


actOnGoal : Dwarf -> World -> Generator (List Msg)
actOnGoal dwarf world =
    case dwarf.goal of
        Sleep _ ->
            doNothing

        FindFood _ ->
            moveTowardsResource Food dwarf world

        FindBeer _ ->
            moveTowardsResource Beer dwarf world

        Eat _ ->
            decrementClosest Food dwarf world

        Drink _ ->
            decrementClosest Beer dwarf world

        Rage _ ->
            moveTowardsAndPunchDwarf dwarf world

        Idle _ ->
            moveRandomly dwarf



-- ACTIONS


doNothing : Generator (List Msg)
doNothing =
    Random.constant []


moveTowardsResource : ResourceKind -> Dwarf -> World -> Generator (List Msg)
moveTowardsResource kind dwarf world =
    case resourceClosestToDwarf kind dwarf world of
        Nothing ->
            moveRandomly dwarf

        Just resource ->
            [ dwarf.position
                |> deltaMovingCloserTo resource.position
                |> MoveDwarf dwarf.id
            ]
                |> Random.constant


decrementClosest : ResourceKind -> Dwarf -> World -> Generator (List Msg)
decrementClosest kind dwarf world =
    resourceTouchingDwarf kind dwarf world
        |> Maybe.map (\res -> [ DecrementResource res.id 1 ])
        |> Maybe.withDefault []
        |> Random.constant


moveTowardsAndPunchDwarf : Dwarf -> World -> Generator (List Msg)
moveTowardsAndPunchDwarf dwarf world =
    dwarfClosestToDwarf dwarf world
        |> Maybe.map
            (\toBePunched ->
                if isNextTo toBePunched.position dwarf.position then
                    [ PunchDwarf dwarf.id toBePunched.id ]
                else
                    [ dwarf.position
                        |> deltaMovingCloserTo toBePunched.position
                        |> MoveDwarf dwarf.id
                    ]
            )
        |> Maybe.withDefault []
        |> Random.constant


moveRandomly : Dwarf -> Generator (List Msg)
moveRandomly dwarf =
    Random.map2 (,)
        (Random.int -1 1)
        (Random.int -1 1)
        |> Random.map (\xyMove -> [ MoveDwarf dwarf.id xyMove ])
