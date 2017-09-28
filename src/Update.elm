module Update exposing (init, update, subscriptions)

import ActOnGoal exposing (..)
import Cmd.Extra exposing (..)
import Constants.World exposing (..)
import Generator.Dwarf as Generator
import Generator.Resource as Generator
import Helpers.World exposing (..)
import Helpers.XY exposing (..)
import List.Extra as List
import Random exposing (Generator)
import Random.Extra as Random
import Time
import Types exposing (..)
import Update.Goal exposing (nextGoal)


init : ( World, Cmd Msg )
init =
    { dwarves = []
    , resources = []
    , nextId = 0
    }
        |> withCmds
            [ genInitData
                { dwarves = initDwarves
                , resources = initResources
                }
            ]


subscriptions : World -> Sub Msg
subscriptions world =
    Time.every (200 * Time.millisecond) (always Tick)


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        GeneratedInitData data ->
            generatedInitData data world

        Tick ->
            tick world

        UpdateDwarf ( dwarf, changes, goal ) ->
            updateDwarf ( dwarf, changes, goal ) world

        -- "World changes" - always without Cmds:
        MoveDwarf dwarf move ->
            moveDwarf dwarf move world
                |> withNoCmd

        PunchDwarf who whom ->
            punchDwarf who whom world
                |> withNoCmd

        DecrementResource resource amount ->
            decrementResource resource amount world
                |> withNoCmd

        IncrementResource resource amount ->
            incrementResource resource amount world
                |> withNoCmd



-- SUBFUNCTIONS


generatedInitData :
    ( List (Int -> Dwarf), List (Int -> Resource) )
    -> World
    -> ( World, Cmd Msg )
generatedInitData ( dwarves, resources ) world =
    let
        nextIdAfterDwarves =
            world.nextId + List.length dwarves

        nextIdAfterAll =
            nextIdAfterDwarves + List.length resources
    in
        { world
            | nextId = nextIdAfterAll
            , dwarves =
                world.dwarves
                    ++ List.map2 (\id dwarf -> dwarf id)
                        (List.range world.nextId (nextIdAfterDwarves - 1))
                        dwarves
            , resources =
                world.resources
                    ++ List.map2 (\id resource -> resource id)
                        (List.range nextIdAfterDwarves (nextIdAfterAll - 1))
                        resources
        }
            |> withNoCmd


tick : World -> ( World, Cmd Msg )
tick world =
    let
        updateDwarvesCmds =
            world.dwarves
                |> List.map (updatedDwarfGenerator world)
                |> List.map (Random.generate UpdateDwarf)
    in
        world
            |> withCmds updateDwarvesCmds


updateDwarf : ( Dwarf, List Msg, Goal ) -> World -> ( World, Cmd Msg )
updateDwarf ( dwarf, changes, goal ) world =
    let
        worldWithNewGoal =
            world.dwarves
                |> List.updateIf
                    (\d -> d.id == dwarf.id)
                    (\d -> { d | goal = goal })
                |> asDwarvesIn world
    in
        changes
            -- these changes don't have Cmds
            |> List.foldl (\msg world -> update msg world |> Tuple.first)
                worldWithNewGoal
            -- is a World now
            |> withNoCmd


moveDwarf : Id -> XY -> World -> World
moveDwarf dwarfId xyMove world =
    world.dwarves
        |> findById dwarfId
        |> Maybe.map
            (\d ->
                d
                    |> move xyMove
                    |> asDwarfIn world.dwarves
                    |> asDwarvesIn world
            )
        |> Maybe.withDefault world


punchDwarf : Id -> Id -> World -> World
punchDwarf whoId whomId world =
    Maybe.map2
        (\who whom ->
            if isNextTo who.position whom.position then
                whom
                    |> decrementHitpoints 1
                    |> Maybe.map
                        (\punchedDwarf ->
                            punchedDwarf
                                |> asDwarfIn world.dwarves
                                |> asDwarvesIn world
                        )
                    |> Maybe.withDefault
                        (world.dwarves
                            |> removeDwarf whom
                            |> asDwarvesIn world
                        )
            else
                world
        )
        (world.dwarves |> findById whoId)
        (world.dwarves |> findById whomId)
        |> Maybe.withDefault world


decrementResource : Id -> Int -> World -> World
decrementResource resourceId amount world =
    world.resources
        |> findById resourceId
        |> Maybe.map
            (\resource ->
                if resource.amount <= amount then
                    world.resources
                        |> removeResource resource
                        |> asResourcesIn world
                else
                    { resource | amount = resource.amount - amount }
                        |> asResourceIn world.resources
                        |> asResourcesIn world
            )
        |> Maybe.withDefault world


incrementResource : Id -> Int -> World -> World
incrementResource resourceId amount world =
    world.resources
        |> findById resourceId
        |> Maybe.map
            (\resource ->
                { resource | amount = resource.amount + amount }
                    |> asResourceIn world.resources
                    |> asResourcesIn world
            )
        |> Maybe.withDefault world



-- CMDS


genInitData : { dwarves : Int, resources : Int } -> Cmd Msg
genInitData counts =
    let
        dwarves =
            Generator.dwarf
                |> Random.list counts.dwarves

        resources =
            Generator.resource
                |> Random.list counts.resources
    in
        Random.map2 (,) dwarves resources
            |> Random.generate GeneratedInitData



-- GENERATORS


updatedDwarfGenerator : World -> Dwarf -> Generator ( Dwarf, List Msg, Goal )
updatedDwarfGenerator world dwarf =
    Random.map3 (,,)
        (Random.constant dwarf)
        (actOnGoal dwarf world)
        (nextGoal dwarf world)
