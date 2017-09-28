module Update exposing (init, update, subscriptions)

import Cmd exposing (..)
import Cmd.Extra exposing (..)
import Constants.World exposing (..)
import List.Extra as List
import Random exposing (Generator)
import Time
import Types exposing (..)
import Update.Dwarf exposing (updatedDwarfGenerator)
import Update.Extra exposing (..)


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
        |> findDwarf dwarfId
        |> Maybe.map
            (\d ->
                d
                    |> move xyMove
                    |> asDwarfIn world.dwarves
                    |> asDwarvesIn world
            )
        |> Maybe.withDefault world


findDwarf : Id -> List Dwarf -> Maybe Dwarf
findDwarf dwarfId dwarves =
    dwarves
        |> List.find (\d -> d.id == dwarfId)


findResource : Id -> List Resource -> Maybe Resource
findResource resourceId resources =
    resources
        |> List.find (\r -> r.id == resourceId)


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
        (world.dwarves |> findDwarf whoId)
        (world.dwarves |> findDwarf whomId)
        |> Maybe.withDefault world


decrementHitpoints : Int -> Dwarf -> Maybe Dwarf
decrementHitpoints amount dwarf =
    if dwarf.hitpoints <= amount then
        -- dies
        Nothing
    else
        Just { dwarf | hitpoints = dwarf.hitpoints - amount }


decrementResource : Id -> Int -> World -> World
decrementResource resourceId amount world =
    world.resources
        |> findResource resourceId
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
        |> findResource resourceId
        |> Maybe.map
            (\resource ->
                { resource | amount = resource.amount + amount }
                    |> asResourceIn world.resources
                    |> asResourcesIn world
            )
        |> Maybe.withDefault world


removeDwarf : Dwarf -> List Dwarf -> List Dwarf
removeDwarf dwarf dwarves =
    dwarves
        |> List.filter (\d -> d.id /= dwarf.id)


removeResource : Resource -> List Resource -> List Resource
removeResource resource resources =
    resources
        |> List.filter (\r -> r.id /= resource.id)


asResourceIn : List Resource -> Resource -> List Resource
asResourceIn resources resource =
    resources
        |> List.replaceIf (\r -> r.id == resource.id) resource


asResourcesIn : World -> List Resource -> World
asResourcesIn world resources =
    { world | resources = resources }


asDwarfIn : List Dwarf -> Dwarf -> List Dwarf
asDwarfIn dwarves dwarf =
    dwarves
        |> List.replaceIf (\d -> d.id == dwarf.id) dwarf


asDwarvesIn : World -> List Dwarf -> World
asDwarvesIn world dwarves =
    { world | dwarves = dwarves }


move : XY -> HasPosition a -> HasPosition a
move ( dx, dy ) thing =
    let
        ( x, y ) =
            thing.position

        newPosition =
            ( x + dx |> clamp 0 (worldWidth - 1)
            , y + dy |> clamp 0 (worldHeight - 1)
            )
    in
        { thing | position = newPosition }
