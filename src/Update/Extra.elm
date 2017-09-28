module Update.Extra exposing (..)

import Constants.World as Constants
import Random exposing (Generator)
import Random.Extra as Random
import Types exposing (..)


move : ( Int, Int ) -> HasPosition a -> HasPosition a
move ( dx, dy ) thing =
    let
        ( x, y ) =
            thing.position
    in
        { thing | position = ( x + dx, y + dy ) |> clampPosition }


clampPosition : ( Int, Int ) -> ( Int, Int )
clampPosition ( x, y ) =
    ( clamp 0 (Constants.worldWidth - 1) x
    , clamp 0 (Constants.worldHeight - 1) y
    )


resourceTouchingDwarf : ResourceKind -> Dwarf -> World -> Maybe Resource
resourceTouchingDwarf kind dwarf world =
    world.resources
        |> List.filter (\resource -> resource.kind == kind)
        |> List.filter (\resource -> isNextTo resource.position dwarf.position)
        |> List.head


resourceClosestToDwarf : ResourceKind -> Dwarf -> World -> Maybe Resource
resourceClosestToDwarf kind dwarf world =
    world.resources
        |> List.filter (\resource -> resource.kind == kind)
        |> List.sortBy (\resource -> distance resource.position dwarf.position)
        |> List.head


isDwarfNextTo : ResourceKind -> Dwarf -> World -> Bool
isDwarfNextTo kind dwarf world =
    resourceTouchingDwarf kind dwarf world /= Nothing


isNextTo : XY -> XY -> Bool
isNextTo ( x1, y1 ) ( x2, y2 ) =
    (abs (x1 - x2) < 2)
        && (abs (y1 - y2) < 2)


distance : XY -> XY -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
        |> toFloat
        |> sqrt


decrementClosest : ResourceKind -> Dwarf -> World -> Generator (List Msg)
decrementClosest kind dwarf world =
    resourceTouchingDwarf kind dwarf world
        |> Maybe.map (\res -> [ DecrementResource res.id 1 ])
        |> Maybe.withDefault []
        |> Random.constant


moveCloserTo : XY -> XY -> XY
moveCloserTo ( goalX, goalY ) ( currentX, currentY ) =
    ( compare goalX currentX |> toDelta
    , compare goalY currentY |> toDelta
    )


toDelta : Order -> Int
toDelta order =
    case order of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


moveTowardsResource : ResourceKind -> Dwarf -> World -> Generator (List Msg)
moveTowardsResource kind dwarf world =
    resourceClosestToDwarf kind dwarf world
        |> Maybe.map
            (\res ->
                [ dwarf.position
                    |> moveCloserTo res.position
                    |> MoveDwarf dwarf.id
                ]
            )
        |> Maybe.withDefault []
        |> Random.constant


dwarfClosestToDwarf : Dwarf -> World -> Maybe Dwarf
dwarfClosestToDwarf dwarf world =
    world.dwarves
        |> List.sortBy (\d -> distance d.position dwarf.position)
        |> List.filter (\d -> d.id /= dwarf.id)
        |> List.head


moveTowardsAndPunchDwarf : Dwarf -> World -> Generator (List Msg)
moveTowardsAndPunchDwarf dwarf world =
    dwarfClosestToDwarf dwarf world
        |> Maybe.map
            (\toBePunched ->
                if isNextTo toBePunched.position dwarf.position then
                    [ PunchDwarf dwarf.id toBePunched.id ]
                else
                    [ dwarf.position
                        |> moveCloserTo toBePunched.position
                        |> MoveDwarf dwarf.id
                    ]
            )
        |> Maybe.withDefault []
        |> Random.constant
