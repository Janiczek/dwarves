module Helpers.XY exposing (..)

import Constants.World exposing (..)
import Types exposing (..)


moveByDelta : XYDelta -> HasPosition a -> HasPosition a
moveByDelta ( dx, dy ) thing =
    let
        ( x, y ) =
            thing.position
    in
        { thing | position = ( x + dx, y + dy ) |> clampPosition }


distance : XY -> XY -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
        |> toFloat
        |> sqrt


isNextTo : XY -> XY -> Bool
isNextTo ( x1, y1 ) ( x2, y2 ) =
    (abs (x1 - x2) < 2)
        && (abs (y1 - y2) < 2)


clampPosition : XY -> XY
clampPosition ( x, y ) =
    ( clamp 0 (worldWidth - 1) x
    , clamp 0 (worldHeight - 1) y
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


deltaMovingCloserTo : XY -> XY -> XYDelta
deltaMovingCloserTo ( goalX, goalY ) ( currentX, currentY ) =
    ( compare goalX currentX |> toDelta
    , compare goalY currentY |> toDelta
    )


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
