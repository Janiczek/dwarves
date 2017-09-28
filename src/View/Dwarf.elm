module View.Dwarf exposing (..)

import Collage as C exposing (Form)
import Color exposing (Color)
import Constants.Goal exposing (thresholds)
import Constants.View exposing (..)
import Text as T
import Types exposing (..)
import View.Extra exposing (..)


-- ALL DWARVES


viewDwarvesBg : List Dwarf -> List Form
viewDwarvesBg dwarves =
    dwarves
        |> List.map viewDwarfBg


viewDwarvesText : List Dwarf -> List Form
viewDwarvesText dwarves =
    List.concat
        [ dwarves |> List.map viewDwarfHp
        , dwarves |> List.map viewDwarfGoal
        ]



-- ONE DWARF


viewDwarfBg : Dwarf -> Form
viewDwarfBg { goal, position } =
    C.square (toFloat tileSize)
        |> C.filled (goalColor goal)
        |> C.move (tilify position)


goalColor : Goal -> Color
goalColor goal =
    -- TODO interpolate colors based on ticks?
    case goal of
        Idle _ ->
            Color.black

        Sleep _ ->
            Color.blue

        FindFood _ ->
            Color.darkPurple

        Eat _ ->
            Color.lightPurple

        FindBeer _ ->
            Color.darkGreen

        Drink _ ->
            Color.lightGreen

        Rage _ ->
            Color.red


viewDwarfHp : Dwarf -> Form
viewDwarfHp { position, hitpoints } =
    ("♥" |> String.repeat hitpoints)
        |> T.fromString
        |> T.color Color.white
        |> C.text
        |> C.move (tilifyBottom position)


viewDwarfGoal : Dwarf -> Form
viewDwarfGoal { position, goal } =
    goal
        |> goalToString
        |> T.fromString
        |> T.color Color.white
        |> C.text
        |> C.move (tilifyTop position)


goalToString : Goal -> String
goalToString goal =
    case goal of
        Idle { ticks } ->
            goalToStringHelper "Idle" ticks thresholds.idle

        Sleep { ticks } ->
            goalToStringHelper "Sleeping" ticks thresholds.sleep

        FindFood { ticks } ->
            goalToStringHelper "Hungry, seeks food" ticks thresholds.findFood

        FindBeer { ticks } ->
            goalToStringHelper "Thirsty, seeks beer" ticks thresholds.findBeer

        Eat { ticks } ->
            goalToStringHelper "Eating" ticks thresholds.eat

        Drink { ticks } ->
            goalToStringHelper "Drinking" ticks thresholds.drink

        Rage { ticks } ->
            goalToStringHelper "Raging (wants to punch somebody!)" ticks thresholds.rage


goalToStringHelper : String -> Int -> Int -> String
goalToStringHelper goalName ticks threshold =
    goalName ++ " (" ++ toString ticks ++ "/" ++ toString threshold ++ ")"
