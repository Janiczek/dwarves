module Update.Goal exposing (nextGoal)

import Constants.Goal exposing (thresholds)
import Helpers.WorldQuery exposing (..)
import Random exposing (Generator)
import Random.Extra as Random
import Types exposing (..)


nextGoal : Dwarf -> World -> Generator Goal
nextGoal dwarf world =
    case dwarf.goal of
        Sleep { ticks } ->
            sleep ticks

        FindFood { ticks } ->
            findFood ticks dwarf world

        FindBeer { ticks } ->
            findBeer ticks dwarf world

        Eat { ticks } ->
            eat ticks

        Drink { ticks } ->
            drink ticks

        Rage { ticks } ->
            rage ticks

        Idle { ticks } ->
            idle ticks



-- CASES


sleep : Int -> Generator Goal
sleep ticks =
    continueWithPossibilityOfChange
        Sleep
        ticks
        thresholds.sleep
        [ ( 1, Idle ) ]


findFood : Int -> Dwarf -> World -> Generator Goal
findFood ticks dwarf world =
    if ticks == thresholds.findFood then
        Random.constant (Rage initDuration)
    else if isDwarfNextTo Food dwarf world then
        Random.constant (Eat initDuration)
    else
        incrementTicks FindFood ticks


findBeer : Int -> Dwarf -> World -> Generator Goal
findBeer ticks dwarf world =
    if ticks == thresholds.findBeer then
        Random.constant (Rage initDuration)
    else if isDwarfNextTo Beer dwarf world then
        Random.constant (Drink initDuration)
    else
        incrementTicks FindBeer ticks


eat : Int -> Generator Goal
eat ticks =
    continueWithPossibilityOfChange
        Eat
        ticks
        thresholds.eat
        [ ( 5, Idle )
        , ( 4, Sleep )
        , ( 1, FindBeer )
        ]


drink : Int -> Generator Goal
drink ticks =
    continueWithPossibilityOfChange
        Drink
        ticks
        thresholds.drink
        [ ( 5, Idle )
        , ( 4, Sleep )
        , ( 1, FindFood )
        ]


rage : Int -> Generator Goal
rage ticks =
    continueWithPossibilityOfChange
        Rage
        ticks
        thresholds.rage
        [ ( 5, Sleep )
        , ( 2, Idle )
        ]


idle : Int -> Generator Goal
idle ticks =
    continueWithPossibilityOfChange
        Idle
        ticks
        thresholds.idle
        [ ( 1, Sleep )
        , ( 1, FindFood )
        , ( 1, FindBeer )
        ]



-- HELPERS


incrementTicks : (Duration -> Goal) -> Int -> Generator Goal
incrementTicks goal ticks =
    goal { ticks = ticks + 1 }
        |> Random.constant


initDuration : Duration
initDuration =
    { ticks = 0 }


continueWithPossibilityOfChange :
    (Duration -> Goal)
    -> Int
    -> Int
    -> List ( Float, Duration -> Goal )
    -> Generator Goal
continueWithPossibilityOfChange oldState ticks threshold newStates =
    Random.float 0 1
        |> Random.andThen
            (\p ->
                if aboveThreshold p ticks threshold then
                    newStates
                        |> List.map
                            (\( freq, goal ) ->
                                ( freq, goal initDuration |> Random.constant )
                            )
                        |> Random.frequency
                else
                    incrementTicks oldState ticks
            )


aboveThreshold : Float -> Int -> Int -> Bool
aboveThreshold p ticks threshold =
    p < ((toFloat ticks + 1) / (toFloat threshold)) ^ 4
