module Types exposing (..)


type alias World =
    { dwarves : List Dwarf
    , resources : List Resource
    , nextId : Int
    }


type alias Id =
    Int


type alias Dwarf =
    { position : XY
    , goal : Goal
    , hitpoints : Int
    , id : Id
    }


type alias Resource =
    { position : XY
    , kind : ResourceKind
    , amount : Int
    , id : Id
    }


type alias HasPosition a =
    { a | position : XY }


type alias XY =
    ( Int, Int )


type Goal
    = Sleep Duration
    | FindFood Duration
    | Eat Duration
    | FindBeer Duration
    | Drink Duration
    | Rage Duration
    | Idle Duration


type alias Duration =
    { ticks : Int }


type ResourceKind
    = Beer
    | Food


type Msg
    = GeneratedInitData ( List (Int -> Dwarf), List (Int -> Resource) )
    | Tick
    | UpdateDwarf ( Dwarf, List Msg, Goal )
      -- "world changes"
    | MoveDwarf Id XY
    | PunchDwarf Id Id
    | DecrementResource Id Int
    | IncrementResource Id Int
