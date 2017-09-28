module Constants.Goal exposing (..)

import Types exposing (..)


thresholds :
    { drink : Int
    , findFood : Int
    , findBeer : Int
    , eat : Int
    , idle : Int
    , rage : Int
    , sleep : Int
    }
thresholds =
    { sleep = 24
    , findFood = 24
    , findBeer = 30
    , eat = 12
    , drink = 8
    , rage = 12
    , idle = 24
    }


initialGoals : List ( Float, Duration -> Goal )
initialGoals =
    [ ( 10, Sleep )
    , ( 10, Idle )
    , ( 3, FindFood )
    , ( 2, FindBeer )
    ]
