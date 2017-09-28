module Main exposing (main)

import Html
import Types exposing (..)
import Update exposing (..)
import View exposing (..)


main : Program Never World Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
