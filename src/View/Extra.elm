module View.Extra exposing (..)

import Collage as C exposing (Form)
import Constants.View exposing (..)
import Transform as T
import Types exposing (..)


tilify : XY -> ( Float, Float )
tilify ( x, y ) =
    ( toFloat (x * tileSize), toFloat (y * tileSize) )


tilifyBottom : XY -> ( Float, Float )
tilifyBottom position =
    let
        ( pxX, pxY ) =
            tilify position
    in
        ( pxX, pxY - (toFloat tileSize) / 4 )


tilifyTop : XY -> ( Float, Float )
tilifyTop position =
    let
        ( pxX, pxY ) =
            tilify position
    in
        ( pxX, pxY + (toFloat tileSize) / 4 )


fixCoordinateSpace : List (List Form) -> List Form
fixCoordinateSpace formLists =
    formLists
        |> List.concat
        |> C.groupTransform
            (T.translation
                (minusHalfWidth + toFloat halfTileSize)
                (minusHalfHeight + toFloat halfTileSize)
            )
        |> List.singleton
