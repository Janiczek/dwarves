module View.Extra exposing (..)

import Constants.View exposing (..)


tilify : ( Int, Int ) -> ( Float, Float )
tilify ( x, y ) =
    ( toFloat (x * tileSize), toFloat (y * tileSize) )


tilifyBottom : ( Int, Int ) -> ( Float, Float )
tilifyBottom position =
    let
        ( pxX, pxY ) =
            tilify position
    in
        ( pxX, pxY - (toFloat tileSize) / 4 )


tilifyTop : ( Int, Int ) -> ( Float, Float )
tilifyTop position =
    let
        ( pxX, pxY ) =
            tilify position
    in
        ( pxX, pxY + (toFloat tileSize) / 4 )
