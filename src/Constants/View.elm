module Constants.View exposing (..)

import Constants.World exposing (..)


tileSize : Int
tileSize =
    32


halfTileSize : Int
halfTileSize =
    tileSize // 2


worldWidthPx : Int
worldWidthPx =
    worldWidth * tileSize


worldHeightPx : Int
worldHeightPx =
    worldHeight * tileSize


minusHalfWidth : Float
minusHalfWidth =
    toFloat (negate worldWidthPx) / 2


minusHalfHeight : Float
minusHalfHeight =
    toFloat (negate worldHeightPx) / 2


circleSize : Float
circleSize =
    toFloat tileSize / 2
