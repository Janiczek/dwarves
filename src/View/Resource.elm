module View.Resource exposing (..)

import Collage as C exposing (Form)
import Color exposing (Color)
import Constants.View exposing (..)
import Text as T
import Types exposing (..)
import View.Extra exposing (..)


-- ALL RESOURCES


viewResourcesBg : List Resource -> List Form
viewResourcesBg resources =
    resources |> List.map viewResourceBg


viewResourcesText : List Resource -> List Form
viewResourcesText resources =
    [ resources |> List.map viewResourceKind
    , resources |> List.map viewResourceAmount
    ]
        |> List.concat



-- ONE RESOURCE


viewResourceKind : Resource -> Form
viewResourceKind { kind, position } =
    toString kind
        |> T.fromString
        |> T.color Color.black
        |> C.text
        |> C.move (tilifyTop position)


viewResourceAmount : Resource -> Form
viewResourceAmount { amount, position } =
    (toString amount ++ "x")
        |> T.fromString
        |> T.color Color.black
        |> C.text
        |> C.move (tilifyBottom position)


viewResourceBg : Resource -> Form
viewResourceBg { kind, position } =
    C.circle circleSize
        |> C.filled (resourceColor kind)
        |> C.move (tilify position)


resourceColor : ResourceKind -> Color
resourceColor kind =
    case kind of
        Beer ->
            Color.yellow

        Food ->
            Color.orange
