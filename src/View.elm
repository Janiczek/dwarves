module View exposing (view)

import Collage as C exposing (Form)
import Color exposing (Color)
import Constants.View exposing (..)
import Element as E exposing (Element)
import Html as H exposing (Html)
import Types exposing (..)
import View.Dwarf exposing (..)
import View.Extra exposing (..)
import View.Resource exposing (..)


view : World -> Html Msg
view world =
    viewGame world
        |> C.collage worldWidthPx worldHeightPx
        |> E.toHtml


viewGame : World -> List Form
viewGame world =
    viewBg :: viewEntities world


viewBg : Form
viewBg =
    C.rect (toFloat worldWidthPx) (toFloat worldHeightPx)
        |> C.filled Color.lightBlue


viewEntities : World -> List Form
viewEntities world =
    [ viewDwarvesBg world.dwarves
    , viewResourcesBg world.resources
    , viewResourcesText world.resources
    , viewDwarvesText world.dwarves
    ]
        |> fixCoordinateSpace
