module Generator.Resource exposing (resource)

import Generator.Common exposing (..)
import Random exposing (Generator)
import Random.Extra as Random
import Types exposing (..)


resource : Generator (Id -> Resource)
resource =
    Random.map3 Resource
        position
        resourceKind
        resourceAmount



-- COMPONENTS


resourceKind : Generator ResourceKind
resourceKind =
    [ Beer, Food ]
        |> List.map Random.constant
        |> Random.choices


resourceAmount : Generator Int
resourceAmount =
    Random.int 1 10
