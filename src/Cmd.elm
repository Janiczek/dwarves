module Cmd exposing (genInitData)

import Generator.Dwarf as Generator
import Generator.Resource as Generator
import Random exposing (Generator)
import Types exposing (..)


genInitData : { dwarves : Int, resources : Int } -> Cmd Msg
genInitData counts =
    let
        dwarves =
            Generator.dwarf
                |> Random.list counts.dwarves

        resources =
            Generator.resource
                |> Random.list counts.resources
    in
        Random.map2 (,) dwarves resources
            |> Random.generate GeneratedInitData
