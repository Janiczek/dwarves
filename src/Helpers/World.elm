module Helpers.World exposing (..)

import List.Extra as List
import Types exposing (..)


findById : Id -> List (HasId a) -> Maybe (HasId a)
findById id entities =
    entities
        |> List.find (\e -> e.id == id)


removeDwarf : Dwarf -> List Dwarf -> List Dwarf
removeDwarf dwarf dwarves =
    dwarves
        |> List.filter (\d -> d.id /= dwarf.id)


removeResource : Resource -> List Resource -> List Resource
removeResource resource resources =
    resources
        |> List.filter (\r -> r.id /= resource.id)


asResourceIn : List Resource -> Resource -> List Resource
asResourceIn resources resource =
    resources
        |> List.replaceIf (\r -> r.id == resource.id) resource


asResourcesIn : World -> List Resource -> World
asResourcesIn world resources =
    { world | resources = resources }


asDwarfIn : List Dwarf -> Dwarf -> List Dwarf
asDwarfIn dwarves dwarf =
    dwarves
        |> List.replaceIf (\d -> d.id == dwarf.id) dwarf


asDwarvesIn : World -> List Dwarf -> World
asDwarvesIn world dwarves =
    { world | dwarves = dwarves }


decrementHitpoints : Int -> Dwarf -> Maybe Dwarf
decrementHitpoints amount dwarf =
    if dwarf.hitpoints <= amount then
        -- dies
        Nothing
    else
        Just { dwarf | hitpoints = dwarf.hitpoints - amount }
