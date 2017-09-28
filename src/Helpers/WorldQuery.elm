module Helpers.WorldQuery exposing (..)

import Helpers.XY exposing (..)
import Types exposing (..)


resourceClosestToDwarf : ResourceKind -> Dwarf -> World -> Maybe Resource
resourceClosestToDwarf kind dwarf world =
    world.resources
        |> List.filter (\resource -> resource.kind == kind)
        |> List.sortBy (\resource -> distance resource.position dwarf.position)
        |> List.head


resourceTouchingDwarf : ResourceKind -> Dwarf -> World -> Maybe Resource
resourceTouchingDwarf kind dwarf world =
    world.resources
        |> List.filter (\resource -> resource.kind == kind)
        |> List.filter (\resource -> isNextTo resource.position dwarf.position)
        |> List.head


isDwarfNextTo : ResourceKind -> Dwarf -> World -> Bool
isDwarfNextTo kind dwarf world =
    resourceTouchingDwarf kind dwarf world /= Nothing


dwarfClosestToDwarf : Dwarf -> World -> Maybe Dwarf
dwarfClosestToDwarf dwarf world =
    world.dwarves
        |> List.sortBy (\d -> distance d.position dwarf.position)
        |> List.filter (\d -> d.id /= dwarf.id)
        |> List.head
