module Helpers.Dwarf exposing (..)

import Types exposing (..)


decrementHitpoints : Int -> Dwarf -> Maybe Dwarf
decrementHitpoints amount dwarf =
    if dwarf.hitpoints <= amount then
        -- dies
        Nothing
    else
        Just { dwarf | hitpoints = dwarf.hitpoints - amount }
