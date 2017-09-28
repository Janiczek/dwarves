module Generator.Dwarf.Goals exposing (..)

import Random exposing (Generator)
import Random.Extra as Random
import Types exposing (..)
import Update.Extra exposing (..)


sleep : Generator (List Msg)
sleep =
    Random.constant []


moveTowardsFood : World -> Dwarf -> Generator (List Msg)
moveTowardsFood world dwarf =
    moveTowardsResource Food dwarf world


moveTowardsBeer : World -> Dwarf -> Generator (List Msg)
moveTowardsBeer world dwarf =
    moveTowardsResource Beer dwarf world


eatFood : World -> Dwarf -> Generator (List Msg)
eatFood world dwarf =
    decrementClosest Food dwarf world


drinkBeer : World -> Dwarf -> Generator (List Msg)
drinkBeer world dwarf =
    decrementClosest Beer dwarf world


attackClosestDwarf : World -> Dwarf -> Generator (List Msg)
attackClosestDwarf world dwarf =
    moveTowardsAndPunchDwarf dwarf world
