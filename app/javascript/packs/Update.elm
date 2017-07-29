module Update exposing (..)

import Array
import Maybe exposing (andThen)
import Model exposing (..)

type Action
  = MouseMove Int Int
  | ClickRegion Int
  | AddUnit AddUnitAction

type AddUnitAction = Start
             | ChooseUnitSide Side
             | ChooseUnitType Side UnitType
             | Finish

-- UPDATE

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    MouseMove left top -> ({ model | mousex = left, mousey = top }, Cmd.none)
    ClickRegion clickedIndex ->
      case model.currentState of
        AddingUnit addingUnitState -> (model, Cmd.none)
        Idle ->
          let
            army = Array.get clickedIndex model.regions |> andThen .army
          in
            case army of
              Nothing -> (model, Cmd.none)
              Just army ->
                ({ model | selectedRegion = Just clickedIndex, currentState = MovingArmy }, Cmd.none)
        MovingArmy ->
          case model.selectedRegion of
            Nothing -> ({ model | selectedRegion = Nothing, currentState = Idle }, Cmd.none)
            Just selectedRegionIndex ->
              if selectedRegionIndex == clickedIndex then
                -- deselect army
                ({ model | selectedRegion = Nothing, currentState = Idle }, Cmd.none)
              else
                case (Array.get selectedRegionIndex model.regions) of
                  Nothing -> (model, Cmd.none)
                  Just selectedRegion ->
                    if (List.any (\i -> i == clickedIndex) selectedRegion.connections) then
                      ({
                        model |
                          selectedRegion = Just clickedIndex,
                          regions =
                            (moveArmy model.regions selectedRegion.army selectedRegionIndex clickedIndex)
                      }, Cmd.none)
                    else
                      (model, Cmd.none)
    AddUnit addUnitAction ->
      case addUnitAction of
        Start ->
          ({ model | currentState = AddingUnit ChoosingUnitSide }, Cmd.none)
        ChooseUnitSide side ->
          ({ model | currentState = AddingUnit (ChoosingUnitType side) }, Cmd.none)
        ChooseUnitType side unitType ->
          ({ model | currentState = AddingUnit (PlacingUnit side unitType) }, Cmd.none)
        Finish ->
          ({ model | currentState = Idle }, Cmd.none)

moveArmy : Array.Array (Region) -> Maybe Army -> Int -> Int -> Array.Array (Region)
moveArmy regions army oldIndex newIndex =
  Array.indexedMap
    (\mapIndex mapRegion ->
      if mapIndex == oldIndex then
        { mapRegion | army = Nothing }
      else if mapIndex == newIndex then
        { mapRegion | army = army }
      else
        mapRegion
    )
    regions


init : (Model, Cmd Action)
init =
  (model, Cmd.none)
