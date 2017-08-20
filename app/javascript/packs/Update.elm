module Update exposing (..)

import Array
import Maybe exposing (andThen)
import Window exposing (Size)
import Task
import Model exposing (..)
import Utils exposing (..)


type Action
    = MouseMove Int Int
    | WindowResize Size
    | ClearError
    | ClickRegion Int
    | AddUnit AddUnitAction
    | EndTurn


type AddUnitAction
    = Start
    | ChooseUnitSide Side
    | ChooseUnitType Side UnitType
    | Finish



-- UPDATE


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        MouseMove left top ->
            ( { model | mousex = left, mousey = top }, Cmd.none )

        WindowResize size ->
            ( { model | windowSize = size }, Cmd.none )

        ClearError ->
            ( { model | error = Nothing }, Cmd.none )

        ClickRegion clickedIndex ->
            case model.currentState of
                AddingUnit addingUnitState ->
                    ( model, Cmd.none )

                Idle ->
                    selectRegion model clickedIndex

                MovingArmy ->
                    case model.selectedRegion of
                        Nothing ->
                            setError model "No region selected, resetting state."

                        Just selectedRegionIndex ->
                            if selectedRegionIndex == clickedIndex then
                                deselectRegion model
                            else
                                case (Array.get selectedRegionIndex model.regions) of
                                    Nothing ->
                                        setError model "Selected region does not exist, resetting state."

                                    Just selectedRegion ->
                                        if (listContains selectedRegion.connections clickedIndex) then
                                            moveArmy model selectedRegionIndex clickedIndex
                                        else
                                            ( { model | error = Just "Please select one of the yellow regions." }, Cmd.none )

        AddUnit addUnitAction ->
            addUnit model addUnitAction

        EndTurn ->
            case model.turn of
                Union ->
                    { model | turn = Confederate } ! []

                Confederate ->
                    { model | turn = Union } ! []


selectRegion : Model -> Int -> ( Model, Cmd Action )
selectRegion model clickedIndex =
    case Array.get clickedIndex model.regions |> andThen .army of
        Nothing ->
            model ! []

        Just army ->
            if army.side == model.turn then
                { model | selectedRegion = Just clickedIndex, currentState = MovingArmy } ! []
            else
                model ! []


deselectRegion : Model -> ( Model, Cmd Action )
deselectRegion model =
    ( { model | selectedRegion = Nothing, currentState = Idle }, Cmd.none )


addUnit : Model -> AddUnitAction -> ( Model, Cmd Action )
addUnit model addUnitAction =
    case addUnitAction of
        Start ->
            ( { model | currentState = AddingUnit ChoosingUnitSide }, Cmd.none )

        ChooseUnitSide side ->
            ( { model | currentState = AddingUnit (ChoosingUnitType side) }, Cmd.none )

        ChooseUnitType side unitType ->
            ( { model | currentState = AddingUnit (PlacingUnit side unitType) }, Cmd.none )

        Finish ->
            ( { model | currentState = Idle }, Cmd.none )


moveArmy : Model -> Int -> Int -> ( Model, Cmd Action )
moveArmy model oldIndex newIndex =
    case
        Result.map2
            (\newRegion oldRegion ->
                Maybe.withDefault
                    -- move the army from the old region to the new one.
                    -- it doesn't matter if the old region had no army, since it'll
                    -- just be nothing in the new region anyway
                    { model
                        | selectedRegion = Just newIndex
                        , regions =
                            model.regions
                                |> Array.set oldIndex { oldRegion | army = Nothing }
                                |> Array.set newIndex { newRegion | army = oldRegion.army }
                    }
                    (Maybe.map2
                        (\newArmy oldArmy ->
                            -- if the armies are the same, join them in the new region
                            if newArmy.side == oldArmy.side then
                                { model
                                    | selectedRegion = Just newIndex
                                    , regions =
                                        model.regions
                                            |> Array.set oldIndex { oldRegion | army = Nothing }
                                            |> Array.set newIndex { newRegion | army = (joinArmies oldArmy newArmy) }
                                }
                            else
                                -- TODO
                                { model | error = Just "Battles not implemented yet." }
                        )
                        newRegion.army
                        oldRegion.army
                    )
            )
            (Array.get newIndex model.regions
                |> Result.fromMaybe "Destination region does not exist. Resetting state."
            )
            (Array.get oldIndex model.regions
                |> Result.fromMaybe "Source region does not exist. Resetting state."
            )
    of
        Ok newModel ->
            ( newModel, Cmd.none )

        Err error ->
            setError model error


joinArmies : Army -> Army -> Maybe Army
joinArmies oldArmy newArmy =
    Just { oldArmy | units = (List.append oldArmy.units newArmy.units) }


setError : Model -> String -> ( Model, Cmd Action )
setError model message =
    ( { model
        | error = Just message
        , selectedRegion = Nothing
        , currentState = Idle
      }
    , Cmd.none
    )


init : ( Model, Cmd Action )
init =
    ( model, Task.perform WindowResize Window.size )
