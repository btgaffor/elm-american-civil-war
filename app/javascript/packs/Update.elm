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
                            setError model "No region selected, resetting state." ! []

                        Just selectedRegionIndex ->
                            if selectedRegionIndex == clickedIndex then
                                deselectRegion model
                            else
                                case (Array.get selectedRegionIndex model.regions) of
                                    Nothing ->
                                        setError model "Selected region does not exist, resetting state." ! []

                                    Just selectedRegion ->
                                        if (listContains selectedRegion.connections clickedIndex) then
                                            moveArmy model selectedRegionIndex clickedIndex
                                        else
                                            ( { model | error = Just "Please select one of the yellow regions." }, Cmd.none )

        AddUnit addUnitAction ->
            addUnit model addUnitAction

        EndTurn ->
            let
                -- reset the movement points of all units
                regions =
                    Array.map
                        (\region ->
                            case region.army of
                                Nothing ->
                                    region

                                Just army ->
                                    { region
                                        | army =
                                            Just { army | units = List.map (\unit -> resetMoves unit) army.units }
                                    }
                        )
                        model.regions
            in
                case model.turn of
                    Union ->
                        { model | turn = Confederate, regions = regions } ! []

                    Confederate ->
                        { model | turn = Union, regions = regions } ! []


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
                case oldRegion.army of
                    Nothing ->
                        setError model "Trying to move an army that doesn't exist. Resetting state."

                    Just oldArmy ->
                        if List.all (\unit -> unit.moves >= 1) oldArmy.units then
                            let
                                -- subtract one from the movement of all units moving
                                movedArmy =
                                    Just { oldArmy | units = (List.map (\unit -> { unit | moves = unit.moves - 1 }) oldArmy.units) }
                            in
                                case newRegion.army of
                                    Nothing ->
                                        -- move the army to the empty region
                                        { model
                                            | selectedRegion = Just newIndex
                                            , regions =
                                                model.regions
                                                    |> Array.set oldIndex { oldRegion | army = Nothing }
                                                    |> Array.set newIndex { newRegion | army = movedArmy }
                                        }

                                    Just newArmy ->
                                        if newArmy.side == oldArmy.side then
                                            -- join the armies
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
                        else
                            { model | error = Just "At least one unit in the army has no more movement points. Consider splitting the army." }
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
            setError model error ! []


joinArmies : Army -> Army -> Maybe Army
joinArmies oldArmy newArmy =
    Just { oldArmy | units = (List.append oldArmy.units newArmy.units) }


resetMoves : Unit -> Unit
resetMoves unit =
    case unit.unitType of
        Infantry ->
            { unit | moves = 1 }

        Artillary ->
            { unit | moves = 1 }

        Cavalry ->
            { unit | moves = 2 }

        EliteCavalry ->
            { unit | moves = 2 }

        Leader ->
            { unit | moves = 2 }


setError : Model -> String -> Model
setError model message =
    { model
        | error = Just message
        , selectedRegion = Nothing
        , currentState = Idle
    }


init : ( Model, Cmd Action )
init =
    ( model, Task.perform WindowResize Window.size )
