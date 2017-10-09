module Update exposing (..)

import Array
import Maybe exposing (andThen)
import Window exposing (Size)
import Task
import Model exposing (..)
import Utils exposing (listContains)
import Chain exposing (Chain(..))


type Action
    = MouseMove Int Int
    | WindowResize Size
    | ClearError
    | ClickRegion Int
    | SplitArmy
    | AddUnitToSplit Unit
    | RemoveUnitFromSplit Unit
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
            { model | mousex = left, mousey = top } ! []

        WindowResize size ->
            { model | windowSize = size } ! []

        ClearError ->
            { model | error = Nothing } ! []

        ClickRegion clickedIndex ->
            case model.currentState of
                AddingUnit addingUnitState ->
                    model ! []

                SplittingArmy oldArmy newArmy ->
                    case model.selectedRegion of
                        Nothing ->
                            setError model "No region selected, cannot return armies." ! []

                        Just selectedRegionIndex ->
                            case (Array.get selectedRegionIndex model.regions) of
                                Nothing ->
                                    setError model "Selected region does not exist, cannot return armies." ! []

                                Just region ->
                                    if selectedRegionIndex == clickedIndex then
                                        -- cancel the splitting and put both armies back
                                        deselectRegion (joinArmies oldArmy newArmy) model
                                    else if (listContains region.connections clickedIndex) then
                                        -- put the old army back and move the new army
                                        model
                                            |> deselectRegion oldArmy
                                            |> Tuple.first
                                            |> moveArmy newArmy selectedRegionIndex clickedIndex
                                    else
                                        model ! []

                Idle ->
                    selectRegion model clickedIndex

                MovingArmy army ->
                    case model.selectedRegion of
                        Nothing ->
                            setError model "No region selected, resetting state." ! []

                        Just selectedRegionIndex ->
                            if selectedRegionIndex == clickedIndex then
                                deselectRegion army model
                            else
                                case (Array.get selectedRegionIndex model.regions) of
                                    Nothing ->
                                        setError model "Selected region does not exist, resetting state." ! []

                                    Just selectedRegion ->
                                        if (listContains selectedRegion.connections clickedIndex) then
                                            moveArmy army selectedRegionIndex clickedIndex model
                                        else
                                            { model | error = Just "Please select one of the highlighted regions." } ! []

        SplitArmy ->
            case model.currentState of
                MovingArmy army ->
                    { model | currentState = (SplittingArmy army { side = model.turn, units = [] }) } ! []

                _ ->
                    model ! []

        AddUnitToSplit movingUnit ->
            case model.currentState of
                SplittingArmy oldArmy newArmy ->
                    let
                        modifiedNewArmy =
                            { newArmy | units = movingUnit :: newArmy.units }

                        modifiedOldArmy =
                            { oldArmy
                                | units = List.filter ((/=) movingUnit) oldArmy.units
                            }
                    in
                        { model | currentState = SplittingArmy modifiedOldArmy modifiedNewArmy } ! []

                _ ->
                    model ! []

        RemoveUnitFromSplit movingUnit ->
            case model.currentState of
                SplittingArmy oldArmy newArmy ->
                    let
                        modifiedNewArmy =
                            { newArmy
                                | units = List.filter ((/=) movingUnit) newArmy.units
                            }

                        modifiedOldArmy =
                            { oldArmy | units = movingUnit :: oldArmy.units }
                    in
                        { model | currentState = SplittingArmy modifiedOldArmy modifiedNewArmy } ! []

                _ ->
                    model ! []

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
    case Array.get clickedIndex model.regions of
        Nothing ->
            model ! []

        Just clickedRegion ->
            case clickedRegion.army of
                Nothing ->
                    model ! []

                Just army ->
                    if army.side == model.turn then
                        { model
                            | selectedRegion = Just clickedIndex
                            , regions =
                                model.regions
                                    |> Array.set clickedIndex { clickedRegion | army = Nothing }
                            , currentState = MovingArmy army
                        }
                            ! []
                    else
                        model ! []


deselectRegion : Army -> Model -> ( Model, Cmd Action )
deselectRegion army model =
    case model.selectedRegion of
        Nothing ->
            { model | currentState = Idle } ! []

        Just regionIndex ->
            case Array.get regionIndex model.regions of
                Nothing ->
                    { model | selectedRegion = Nothing, currentState = Idle } ! []

                Just region ->
                    { model
                        | selectedRegion = Nothing
                        , currentState = Idle
                        , regions = model.regions |> Array.set regionIndex { region | army = Just army }
                    }
                        ! []


addUnit : Model -> AddUnitAction -> ( Model, Cmd Action )
addUnit model addUnitAction =
    case addUnitAction of
        Start ->
            { model | currentState = AddingUnit ChoosingUnitSide } ! []

        ChooseUnitSide side ->
            { model | currentState = AddingUnit (ChoosingUnitType side) } ! []

        ChooseUnitType side unitType ->
            { model | currentState = AddingUnit (PlacingUnit side unitType) } ! []

        Finish ->
            { model | currentState = Idle } ! []


moveArmy : Army -> Int -> Int -> Model -> ( Model, Cmd Action )
moveArmy movingArmy oldIndex newIndex model =
    (case (Array.get newIndex model.regions) of
        Nothing ->
            Stop (setError model "Destination region does not exist. Resetting state.")

        Just newRegion ->
            Continue newRegion
    )
        |> Chain.andThen
            (\newRegion ->
                case (Array.get oldIndex model.regions) of
                    Nothing ->
                        Stop (setError model "Source region does not exist. Resetting state.")

                    Just oldRegion ->
                        Continue ( newRegion, oldRegion )
            )
        |> Chain.andThen
            (\( newRegion, oldRegion ) ->
                if List.any (\unit -> unit.moves < 1) movingArmy.units then
                    Stop { model | error = Just "At least one unit in the army has no more movement points. Consider splitting the army." }
                else
                    let
                        -- subtract one from the movement of all units moving
                        movedArmy =
                            { movingArmy | units = (List.map (\unit -> { unit | moves = unit.moves - 1 }) movingArmy.units) }
                    in
                        Continue ( newRegion, oldRegion, movedArmy )
            )
        |> Chain.andThen
            (\( newRegion, oldRegion, movedArmy ) ->
                case newRegion.army of
                    Nothing ->
                        Stop
                            { model
                                | selectedRegion = Just newIndex
                                , currentState = MovingArmy movedArmy
                            }

                    Just newArmy ->
                        if newArmy.side == movedArmy.side then
                            -- join the armies
                            Stop
                                ({ model
                                    | selectedRegion = Just newIndex
                                    , currentState = MovingArmy (joinArmies movedArmy newArmy)
                                    , regions = model.regions |> Array.set newIndex { newRegion | army = Nothing }
                                 }
                                )
                        else
                            -- TODO
                            Stop { model | error = Just "Battles not implemented yet." }
            )
        |> (\chainModel -> Chain.flatten chainModel ! [])


joinArmies : Army -> Army -> Army
joinArmies oldArmy newArmy =
    { oldArmy | units = (List.append oldArmy.units newArmy.units) }


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
