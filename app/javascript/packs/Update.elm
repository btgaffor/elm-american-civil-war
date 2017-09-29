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
                                            { model | error = Just "Please select one of the yellow regions." } ! []

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
    { model | selectedRegion = Nothing, currentState = Idle } ! []


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


moveArmy : Model -> Int -> Int -> ( Model, Cmd Action )
moveArmy model oldIndex newIndex =
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
                case oldRegion.army of
                    Nothing ->
                        Stop (setError model "Trying to move an army that doesn't exist. Resetting state.")

                    Just oldArmy ->
                        if List.any (\unit -> unit.moves < 1) oldArmy.units then
                            Stop { model | error = Just "At least one unit in the army has no more movement points. Consider splitting the army." }
                        else
                            let
                                -- subtract one from the movement of all units moving
                                movedArmy =
                                    { oldArmy | units = (List.map (\unit -> { unit | moves = unit.moves - 1 }) oldArmy.units) }
                            in
                                Continue ( newRegion, oldRegion, movedArmy )
            )
        |> Chain.andThen
            (\( newRegion, oldRegion, movedArmy ) ->
                case newRegion.army of
                    Nothing ->
                        -- move the army to the empty region
                        Stop
                            ({ model
                                | selectedRegion = Just newIndex
                                , regions =
                                    model.regions
                                        |> Array.set oldIndex { oldRegion | army = Nothing }
                                        |> Array.set newIndex { newRegion | army = Just movedArmy }
                             }
                            )

                    Just newArmy ->
                        if newArmy.side == movedArmy.side then
                            -- join the armies
                            Stop
                                ({ model
                                    | selectedRegion = Just newIndex
                                    , regions =
                                        model.regions
                                            |> Array.set oldIndex { oldRegion | army = Nothing }
                                            |> Array.set newIndex { newRegion | army = (joinArmies movedArmy newArmy) }
                                 }
                                )
                        else
                            -- TODO
                            Stop { model | error = Just "Battles not implemented yet." }
            )
        |> (\chainModel -> Chain.flatten chainModel ! [])


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
