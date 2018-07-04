module Update exposing (..)

import Array
import Set
import Maybe exposing (andThen)
import Window exposing (Size)
import Task
import Model exposing (..)
import List.Extra


type Action
    = MouseMove Int Int
    | WindowResize Size
    | ClearError
    | ClickRegion ( Int, Region )
    | SplitArmy
    | AddUnitToSplit Unit
    | RemoveUnitFromSplit Unit
    | AddUnit AddUnitAction
    | EndTurn
    | Reset


type AddUnitAction
    = Start
    | ChooseUnitSide Side
    | ChooseUnitType Side UnitType
    | Finish



-- UPDATE


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        -- system related things
        MouseMove left top ->
            model |> updateBrowser (\b -> { b | mousex = left, mousey = top }) |> noCmd

        WindowResize size ->
            model |> updateBrowser (\b -> { b | windowSize = size }) |> noCmd

        -- error dialog
        ClearError ->
            { model | error = Nothing } |> noCmd

        -- moving armies around the board
        ClickRegion ( clickedIndex, clickedRegion ) ->
            case model.currentState of
                Idle ->
                    selectRegion model ( clickedIndex, clickedRegion ) |> noCmd

                MovingArmy ( selectedRegionIndex, selectedRegion ) army ->
                    if selectedRegionIndex == clickedIndex then
                        deselectRegion ( selectedRegionIndex, selectedRegion ) army model |> noCmd
                    else if not <| (Set.member clickedIndex selectedRegion.connections) then
                        model |> noCmd
                    else
                        moveArmy army selectedRegion ( clickedIndex, clickedRegion ) model |> noCmd

                SplittingArmy ( selectedRegionIndex, selectedRegion ) oldArmy newArmy ->
                    if selectedRegionIndex == clickedIndex then
                        -- cancel the splitting and put both armies back
                        deselectRegion ( selectedRegionIndex, selectedRegion ) (joinArmies oldArmy newArmy) model |> noCmd
                    else if (Set.member clickedIndex selectedRegion.connections) then
                        -- put the old army back and move the new army
                        model
                            |> deselectRegion ( selectedRegionIndex, selectedRegion ) oldArmy
                            |> moveArmy newArmy selectedRegion ( clickedIndex, clickedRegion )
                            |> noCmd
                    else
                        model |> noCmd

                AddingUnit addingUnitState ->
                    -- TODO
                    model |> noCmd

                Combat _ ->
                    -- TODO
                    model |> noCmd

        -- army splits
        SplitArmy ->
            case model.currentState of
                MovingArmy selectedRegion army ->
                    { model
                        | currentState = (SplittingArmy selectedRegion army { side = model.turn, units = [] })
                    }
                        |> noCmd

                _ ->
                    model |> noCmd

        AddUnitToSplit movingUnit ->
            case model.currentState of
                SplittingArmy selectedRegion oldArmy newArmy ->
                    let
                        modifiedOldArmy =
                            { oldArmy | units = List.Extra.remove movingUnit oldArmy.units }

                        modifiedNewArmy =
                            { newArmy | units = movingUnit :: newArmy.units }
                    in
                        { model | currentState = SplittingArmy selectedRegion modifiedOldArmy modifiedNewArmy } |> noCmd

                _ ->
                    model |> noCmd

        RemoveUnitFromSplit movingUnit ->
            case model.currentState of
                SplittingArmy selectedRegion oldArmy newArmy ->
                    let
                        modifiedOldArmy =
                            { oldArmy | units = movingUnit :: oldArmy.units }

                        modifiedNewArmy =
                            { newArmy | units = List.Extra.remove movingUnit newArmy.units }
                    in
                        { model | currentState = SplittingArmy selectedRegion modifiedOldArmy modifiedNewArmy } |> noCmd

                _ ->
                    model |> noCmd

        -- putting new units on the board
        AddUnit addUnitAction ->
            addUnit model addUnitAction |> noCmd

        -- game logic
        EndTurn ->
            let
                -- reset the movement points of all units
                regions =
                    model.regions
                        |> Array.map
                            (\region ->
                                { region
                                    | army = region.army |> Maybe.map (\army -> { army | units = List.map resetMoves army.units })
                                }
                            )
            in
                case model.turn of
                    Union ->
                        { model | turn = Confederate, regions = regions } |> noCmd

                    Confederate ->
                        { model | turn = Union, regions = regions } |> noCmd

        Reset ->
            { model | currentState = Idle } |> noCmd


selectRegion : Model -> ( Int, Region ) -> Model
selectRegion model ( clickedIndex, clickedRegion ) =
    case clickedRegion.army of
        Just army ->
            if army.side == model.turn then
                { model
                    | regions = model.regions |> Array.set clickedIndex { clickedRegion | army = Nothing }
                    , currentState = MovingArmy ( clickedIndex, clickedRegion ) army
                }
            else
                model

        Nothing ->
            model


deselectRegion : ( Int, Region ) -> Army -> Model -> Model
deselectRegion ( selectedRegionIndex, selectedRegion ) army model =
    { model
        | currentState = Idle
        , regions = model.regions |> Array.set selectedRegionIndex { selectedRegion | army = Just army }
    }


addUnit : Model -> AddUnitAction -> Model
addUnit model addUnitAction =
    case addUnitAction of
        Start ->
            { model | currentState = AddingUnit ChoosingUnitSide }

        ChooseUnitSide side ->
            { model | currentState = AddingUnit (ChoosingUnitType side) }

        ChooseUnitType side unitType ->
            { model | currentState = AddingUnit (PlacingUnit side unitType) }

        Finish ->
            { model | currentState = Idle }


moveArmy : Army -> Region -> ( Int, Region ) -> Model -> Model
moveArmy movingArmy oldRegion ( newIndex, newRegion ) model =
    checkMovePoints model movingArmy newRegion
        |> Result.andThen (moveIfEmpty model newIndex)
        |> Result.andThen (joinOrBattle model newIndex)
        |> flattenResult


checkMovePoints : Model -> Army -> Region -> Result Model ( Region, Army )
checkMovePoints model movingArmy newRegion =
    if List.any (\unit -> unit.moves < 1) movingArmy.units then
        Err { model | error = Just "At least one unit in the army has no more movement points. Consider splitting the army." }
    else
        let
            -- subtract one from the movement of all units moving
            movedArmy =
                { movingArmy | units = movingArmy.units |> List.map (\unit -> { unit | moves = unit.moves - 1 }) }
        in
            Ok ( newRegion, movedArmy )


moveIfEmpty : Model -> Int -> ( Region, Army ) -> Result Model ( Region, Army, Army )
moveIfEmpty model newIndex ( newRegion, movedArmy ) =
    case newRegion.army of
        Nothing ->
            -- move the army into the empty region
            Err
                ({ model
                    | currentState = Idle
                    , regions = model.regions |> Array.set newIndex { newRegion | army = Just movedArmy }
                 }
                )

        Just newArmy ->
            Ok ( newRegion, movedArmy, newArmy )


joinOrBattle : Model -> Int -> ( Region, Army, Army ) -> Result Model Model
joinOrBattle model newIndex ( newRegion, movedArmy, newArmy ) =
    if newArmy.side == movedArmy.side then
        -- join the armies
        Ok
            { model
                | currentState = Idle
                , regions = model.regions |> Array.set newIndex { newRegion | army = Just (joinArmies movedArmy newArmy) }
            }
    else
        -- start a battle
        Ok
            { model
                | combatBoard = Just (majorBoard model.turn)
                , currentState = Combat { attackingArmy = movedArmy, defendingArmy = newArmy }
            }


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
        , currentState = Idle
    }


init : ( Model, Cmd Action )
init =
    ( initialModel, Task.perform WindowResize Window.size )



-- utility


noCmd : Model -> ( Model, Cmd Action )
noCmd model =
    ( model, Cmd.none )


{-| nested update for browser
-}
updateBrowser : (Browser -> Browser) -> Model -> Model
updateBrowser fn model =
    { model | browser = fn model.browser }


flattenResult : Result value value -> value
flattenResult result =
    case result of
        Ok value ->
            value

        Err value ->
            value
