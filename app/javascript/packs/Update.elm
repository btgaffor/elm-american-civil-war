module Update exposing (..)

import Array
import Maybe exposing (andThen)
import Window exposing (Size)
import Task
import Model exposing (..)
import List.Extra


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
    | EndCombat
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
        ClickRegion clickedIndex ->
            case model.currentState of
                Idle ->
                    selectRegion model clickedIndex |> noCmd

                MovingArmy selectedRegionIndex army ->
                    if selectedRegionIndex == clickedIndex then
                        deselectRegion selectedRegionIndex army model |> noCmd
                    else
                        moveArmy army selectedRegionIndex clickedIndex model |> noCmd

                SplittingArmy selectedRegionIndex oldArmy newArmy ->
                    if selectedRegionIndex == clickedIndex then
                        -- cancel the splitting and put both armies back
                        deselectRegion selectedRegionIndex (joinArmies oldArmy newArmy) model |> noCmd
                    else
                        model
                            |> deselectRegion selectedRegionIndex oldArmy
                            |> moveArmy newArmy selectedRegionIndex clickedIndex
                            |> noCmd

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

        EndCombat ->
            case model.currentState of
                Combat { attackingArmy, attackingRegionIndex, defendingArmy, defendingRegionIndex } ->
                    -- refactor getting regions
                    Maybe.map2
                        (\attackingRegion defendingRegion ->
                            let
                                updatedAttackingArmy =
                                    case attackingRegion.army of
                                        Nothing ->
                                            attackingArmy

                                        Just existingArmy ->
                                            (joinArmies attackingArmy existingArmy)
                            in
                                { model
                                    | currentState = Idle
                                    , regions =
                                        model.regions
                                            |> Array.set defendingRegionIndex { defendingRegion | army = Just defendingArmy }
                                            |> Array.set attackingRegionIndex { attackingRegion | army = Just updatedAttackingArmy }
                                }
                        )
                        (Array.get attackingRegionIndex model.regions)
                        (Array.get defendingRegionIndex model.regions)
                        |> Maybe.withDefault (setError model "attacking or defending region do not exist - resetting state")
                        |> noCmd

                _ ->
                    model |> noCmd

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


selectRegion : Model -> Int -> Model
selectRegion model clickedIndex =
    case Array.get clickedIndex model.regions of
        Nothing ->
            setError model "selected region no longer exists - resetting state"

        Just clickedRegion ->
            case clickedRegion.army of
                Just army ->
                    if army.side == model.turn then
                        { model
                            | regions = model.regions |> Array.set clickedIndex { clickedRegion | army = Nothing }
                            , currentState = MovingArmy clickedIndex army
                        }
                    else
                        model

                Nothing ->
                    model


deselectRegion : Int -> Army -> Model -> Model
deselectRegion selectedRegionIndex army model =
    case Array.get selectedRegionIndex model.regions of
        Nothing ->
            setError model "selected region no longer exists - resetting state"

        Just selectedRegion ->
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


moveArmy : Army -> Int -> Int -> Model -> Model
moveArmy movingArmy oldRegionIndex newRegionIndex model =
    (checkOldRegionExists model oldRegionIndex)
        |> Result.andThen (checkNewRegionConnected model newRegionIndex)
        |> Result.andThen (checkNewRegionExists model newRegionIndex)
        |> Result.andThen (checkMovePoints model movingArmy)
        |> Result.andThen (moveIfEmpty model newRegionIndex)
        |> Result.andThen (joinOrBattle model oldRegionIndex newRegionIndex)
        |> flattenResult


checkOldRegionExists : Model -> Int -> Result Model Region
checkOldRegionExists model oldRegionIndex =
    case Array.get oldRegionIndex model.regions of
        Nothing ->
            Err <| setError model "selected region no longer exists - resetting state"

        Just oldRegion ->
            Ok oldRegion


checkNewRegionConnected : Model -> Int -> Region -> Result Model Region
checkNewRegionConnected model newRegionIndex oldRegion =
    if connected oldRegion newRegionIndex then
        Ok oldRegion
    else
        -- ignore clicks on regions that aren't connected
        Err model


checkNewRegionExists : Model -> Int -> Region -> Result Model ( Region, Region )
checkNewRegionExists model newRegionIndex oldRegion =
    case Array.get newRegionIndex model.regions of
        Nothing ->
            Err <| setError model "clicked region no longer exists - resetting state"

        Just newRegion ->
            Ok ( oldRegion, newRegion )


checkMovePoints : Model -> Army -> ( Region, Region ) -> Result Model ( Region, Region, Army )
checkMovePoints model movingArmy ( oldRegion, newRegion ) =
    if List.any (\unit -> unit.moves < 1) movingArmy.units then
        Err { model | error = Just "At least one unit in the army has no more movement points. Consider splitting the army." }
    else
        let
            -- subtract one from the movement of all units moving
            movedArmy =
                { movingArmy | units = movingArmy.units |> List.map (\unit -> { unit | moves = unit.moves - 1 }) }
        in
            Ok ( oldRegion, newRegion, movedArmy )


moveIfEmpty : Model -> Int -> ( Region, Region, Army ) -> Result Model ( Region, Region, Army, Army )
moveIfEmpty model newIndex ( oldRegion, newRegion, movedArmy ) =
    case newRegion.army of
        Nothing ->
            -- move the army into the empty region and stop
            Err
                ({ model
                    | currentState = Idle
                    , regions = model.regions |> Array.set newIndex { newRegion | army = Just movedArmy }
                 }
                )

        Just newArmy ->
            Ok ( oldRegion, newRegion, movedArmy, newArmy )


joinOrBattle : Model -> Int -> Int -> ( Region, Region, Army, Army ) -> Result Model Model
joinOrBattle model oldRegionIndex newRegionIndex ( oldRegion, newRegion, movedArmy, newArmy ) =
    if newArmy.side == movedArmy.side then
        -- join the armies
        Ok
            { model
                | currentState = Idle
                , regions = model.regions |> Array.set newRegionIndex { newRegion | army = Just (joinArmies movedArmy newArmy) }
            }
    else
        -- start a battle
        Ok
            { model
                | combatBoard = Just (majorBoard model.turn)
                , currentState =
                    Combat
                        { attackingArmy = movedArmy
                        , attackingRegionIndex = oldRegionIndex
                        , defendingArmy = newArmy
                        , defendingRegionIndex = newRegionIndex
                        }
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
