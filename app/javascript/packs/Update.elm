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
    | ClickRegion Int
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
        ClickRegion clickedIndex ->
            case model.currentState of
                Idle ->
                    selectRegion model clickedIndex |> noCmd

                MovingArmy selectedRegionIndex army ->
                    if selectedRegionIndex == clickedIndex then
                        deselectRegion selectedRegionIndex army model
                    else
                        case (Array.get selectedRegionIndex model.regions) of
                            Nothing ->
                                setError model "Selected region does not exist, resetting state." |> noCmd

                            Just selectedRegion ->
                                if (Set.member clickedIndex selectedRegion.connections) then
                                    moveArmy army selectedRegionIndex clickedIndex model
                                else
                                    model |> noCmd

                SplittingArmy selectedRegionIndex oldArmy newArmy ->
                    case (Array.get selectedRegionIndex model.regions) of
                        Nothing ->
                            setError model "Selected region does not exist, cannot return armies." |> noCmd

                        Just region ->
                            if selectedRegionIndex == clickedIndex then
                                -- cancel the splitting and put both armies back
                                deselectRegion selectedRegionIndex (joinArmies oldArmy newArmy) model
                            else if (Set.member clickedIndex region.connections) then
                                -- put the old army back and move the new army
                                model
                                    |> deselectRegion selectedRegionIndex oldArmy
                                    |> Tuple.first
                                    |> moveArmy newArmy selectedRegionIndex clickedIndex
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
                MovingArmy selectedRegionIndex army ->
                    { model
                        | currentState = (SplittingArmy selectedRegionIndex army { side = model.turn, units = [] })
                    }
                        |> noCmd

                _ ->
                    model |> noCmd

        AddUnitToSplit movingUnit ->
            case model.currentState of
                SplittingArmy selectedRegionIndex oldArmy newArmy ->
                    let
                        modifiedOldArmy =
                            { oldArmy | units = List.Extra.remove movingUnit oldArmy.units }

                        modifiedNewArmy =
                            { newArmy | units = movingUnit :: newArmy.units }
                    in
                        { model | currentState = SplittingArmy selectedRegionIndex modifiedOldArmy modifiedNewArmy } |> noCmd

                _ ->
                    model |> noCmd

        RemoveUnitFromSplit movingUnit ->
            case model.currentState of
                SplittingArmy selectedRegionIndex oldArmy newArmy ->
                    let
                        modifiedOldArmy =
                            { oldArmy | units = movingUnit :: oldArmy.units }

                        modifiedNewArmy =
                            { newArmy | units = List.Extra.remove movingUnit newArmy.units }
                    in
                        { model | currentState = SplittingArmy selectedRegionIndex modifiedOldArmy modifiedNewArmy } |> noCmd

                _ ->
                    model |> noCmd

        -- putting new units on the board
        AddUnit addUnitAction ->
            addUnit model addUnitAction

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
    case regionWithArmy clickedIndex model.regions of
        Just ( clickedRegion, army ) ->
            if army.side == model.turn then
                { model
                    | regions = model.regions |> Array.set clickedIndex { clickedRegion | army = Nothing }
                    , currentState = MovingArmy clickedIndex army
                }
            else
                model

        Nothing ->
            model


deselectRegion : Int -> Army -> Model -> ( Model, Cmd Action )
deselectRegion selectedRegionIndex army model =
    case Array.get selectedRegionIndex model.regions of
        Nothing ->
            { model | currentState = Idle } |> noCmd

        Just region ->
            { model
                | currentState = Idle
                , regions = model.regions |> Array.set selectedRegionIndex { region | army = Just army }
            }
                |> noCmd


addUnit : Model -> AddUnitAction -> ( Model, Cmd Action )
addUnit model addUnitAction =
    case addUnitAction of
        Start ->
            { model | currentState = AddingUnit ChoosingUnitSide } |> noCmd

        ChooseUnitSide side ->
            { model | currentState = AddingUnit (ChoosingUnitType side) } |> noCmd

        ChooseUnitType side unitType ->
            { model | currentState = AddingUnit (PlacingUnit side unitType) } |> noCmd

        Finish ->
            { model | currentState = Idle } |> noCmd


moveArmy : Army -> Int -> Int -> Model -> ( Model, Cmd Action )
moveArmy movingArmy oldIndex newIndex model =
    -- check for destination region
    checkSourceExists model oldIndex
        |> Result.andThen (checkDestinationExists model newIndex)
        |> Result.andThen (checkMovePoints model movingArmy)
        |> Result.andThen (tryMoveArmy model newIndex)
        |> Result.andThen (joinOrBattle model newIndex)
        |> flattenResult
        |> noCmd


checkSourceExists : Model -> Int -> Result Model ()
checkSourceExists model oldIndex =
    case (Array.get oldIndex model.regions) of
        Nothing ->
            Err (setError model "Source region does not exist. Resetting state.")

        Just oldRegion ->
            Ok ()


checkDestinationExists : Model -> Int -> () -> Result Model Region
checkDestinationExists model newIndex () =
    case (Array.get newIndex model.regions) of
        Nothing ->
            Err (setError model "Destination region does not exist. Resetting state.")

        Just newRegion ->
            Ok newRegion


checkMovePoints : Model -> Army -> Region -> Result Model ( Region, Army )
checkMovePoints model movingArmy newRegion =
    case List.any (\unit -> unit.moves < 1) movingArmy.units of
        True ->
            Err { model | error = Just "At least one unit in the army has no more movement points. Consider splitting the army." }

        False ->
            let
                -- subtract one from the movement of all units moving
                movedArmy =
                    { movingArmy | units = movingArmy.units |> List.map (\unit -> { unit | moves = unit.moves - 1 }) }
            in
                Ok ( newRegion, movedArmy )


tryMoveArmy : Model -> Int -> ( Region, Army ) -> Result Model ( Region, Army, Army )
tryMoveArmy model newIndex ( newRegion, movedArmy ) =
    case newRegion.army of
        -- move the army into the empty region
        Nothing ->
            Err
                ({ model
                    | currentState = Idle
                    , regions = model.regions |> Array.set newIndex { newRegion | army = Just movedArmy }
                 }
                )

        -- either join the armies or battle!
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
        let
            combatBoard =
                model.combatBoard
        in
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


{-| returns Just (region, army) if the region exists and has an army
-}
regionWithArmy : Int -> Array.Array Region -> Maybe ( Region, Army )
regionWithArmy clickedIndex regions =
    Array.get clickedIndex regions
        |> Maybe.andThen
            (\clickedRegion ->
                clickedRegion.army
                    |> Maybe.map (\army -> ( clickedRegion, army ))
            )


flattenResult : Result value value -> value
flattenResult result =
    case result of
        Ok value ->
            value

        Err value ->
            value
