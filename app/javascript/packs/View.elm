module View exposing (..)

import Html exposing (Html, h1, h3, h5, div, text, img, button)
import Html.Attributes exposing (id, class, style, src)
import Html.Events exposing (onClick)
import Array
import Set
import Maybe exposing (andThen)
import Model exposing (..)
import Update exposing (..)


view : Model -> Html Action
view model =
    div []
        (List.concat
            [ [ errorModal model.error model.browser ]
            , (case model.currentState of
                Combat combatMeta ->
                    [ img [ src combatMeta.board.imageSrc ] []
                    , div [] (Array.toList (Array.indexedMap renderCombatRegion combatMeta.board.regions))
                    ]

                _ ->
                    [ img [ src "map_v2.png" ] []
                    , div [] (Array.toList (Array.indexedMap (renderMainRegion <| selectedRegion <| model) model.regions))
                    ]
              )
            , [ sidebar model ]
            ]
        )



-- (case model.combatBoard of
--     Nothing ->
--         [ errorModal model.error model.browser
--         , img [ src "map_v2.png" ] []
--         , div [] (Array.toList (Array.indexedMap (renderMainRegion <| selectedRegion <| model) model.regions))
--         , sidebar model
--         ]
--     Just combatBoard ->
--         [ errorModal model.error model.browser
--         , img [ src combatBoard.imageSrc ] []
--         ]
-- )


errorModal : Maybe String -> Browser -> Html Action
errorModal maybeError browser =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            div []
                [ div
                    [ class "modal-backdrop"
                    , style
                        [ ( "width", (browser.windowSize.width |> toString) ++ "px" )
                        , ( "height", (browser.windowSize.height |> toString) ++ "px" )
                        ]
                    ]
                    []
                , div
                    [ class "modal-window"
                    , style
                        (let
                            modalWidth =
                                300

                            modalHeight =
                                70
                         in
                            [ ( "left", ((browser.windowSize.width // 2) - (modalWidth // 2) |> toString) ++ "px" )
                            , ( "top", ((browser.windowSize.height // 2) - (modalHeight // 2) |> toString) ++ "px" )
                            , ( "width", (modalWidth |> toString) ++ "px" )
                            , ( "height", (modalHeight |> toString) ++ "px" )
                            ]
                        )
                    ]
                    [ div [] [ text error ]
                    , button [ onClick ClearError ] [ text "Close" ]
                    ]
                ]


mainRegionColor : Maybe Int -> Int -> Region -> String
mainRegionColor maybeSelectedRegionIndex index region =
    case maybeSelectedRegionIndex of
        Nothing ->
            "inherit"

        Just selectedRegionIndex ->
            if selectedRegionIndex == index then
                "green"
            else if (Set.member selectedRegionIndex region.connections) then
                "yellow"
            else
                "inherit"


renderMainRegion : Maybe Int -> Int -> Region -> Html Action
renderMainRegion maybeSelectedRegionIndex index region =
    renderRegion index region <|
        mainRegionColor maybeSelectedRegionIndex index region


renderCombatRegion : Int -> Region -> Html Action
renderCombatRegion index region =
    renderRegion index region "yellow"


renderRegion : Int -> Region -> String -> Html Action
renderRegion index region backgroundColor =
    div
        [ class "region"
        , style
            [ ( "top", (toString region.position.top) ++ "px" )
            , ( "left", (toString region.position.left) ++ "px" )
            , ( "background-color", backgroundColor )
            ]
        , onClick <| ClickRegion index
        ]
        (army region.army)


army : Maybe Army -> List (Html Action)
army army =
    case army of
        Nothing ->
            []

        Just army ->
            let
                flag =
                    case army.side of
                        Union ->
                            "union_flag_small.png"

                        Confederate ->
                            "confederate_flag_small.png"
            in
                [ div []
                    [ img
                        [ class "flag"
                        , src flag
                        ]
                        []
                    , div
                        [ class "army-info" ]
                        [ text (((List.length army.units) |> toString) ++ " Units") ]
                    ]
                ]


sidebar : Model -> Html Action
sidebar model =
    div
        [ id "sidebar" ]
        [ (stateHeader model)
        , (sidebarButtons model)
        , (armyInfo model)
        ]


stateHeader : Model -> Html Action
stateHeader model =
    div []
        [ h1 []
            [ case model.turn of
                Union ->
                    text "Union"

                Confederate ->
                    text "Confederate"
            ]
        , case model.currentState of
            Idle ->
                h1 [] [ text "Idle" ]

            MovingArmy _ _ ->
                h1 [] [ text "Moving an Army" ]

            SplittingArmy _ _ _ ->
                h1 [] [ text "Splitting an Army" ]

            AddingUnit addingUnitState ->
                div []
                    [ h1 [] [ text "Adding a Unit" ]
                    , case addingUnitState of
                        ChoosingUnitSide ->
                            h3 [] [ text "Choose a Side" ]

                        ChoosingUnitType side ->
                            h3 [] [ text "Choose a Unit Type" ]

                        PlacingUnit side unitType ->
                            h3 [] [ text "Place Your Unit" ]
                    ]

            Combat _ ->
                h1 [] [ text "Combat!" ]
        ]


sidebarButtons : Model -> Html Action
sidebarButtons model =
    case model.currentState of
        Idle ->
            div []
                [ div [] [ button [ onClick (EndTurn) ] [ text "End Turn" ] ]
                , div [] [ button [ onClick (AddUnit Start) ] [ text "Add Unit" ] ]
                ]

        MovingArmy _ army ->
            div []
                [ if List.length army.units > 1 then
                    div [] [ button [ onClick (SplitArmy) ] [ text "Split Army" ] ]
                  else
                    text ""
                ]

        SplittingArmy _ _ _ ->
            text ""

        AddingUnit addingUnitState ->
            case addingUnitState of
                ChoosingUnitSide ->
                    div []
                        [ div [] [ button [ onClick (AddUnit (ChooseUnitSide Union)) ] [ text "Union" ] ]
                        , div [] [ button [ onClick (AddUnit (ChooseUnitSide Confederate)) ] [ text "Confederate" ] ]
                        ]

                ChoosingUnitType side ->
                    div []
                        [ div [] [ button [ onClick (AddUnit (ChooseUnitType side Infantry)) ] [ text "Infantry" ] ]
                        , div [] [ button [ onClick (AddUnit (ChooseUnitType side Cavalry)) ] [ text "Cavalry" ] ]
                        , div [] [ button [ onClick (AddUnit (ChooseUnitType side EliteCavalry)) ] [ text "Elite Cavalry" ] ]
                        , div [] [ button [ onClick (AddUnit (ChooseUnitType side Artillary)) ] [ text "Artillary" ] ]
                        , div [] [ button [ onClick (AddUnit (ChooseUnitType side Leader)) ] [ text "Leader" ] ]
                        ]

                PlacingUnit side unitType ->
                    div []
                        [ div [] [ button [ onClick (AddUnit (ChooseUnitSide side)) ] [ text "Back" ] ]
                        , div [] [ button [ onClick (AddUnit Finish) ] [ text "Done" ] ]
                        ]

        Combat _ ->
            div [] [ div [] [ button [ onClick EndCombat ] [ text "End Combat" ] ] ]


armyInfo : Model -> Html Action
armyInfo model =
    case model.currentState of
        SplittingArmy _ oldArmy newArmy ->
            div []
                [ h5 [] [ text "Existing Army" ]
                , div []
                    (List.map
                        existingUnitInfo
                        (List.sortWith compareUnits oldArmy.units)
                    )
                , h5 [] [ text "New Army" ]
                , div []
                    (List.map
                        newUnitInfo
                        (List.sortWith compareUnits newArmy.units)
                    )
                ]

        MovingArmy _ army ->
            div []
                [ h5 []
                    [ text "Army Details" ]
                , div []
                    (List.map
                        basicUnitInfo
                        (List.sortWith compareUnits army.units)
                    )
                ]

        Combat combatState ->
            div []
                [ h5 [] [ text "Armies to Place" ]
                , div [] [ text (armyToUnits combatState.attackingArmy) ]
                ]

        _ ->
            text ""


basicUnitInfo : Unit -> Html Action
basicUnitInfo unit =
    div []
        [ text (unitMoves unit)
        ]


existingUnitInfo : Unit -> Html Action
existingUnitInfo unit =
    div []
        [ button
            [ onClick (AddUnitToSplit unit)
            , style
                [ ( "visibility"
                  , (if unit.moves > 0 then
                        "inherit"
                     else
                        "hidden"
                    )
                  )
                ]
            ]
            [ text "+" ]
        , text " "
        , text (unitMoves unit)
        ]


newUnitInfo : Unit -> Html Action
newUnitInfo unit =
    div []
        [ button [ onClick (RemoveUnitFromSplit unit) ] [ text "-" ]
        , text " "
        , text (unitMoves unit)
        ]


armyToUnits : Army -> String
armyToUnits army =
    String.join " " (List.map unitToString (List.sortWith compareUnits army.units))


unitToString : Unit -> String
unitToString unit =
    case unit.unitType of
        Infantry ->
            "I"

        Cavalry ->
            "C"

        EliteCavalry ->
            "E"

        Artillary ->
            "A"

        Leader ->
            "L"


unitMoves : Unit -> String
unitMoves unit =
    ((toString unit.unitType) ++ " - " ++ (toString unit.moves) ++ " moves")


compareUnits : Unit -> Unit -> Order
compareUnits a b =
    case compare (unitTypeOrder a.unitType) (unitTypeOrder b.unitType) of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            case compare a.moves b.moves of
                LT ->
                    GT

                GT ->
                    LT

                EQ ->
                    EQ


unitTypeOrder : UnitType -> Int
unitTypeOrder unitType =
    case unitType of
        Infantry ->
            0

        Artillary ->
            1

        Cavalry ->
            2

        EliteCavalry ->
            3

        Leader ->
            4
