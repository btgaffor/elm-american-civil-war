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
        [ errorModal model
        , img [ src "map_v2.png" ] []
        , div [] (Array.toList (Array.indexedMap (renderRegion <| selectedRegion <| model) model.regions))
        , sidebar model
        ]


errorModal : Model -> Html Action
errorModal model =
    case model.error of
        Nothing ->
            div [] []

        Just error ->
            div []
                [ div
                    [ class "modal-backdrop"
                    , style
                        [ ( "width", (model.windowSize.width |> toString) ++ "px" )
                        , ( "height", (model.windowSize.height |> toString) ++ "px" )
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
                            [ ( "left", ((model.windowSize.width // 2) - (modalWidth // 2) |> toString) ++ "px" )
                            , ( "top", ((model.windowSize.height // 2) - (modalHeight // 2) |> toString) ++ "px" )
                            , ( "width", (modalWidth |> toString) ++ "px" )
                            , ( "height", (modalHeight |> toString) ++ "px" )
                            ]
                        )
                    ]
                    [ div [] [ text error ]
                    , button [ onClick ClearError ] [ text "Close" ]
                    ]
                ]


renderRegion : Maybe Int -> Int -> Region -> Html Action
renderRegion selectedRegion index region =
    let
        backgroundColor =
            case selectedRegion of
                Nothing ->
                    "inherit"

                Just selectedRegion ->
                    if selectedRegion == index then
                        "green"
                    else if (Set.member selectedRegion region.connections) then
                        "yellow"
                    else
                        "inherit"
    in
        div
            [ class "region"
            , style
                [ ( "top", (toString region.position.top) ++ "px" )
                , ( "left", (toString region.position.left) ++ "px" )
                , ( "background-color", backgroundColor )
                ]
            , onClick (ClickRegion index)
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

            MovingArmy selectedRegionIndex army ->
                h1 [] [ text "Moving an Army" ]

            SplittingArmy selectedRegionIndex oldArmy newArmy ->
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
        ]


sidebarButtons : Model -> Html Action
sidebarButtons model =
    case model.currentState of
        Idle ->
            div []
                [ div [] [ button [ onClick (EndTurn) ] [ text "End Turn" ] ]
                , div [] [ button [ onClick (AddUnit Start) ] [ text "Add Unit" ] ]
                ]

        MovingArmy selectedRegionIndex army ->
            div []
                [ if List.length army.units > 1 then
                    div [] [ button [ onClick (SplitArmy) ] [ text "Split Army" ] ]
                  else
                    text ""
                ]

        SplittingArmy selectedRegionIndex oldArmy newArmy ->
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


armyInfo : Model -> Html Action
armyInfo model =
    case model.currentState of
        SplittingArmy selectedRegionIndex oldArmy newArmy ->
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

        MovingArmy selectedRegionIndex army ->
            div []
                [ h5 []
                    [ text "Army Details" ]
                , div []
                    (List.map
                        basicUnitInfo
                        (List.sortWith compareUnits army.units)
                    )
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
