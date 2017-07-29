module View exposing (..)

import Html exposing (Html, h1, div, text, img, button)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import Array
import Maybe exposing (..)

import Model exposing (..)
import Update exposing (..)

view : Model -> Html Action
view model =
  -- The inline style is being used for example purposes in order to keep this example simple and
  -- avoid loading additional resources. Use a proper stylesheet when building your own app.
  div []
    [
      img [ src "map_v2.png" ] [],
      div [] (Array.toList (Array.indexedMap (renderRegion model.selectedRegion) model.regions)),
      sidebar model
    ]

renderRegion : Maybe Int -> Int -> Region -> Html Action
renderRegion selectedRegion index region =
  let
    backgroundColor =
      case selectedRegion of
        Nothing -> "inherit"
        Just selectedRegion ->
          if (List.any (\i -> i == selectedRegion) region.connections) then
            "yellow"
          else
            "inherit"
  in
    div
      [
        style
          [
            ("position", "absolute"),
            ("width", "40px"),
            ("height", "40px"),
            ("border-radius", "20px"),
            ("top", (toString region.position.top) ++ "px"),
            ("left", (toString region.position.left) ++ "px"),
            ("background-color", backgroundColor)
          ],
        onClick (ClickRegion index)
      ]
      (army region.army)

army : Maybe Army -> List (Html Action)
army army =
  case army of
    Nothing -> []
    Just army ->
      let
        flag =
          case army.side of
            Union -> "union_flag_small.png"
            Confederate -> "confederate_flag_small.png"
      in
        [
          div []
            [
            img
              [
                style [ ("position", "relative"), ("left", "-5px"), ("top", "3px") ],
                src flag
              ] [],
            div
              [ style [ ("position", "relative"), ("left", "-5px"), ("min-width", "50px") ] ]
              [ text ((totalUnits army |> toString) ++ " Units") ]
            ]
        ]

totalUnits : Army -> Int
totalUnits army =
  List.sum
    [
     army.infantry,
     army.artillary,
     army.cavalry,
     army.eliteCavalry,
     army.leader
    ]

sidebar : Model -> Html Action
sidebar model =
  div
    [
      style
        [
          ("position", "absolute"),
          ("top", "10px"),
          ("left", "900px"),
          ("background-color", "light-gray")
        ]
    ]
    [
      (sidebarButtons model),
      (armyInfo model)
    ]

sidebarButtons : Model -> Html Action
sidebarButtons model =
  case model.currentState of
    Idle ->
      button [ onClick (AddUnit Start) ] [ text "Add Unit" ]
    MovingArmy ->
      text "Moving Army"
    AddingUnit addingUnitState ->
      case addingUnitState of
        ChoosingUnitSide ->
          div []
            [
              div [] [ button [ onClick (AddUnit (ChooseUnitSide Union)) ] [ text "Union" ] ],
              div [] [ button [ onClick (AddUnit (ChooseUnitSide Confederate)) ] [ text "Confederate" ] ]
            ]
        ChoosingUnitType side ->
          div []
            [
              div [] [ button [ onClick (AddUnit (ChooseUnitType side Infantry)) ] [ text "Infantry" ] ],
              div [] [ button [ onClick (AddUnit (ChooseUnitType side Cavalry)) ] [ text "Cavalry" ] ],
              div [] [ button [ onClick (AddUnit (ChooseUnitType side EliteCavalry)) ] [ text "Elite Cavalry" ] ],
              div [] [ button [ onClick (AddUnit (ChooseUnitType side Artillary)) ] [ text "Artillary" ] ],
              div [] [ button [ onClick (AddUnit (ChooseUnitType side Leader)) ] [ text "Leader" ] ]
            ]
        PlacingUnit side unitType ->
          div []
            [
              div [] [ button [ onClick (AddUnit (ChooseUnitSide side)) ] [ text "Back" ] ],
              div [] [ button [ onClick (AddUnit Finish) ] [ text "Done" ] ]
            ]

armyInfo : Model -> Html Action
armyInfo model =
  let
    army =
      model.selectedRegion
        |> andThen (\index -> Array.get index model.regions)
        |> andThen (\region -> region.army)
  in
    case army of
      Nothing -> div [] []
      Just army ->
        div []
          [
            div [] [ text "Army Details" ],
            div [] [ text ((army.infantry |> toString) ++ " Infantry") ],
            div [] [ text ((army.artillary |> toString) ++ " Artillary") ],
            div [] [ text ((army.cavalry |> toString) ++ " Cavalry") ],
            div [] [ text ((army.eliteCavalry |> toString) ++ " Elite Cavalry") ],
            div [] [ text ((army.leader |> toString) ++ " Leaders") ]
          ]
