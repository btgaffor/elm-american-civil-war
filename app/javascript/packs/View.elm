module View exposing (..)

import Html exposing (Html, h1, h3, h5, div, text, img, button)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import Array
import Maybe exposing (..)

import Model exposing (..)
import Update exposing (..)
import Utils exposing (..)

view : Model -> Html Action
view model =
  -- The inline style is being used for example purposes in order to keep this example simple and
  -- avoid loading additional resources. Use a proper stylesheet when building your own app.
  div []
    [
      errorModal model,
      img [ src "map_v2.png" ] [],
      div [] (Array.toList (Array.indexedMap (renderRegion model.selectedRegion) model.regions)),
      sidebar model
    ]

errorModal : Model -> Html Action
errorModal model =
  case model.error of
    Nothing -> div [] []
    Just error ->
      div []
        [
         div
           [
             style
               [
                 ("position", "absolute"),
                 ("left", "0px"),
                 ("top", "0px"),
                 ("width", (model.windowSize.width |> toString) ++ "px"),
                 ("height", (model.windowSize.height |> toString) ++ "px"),
                 ("background-color", "gray"),
                 ("opacity", "0.8"),
                 ("z-index", "1000") -- make sure it's above everything
               ]
           ] [],
         div
           [
             style
               (
                 let
                   modalWidth = 300
                   modalHeight = 70
                 in
                   [
                     ("position", "absolute"),
                     ("left", ((model.windowSize.width // 2) - (modalWidth // 2) |> toString) ++ "px"),
                     ("top", ((model.windowSize.height // 2) - (modalHeight // 2) |> toString) ++ "px"),
                     ("width", (modalWidth |> toString) ++ "px"),
                     ("height", (modalHeight |> toString) ++ "px"),
                     ("background-color", "white"),
                     ("z-index", "1001"), -- make sure it's above everything
                     ("border-radius", "10px"),
                     ("border", "black solid 2px"),
                     ("padding", "10px"),
                     ("text-align", "center"),
                     ("display", "flex"),
                     ("flex-direction", "column"),
                     ("justify-content", "space-between")
                   ]
               )
           ]
           [
             div [] [ text error ],
             button [ onClick ClearError ] [ text "Close" ]
           ]
        ]

renderRegion : Maybe Int -> Int -> Region -> Html Action
renderRegion selectedRegion index region =
  let
    backgroundColor =
      case selectedRegion of
        Nothing -> "inherit"
        Just selectedRegion ->
          if (listContains region.connections selectedRegion) then
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
      (stateHeader model),
      (sidebarButtons model),
      (armyInfo model)
    ]

stateHeader : Model -> Html Action
stateHeader model =
  case model.currentState of
    Idle -> h1 [] [ text "Idle" ]
    MovingArmy -> h1 [] [ text "Moving an Army" ]
    AddingUnit addingUnitState ->
      div []
        [
          h1 [] [ text "Adding a Unit" ],
          case addingUnitState of
            ChoosingUnitSide -> h3 [] [ text "Choose a Side" ]
            ChoosingUnitType side -> h3 [] [ text "Choose a Unit Type" ]
            PlacingUnit side unitType -> h3 [] [ text "Place Your Unit" ]
        ]

sidebarButtons : Model -> Html Action
sidebarButtons model =
  case model.currentState of
    Idle ->
      button [ onClick (AddUnit Start) ] [ text "Add Unit" ]
    MovingArmy -> div [] []
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
            h5 [] [ text "Army Details" ],
            div [] [ text ((army.infantry |> toString) ++ " Infantry") ],
            div [] [ text ((army.artillary |> toString) ++ " Artillary") ],
            div [] [ text ((army.cavalry |> toString) ++ " Cavalry") ],
            div [] [ text ((army.eliteCavalry |> toString) ++ " Elite Cavalry") ],
            div [] [ text ((army.leader |> toString) ++ " Leaders") ]
          ]
