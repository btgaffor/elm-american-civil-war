import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

import Model exposing (..)
import Update exposing (..)

renderMouseInfo : Model -> Html Action
renderMouseInfo model =
  div [ style [ ("position", "absolute"), ("left", "900px"), ("top", "20px") ] ]
    [
      div [] [ text (model.mousex - 20 |> toString) ],
      div [] [ text (model.mousey - 20 |> toString) ]
    ]

renderMouse : Model -> Html Action
renderMouse model =
  div
    [
      style
        [
          ("position", "absolute"),
          ("width", "40px"),
          ("height", "40px"),
          ("left", (model.mousex - 20 |> toString) ++ "px"),
          ("top", (model.mousey - 20 |> toString) ++ "px"),
          ("border-radius", "50px"),
          ("background-color", "green")
        ]
    ] []
