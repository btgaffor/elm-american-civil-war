module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Model exposing (..)
import Update exposing (..)


renderMouseInfo : Browser -> Html Action
renderMouseInfo browser =
    div [ style [ ( "position", "absolute" ), ( "left", "900px" ), ( "top", "20px" ) ] ]
        [ div [] [ text (browser.mousex - 20 |> toString) ]
        , div [] [ text (browser.mousey - 20 |> toString) ]
        ]


renderMouse : Browser -> Html Action
renderMouse browser =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "width", "40px" )
            , ( "height", "40px" )
            , ( "left", (browser.mousex - 20 |> toString) ++ "px" )
            , ( "top", (browser.mousey - 20 |> toString) ++ "px" )
            , ( "border-radius", "50px" )
            , ( "background-color", "green" )
            ]
        ]
        []
