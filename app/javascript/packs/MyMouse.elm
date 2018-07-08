module MyMouse exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Model exposing (Browser)
import Update exposing (..)


renderMouseInfo : Browser -> Html Action
renderMouseInfo browser =
    div [ style [ ( "position", "absolute" ), ( "left", "1200px" ), ( "top", "20px" ) ] ]
        [ div [] [ text (browser.mousex - 130 |> toString) ]
        , div [] [ text (browser.mousey - 35 |> toString) ]
        ]



-- renderMouse : Browser -> Html Action
-- renderMouse browser =
--     div
--         [ style
--             [ ( "position", "absolute" )
--             , ( "width", "40px" )
--             , ( "height", "40px" )
--             , ( "left", (browser.mousex - 20 |> toString) ++ "px" )
--             , ( "top", (browser.mousey - 20 |> toString) ++ "px" )
--             , ( "border-radius", "50px" )
--             , ( "background-color", "green" )
--             ]
--         ]
--         []


renderMouse : Browser -> Html Action
renderMouse browser =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "width", "260px" )
            , ( "height", "70px" )
            , ( "left", (browser.mousex - 130 |> toString) ++ "px" )
            , ( "top", (browser.mousey - 35 |> toString) ++ "px" )
            , ( "background-color", "yellow" )
            ]
        ]
        []
