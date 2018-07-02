module Main exposing (..)

import Html exposing (Html)


-- import Mouse exposing (..)

import Window
import Model exposing (Model)
import Update exposing (Action(WindowResize), init, update)
import View exposing (view)


subscriptions : Model -> Sub Action
subscriptions model =
    -- Mouse.moves (\position -> MouseMove position.x position.y)
    Window.resizes (\size -> WindowResize size)



-- MAIN


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
