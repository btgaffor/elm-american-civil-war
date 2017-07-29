module Main exposing (..)

import Html exposing (Html)
-- import Mouse exposing (..)
import Window

import Model exposing (..)
import Update exposing (..)
import View exposing (..)

-- VIEW

-- ACTION


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Action
subscriptions model =
  Window.resizes (\size -> WindowResize size)
  -- Mouse.moves (\position -> MouseMove position.x position.y)

-- MAIN

main : Program Never Model Action
main =
  Html.program
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }
