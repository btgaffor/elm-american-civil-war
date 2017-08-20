module Model exposing (..)

import Array
import Window exposing (Size)


type AddingUnitState
    = ChoosingUnitSide
    | ChoosingUnitType Side
    | PlacingUnit Side UnitType


type CurrentState
    = Idle
    | MovingArmy
    | AddingUnit AddingUnitState


type Side
    = Union
    | Confederate


type UnitType
    = Infantry
    | Cavalry
    | EliteCavalry
    | Artillary
    | Leader


type alias Model =
    { error : Maybe String
    , mousex : Int
    , mousey : Int
    , windowSize : Size
    , regions : Array.Array Region
    , selectedRegion : Maybe Int
    , turn : Side
    , currentState : CurrentState
    }


type alias Region =
    { position : Position
    , army : Maybe Army
    , connections : List Int
    }


type alias Position =
    { left : Int
    , top : Int
    }


type alias Army =
    { side : Side
    , units : List Unit
    }


type alias Unit =
    { unitType : UnitType, moves : Int }


infantry : Unit
infantry =
    { unitType = Infantry, moves = 1 }


artillary : Unit
artillary =
    { unitType = Artillary, moves = 1 }


cavalry : Unit
cavalry =
    { unitType = Cavalry, moves = 2 }


eliteCavalry : Unit
eliteCavalry =
    { unitType = EliteCavalry, moves = 2 }


leader : Unit
leader =
    { unitType = Leader, moves = 2 }



-- INIT


model : Model
model =
    { error = Nothing
    , mousex = 0
    , mousey = 0
    , windowSize = { width = 0, height = 0 }
    , selectedRegion = Nothing
    , regions =
        Array.fromList
            [ { -- 0
                position = { left = 138, top = 111 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = [ 1, 2 ]
              }
            , { -- 1
                position = { left = 147, top = 211 }
              , army =
                    Just
                        { side = Union
                        , units =
                            [ infantry
                            , artillary
                            , cavalry
                            , eliteCavalry
                            , leader
                            ]
                        }
              , connections = [ 0, 3 ]
              }
            , { -- 2
                position = { left = 329, top = 158 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = [ 0, 3, 4, 5, 11 ]
              }
            , { -- 3
                position = { left = 200, top = 265 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = [ 1, 2, 4, 8 ]
              }
            , { -- 4
                position = { left = 334, top = 271 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = [ 2, 3, 10, 13 ]
              }
            , { -- 5
                position = { left = 424, top = 71 }
              , army = Nothing
              , connections = [ 2, 6, 7, 11 ]
              }
            , { -- 6
                position = { left = 475, top = 101 }
              , army = Nothing
              , connections = [ 5 ]
              }
            , { -- 7
                position = { left = 632, top = 100 }
              , army = Nothing
              , connections = [ 5, 12 ]
              }
            , { -- 8
                position = { left = 102, top = 349 }
              , army = Nothing
              , connections = [ 3, 9, 10 ]
              }
            , { -- 9
                position = { left = 175, top = 377 }
              , army = Nothing
              , connections = [ 8 ]
              }
            , { -- 10
                position = { left = 346, top = 373 }
              , army = Nothing
              , connections = [ 4, 8, 13 ]
              }
            , { -- 11
                position = { left = 451, top = 203 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = [ 2, 5, 12, 13 ]
              }
            , { -- 12
                position = { left = 578, top = 242 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = [ 7, 11, 13, 14, 15 ]
              }
            , { -- 13
                position = { left = 456, top = 353 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = [ 4, 10, 11, 12, 14, 15 ]
              }
            , { -- 14
                position = { left = 532, top = 337 }
              , army =
                    Just
                        { side = Confederate
                        , units =
                            [ infantry
                            , artillary
                            , cavalry
                            , eliteCavalry
                            , leader
                            ]
                        }
              , connections = [ 12, 13, 15 ]
              }
            , { -- 15
                position = { left = 609, top = 392 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = [ 12, 13, 14 ]
              }
            ]
    , turn = Confederate
    , currentState = Idle
    }
