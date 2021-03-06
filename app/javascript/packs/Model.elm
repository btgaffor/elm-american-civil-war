module Model exposing (..)

import Array
import Set exposing (Set)
import Window exposing (Size)


-- external


type alias Browser =
    { mousex : Int
    , mousey : Int
    , windowSize : Size
    }



-- general


type Side
    = Union
    | Confederate


type UnitType
    = Infantry
    | Cavalry
    | EliteCavalry
    | Artillary
    | Leader


type alias Army =
    { side : Side
    , units : List Unit
    }


type alias Unit =
    { unitType : UnitType, moves : Int }



-- game state


type AddingUnitState
    = ChoosingUnitSide
    | ChoosingUnitType Side
    | PlacingUnit Side UnitType


type CurrentState
    = Idle
    | MovingArmy Int Army
    | SplittingArmy Int Army Army
    | AddingUnit AddingUnitState
    | Combat CombatMeta


type CombatState
    = Deploying



-- misc


type alias CombatMeta =
    { board : CombatBoard
    , turn : Side
    , state : CombatState
    , attackingArmy : Army
    , attackingRegionIndex : Int
    , defendingArmy : Army
    , defendingRegionIndex : Int
    }



-- board


type alias Region =
    { position : Position
    , army : Maybe Army
    , connections : Set Int
    }


type alias Position =
    { left : Int
    , top : Int
    }


type CombatBoard
    = MajorBoard MajorBoardMeta


type alias MajorBoardMeta =
    { imageSrc : String
    , enemyRetreat : Region
    , enemyReserves : Region
    , enemyLeft : Region
    , enemyCenter : Region
    , enemyRight : Region
    , neutralLeft : Region
    , neutralCenter : Region
    , neutralRight : Region
    , friendlyLeft : Region
    , friendlyCenter : Region
    , friendlyRight : Region
    , friendlyReserves : Region
    , friendlyRetreat : Region
    }


type alias Model =
    { error : Maybe String
    , browser : Browser
    , turn : Side
    , currentState : CurrentState
    , regions : Array.Array Region
    }



-- INIT
-- skirmishBoard : CombatBoard


skirmishBoard =
    { imageSrc = "major_board_small.jpeg"
    , regions =
        Array.fromList
            []
    }


majorBoard : MajorBoardMeta
majorBoard =
    { imageSrc = "major_board_small.jpeg"
    , enemyRetreat =
        { position = { left = 317, top = 66 }
        , army = Nothing
        , connections = Set.empty
        }
    , enemyReserves =
        { position = { left = 317, top = 140 }
        , army = Nothing
        , connections = Set.empty
        }
    , enemyLeft =
        { position = { left = 50, top = 216 }
        , army = Nothing
        , connections = Set.empty
        }
    , enemyCenter =
        { position = { left = 317, top = 216 }
        , army = Nothing
        , connections = Set.empty
        }
    , enemyRight =
        { position = { left = 583, top = 216 }
        , army = Nothing
        , connections = Set.empty
        }
    , neutralLeft =
        { position = { left = 50, top = 292 }
        , army = Nothing
        , connections = Set.empty
        }
    , neutralCenter =
        { position = { left = 317, top = 292 }
        , army = Nothing
        , connections = Set.empty
        }
    , neutralRight =
        { position = { left = 583, top = 292 }
        , army = Nothing
        , connections = Set.empty
        }
    , friendlyLeft =
        { position = { left = 50, top = 368 }
        , army = Nothing
        , connections = Set.empty
        }
    , friendlyCenter =
        { position = { left = 317, top = 368 }
        , army = Nothing
        , connections = Set.empty
        }
    , friendlyRight =
        { position = { left = 583, top = 368 }
        , army = Nothing
        , connections = Set.empty
        }
    , friendlyReserves =
        { position = { left = 317, top = 443 }
        , army = Nothing
        , connections = Set.empty
        }
    , friendlyRetreat =
        { position = { left = 317, top = 519 }
        , army = Nothing
        , connections = Set.empty
        }
    }


initialModel : Model
initialModel =
    { error = Nothing
    , browser =
        { mousex = 0
        , mousey = 0
        , windowSize = { width = 0, height = 0 }
        }
    , regions =
        Array.fromList
            [ { -- 0
                position = { left = 138, top = 111 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 1, 2 ]
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
              , connections = Set.fromList [ 0, 3 ]
              }
            , { -- 2
                position = { left = 329, top = 158 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 0, 3, 4, 5, 11 ]
              }
            , { -- 3
                position = { left = 200, top = 265 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 1, 2, 4, 8 ]
              }
            , { -- 4
                position = { left = 334, top = 271 }
              , army =
                    Just
                        { side = Union
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 2, 3, 10, 13 ]
              }
            , { -- 5
                position = { left = 424, top = 71 }
              , army = Nothing
              , connections = Set.fromList [ 2, 6, 7, 11 ]
              }
            , { -- 6
                position = { left = 475, top = 101 }
              , army = Nothing
              , connections = Set.fromList [ 5 ]
              }
            , { -- 7
                position = { left = 632, top = 100 }
              , army = Nothing
              , connections = Set.fromList [ 5, 12 ]
              }
            , { -- 8
                position = { left = 102, top = 349 }
              , army = Nothing
              , connections = Set.fromList [ 3, 9, 10 ]
              }
            , { -- 9
                position = { left = 175, top = 377 }
              , army = Nothing
              , connections = Set.fromList [ 8 ]
              }
            , { -- 10
                position = { left = 346, top = 373 }
              , army = Nothing
              , connections = Set.fromList [ 4, 8, 13 ]
              }
            , { -- 11
                position = { left = 451, top = 203 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 2, 5, 12, 13 ]
              }
            , { -- 12
                position = { left = 578, top = 242 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 7, 11, 13, 14, 15 ]
              }
            , { -- 13
                position = { left = 456, top = 353 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 4, 10, 11, 12, 14, 15 ]
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
              , connections = Set.fromList [ 12, 13, 15 ]
              }
            , { -- 15
                position = { left = 609, top = 392 }
              , army =
                    Just
                        { side = Confederate
                        , units = [ infantry ]
                        }
              , connections = Set.fromList [ 12, 13, 14 ]
              }
            ]
    , turn = Confederate
    , currentState =
        Combat
            { board = MajorBoard majorBoard
            , turn = Confederate
            , state = Deploying
            , attackingArmy = { side = Confederate, units = [] }
            , attackingRegionIndex = 1
            , defendingArmy = { side = Union, units = [] }
            , defendingRegionIndex = 2
            }
    }



-- UTILITIES


selectedRegion : Model -> Maybe Int
selectedRegion model =
    case model.currentState of
        MovingArmy selectedRegionIndex _ ->
            Just selectedRegionIndex

        SplittingArmy selectedRegionIndex _ _ ->
            Just selectedRegionIndex

        _ ->
            Nothing


connected : Region -> Int -> Bool
connected region index =
    Set.member index region.connections


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
