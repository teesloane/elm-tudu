module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))


-- our top level model for entire state.


type alias Model =
    { todos : List Todo
    , beingDragged : Maybe Todo
    , timeAtLoad : Time
    , dateAtLoad : Maybe Date
    , currentWeek : List Day
    }


initialModel : Model
initialModel =
    { todos =
        [ Todo "1" False "Get milk" False 1530020370009
        , Todo "2" False "Do Thing" False 1530120370009
        ]
    , beingDragged = Nothing
    , timeAtLoad = 0
    , dateAtLoad = Nothing
    , currentWeek = []
    }



-- A single todo type


type alias Todo =
    { id : String
    , isEditing : Bool
    , name : String
    , complete : Bool
    , ts : Time
    }


type alias Day =
    { hasTodos : Bool
    , field : String
    , date : Date
    }
