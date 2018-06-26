module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))
import Dict exposing (..)


-- our top level model for entire state.


type alias Model =
    { todos : List Todo
    , timeAtLoad : Time
    , dateAtLoad : Maybe Date
    , currentWeek : List Day
    }


initialModel : Model
initialModel =
    { todos =
        [ Todo "1" "Get milk" False 1530020370009
        , Todo "2" "Do Thing" True 1529932761998
        ]
    , timeAtLoad = 0
    , dateAtLoad = Nothing
    , currentWeek = []
    }



-- A single todo type


type alias Todo =
    { id : String
    , name : String
    , complete : Bool
    , ts : Time
    }


type alias Day =
    { hasTodos : Bool
    , date : Date
    }
