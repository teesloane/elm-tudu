module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))


-- our top level model for entire state.


type alias Model =
    { todos : List Todo
    , thing : Bool
    , timeAtLoad : Time
    , dateAtLoad : Maybe Date
    }


initialModel : Model
initialModel =
    { todos =
        [ Todo "1" "Get milk" False
        , Todo "2" "Do Thing" False
        ]
    , thing = True
    , timeAtLoad = 0
    , dateAtLoad = Nothing
    }



-- A single todo type


type alias Todo =
    { id : String
    , name : String
    , complete : Bool
    }
