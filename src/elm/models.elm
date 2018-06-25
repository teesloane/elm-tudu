module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))
import Dict exposing (..)


-- our top level model for entire state.


type alias Model =
    { todos : List Todo
    , todomap : Dict Time Todo
    , thing : Bool
    , timeAtLoad : Time
    , dateAtLoad : Maybe Date
    }


initialModel : Model
initialModel =
    { todos =
        [ Todo "1"
            "Get milk"
            False
            1529932761990
        , Todo "2"
            "Do Thing"
            False
            1529932761998
        ]
    , todomap =
        Dict.fromList
            [ ( 1529932761990, Todo "1" "Get Milk" False 1529932761990 )
            , ( 1529932761999, Todo "2" "Get Apples" False 1529932761999 )
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
    , ts : Time
    }


type alias Day =
    { hasTodos : Bool
    }
