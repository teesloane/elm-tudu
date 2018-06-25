module Models exposing (..)

-- our top level model for entire state.


type alias Model =
    { todos : List Todo
    , thing : Bool
    }


initialModel : Model
initialModel =
    { todos =
        [ Todo "1" "Get milk" False
        , Todo "2" "Do Thing" False
        ]
    , thing = True
    }



-- A single todo type


type alias Todo =
    { id : String
    , name : String
    , complete : Bool
    }
