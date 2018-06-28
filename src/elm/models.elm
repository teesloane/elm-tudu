module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))


-- our top level model for entire state.


type alias Model =
    { todos : List Todo

    -- , beingDragged : Maybe Todo
    , uuid : Int
    , beingDragged : Bool
    , draggedTodo : Maybe Todo
    , dragTarget : Maybe Day
    , timeAtLoad : Time
    , dateAtLoad : Maybe Date
    , currentWeek : List Day
    }


initialModel : Model
initialModel =
    { todos = []
    , uuid = 0
    , beingDragged = False
    , dragTarget = Nothing
    , draggedTodo = Nothing
    , timeAtLoad = 0
    , dateAtLoad = Nothing
    , currentWeek = []
    }



-- A single todo type


type alias Todo =
    { id : Int
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
