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
    , dragTarget : Maybe TodoList
    , timeAtLoad : Time
    , dateAtLoad : Maybe Date
    , currentWeek : List TodoList
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
    , parentList : TodoListName
    , ts : Time
    }


type alias TodoListName =
    String


type alias TodoList =
    { hasTodos : Bool
    , inputField : String
    , date : Date
    , name : TodoListName
    , ts : Time
    }
