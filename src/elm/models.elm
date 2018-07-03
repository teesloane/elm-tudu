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
    , dragTargetExists : Bool
    , draggedTodo : Maybe Todo
    , dragTarget : Maybe Todo
    , timeAtLoad : Time
    , currentWeek : List TodoList
    }


initialModel : Model
initialModel =
    { todos = []
    , uuid = 0
    , beingDragged = False
    , dragTarget = Nothing
    , dragTargetExists = False
    , draggedTodo = Nothing
    , timeAtLoad = 0
    , currentWeek = []
    }



-- Todo:  Type + Funcs:


type alias Todo =
    { id : Int
    , isEditing : Bool
    , name : String
    , complete : Bool
    , parentList : TodoListName
    , order : Int
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


getTodosInList :
    { b | name : a }
    -> { d | todos : List { c | parentList : a } }
    -> List { c | parentList : a }
getTodosInList todoList model =
    List.filter (\t -> t.parentList == todoList.name) model.todos
