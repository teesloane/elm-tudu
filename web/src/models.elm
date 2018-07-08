module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))


-- our top level model for entire state.


type alias Model =
    { todos : List Todo
    , beingDragged : Bool
    , currentWeek : List TodoList
    , dayOffset : Int
    , dragTarget : Maybe Todo
    , dragTargetExists : Bool
    , draggedTodo : Maybe Todo
    , timeAtLoad : Time
    , uuid : Int
    }


initialModel : Model
initialModel =
    { todos = []
    , beingDragged = False
    , currentWeek = []
    , dayOffset = 0
    , dragTarget = Nothing
    , dragTargetExists = False
    , draggedTodo = Nothing
    , timeAtLoad = 0
    , uuid = 0
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
