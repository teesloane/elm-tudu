module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))
import RemoteData exposing (WebData)


-- our top level model for entire state.


type alias Model =
    { todos : WebData (List Todo)
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
    { todos = RemoteData.Loading
    , beingDragged = False
    , currentWeek = []
    , dayOffset = 0
    , dragTarget = Nothing
    , dragTargetExists = False
    , draggedTodo = Nothing
    , timeAtLoad = 0
    , uuid = 0
    }


type alias Todo =
    { id : Int
    , isEditing : Bool
    , name : String
    , complete : Bool
    , parentList : String
    , order : Int
    , ts : Time
    }



-- maybeTodos : WebData (List Todo) -> Html Msg
-- maybeTodos : RemoteData.RemoteData e (List a) -> List a


maybeTodos response =
    case response of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success todos ->
            todos

        RemoteData.Failure error ->
            -- FIXME HANDLE THIS CASE
            []


type alias TodoListName =
    String


type alias TodoList =
    { hasTodos : Bool
    , inputField : String
    , date : Date
    , name : String
    , ts : Time
    }


getTodosInList :
    { b | name : a }
    -> { d | todos : RemoteData.RemoteData e (List { c | parentList : a }) }
    -> List { c | parentList : a }
getTodosInList todoList model =
    List.filter (\t -> t.parentList == todoList.name) (maybeTodos model.todos)
