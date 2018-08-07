module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))
import RemoteData exposing (WebData)
import Todo.Model exposing (..)
import TodoList.Model exposing (TodoList)


-- our top level model for entire state.


type alias Model =
    { todos : WebData (List Todo)
    , beingDragged : Bool
    , currentWeek : List TodoList
    , customLists : WebData (List TodoList)
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
    , customLists = RemoteData.Loading
    , dayOffset = 0
    , dragTarget = Nothing
    , dragTargetExists = False
    , draggedTodo = Nothing
    , timeAtLoad = 0
    , uuid = 0
    }
