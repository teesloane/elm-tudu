module Models exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))
import RemoteData exposing (WebData)
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


type alias Todo =
    { id : Int
    , isEditing : Bool
    , name : String
    , complete : Bool
    , parentList : String
    , position : Int
    , createdAt : Time
    , originalDay : Time
    , currentDay : Time
    , hasRolledOver : Bool
    }


createDefaultTodo opts =
    { id = opts.id -- no good, change to uuid soon
    , isEditing = False
    , name = opts.parentList.inputField
    , complete = False
    , parentList = opts.parentList.originalName
    , position = opts.position
    , originalDay = (Date.toTime opts.parentList.date)
    , currentDay = (Date.toTime opts.parentList.date)
    , hasRolledOver = False
    , createdAt = (Date.toTime opts.parentList.date)
    }


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
