module Todo.Model exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Maybe exposing (Maybe(..))
import RemoteData exposing (WebData)


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
    , parentList = getParentName opts.parentList
    , position = opts.position
    , originalDay = (Date.toTime opts.parentList.date)
    , currentDay = (Date.toTime opts.parentList.date)
    , hasRolledOver = False
    , createdAt = (Date.toTime opts.parentList.date)
    }


getParentName parentList =
    if parentList.listType == "custom" then
        parentList.id
    else
        parentList.name


getTodoInParent todolist todo =
    if todolist.listType == "custom" then
        todo.parentList == todolist.id
    else
        todo.parentList == todolist.name


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
