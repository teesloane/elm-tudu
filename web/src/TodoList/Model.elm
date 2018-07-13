module TodoList.Model exposing (TodoList, TodoListDB, maybeTodoLists, createDefaultTodoList)

import RemoteData exposing (WebData, map)
import Date exposing (Date)
import Time exposing (Time)


type alias TodoList =
    { hasTodos : Bool
    , inputField : String
    , date : Date
    , name : String
    , ts : Time
    , id : Int
    , isEditingName : Bool

    -- NOTE:  could use a union type for this? "custom" or "day" ?
    , listType : String
    }


type alias TodoListDB =
    { name : String
    , ts : Time
    , id : Int
    , listType : String
    }


createDefaultTodoList opts =
    { hasTodos = False
    , inputField = ""
    , date = (Date.fromTime opts.ts)
    , name = opts.name
    , ts = opts.ts
    , id = opts.id
    , listType = opts.listType
    , isEditingName = False
    }


maybeTodoLists response =
    case response of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success lists ->
            lists

        RemoteData.Failure error ->
            -- FIXME HANDLE THIS CASE
            []
