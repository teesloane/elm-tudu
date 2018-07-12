module TodoList.Model exposing (TodoList, TodoListDB, maybeTodoLists)

import RemoteData exposing (WebData, map)
import Date exposing (Date)
import Time exposing (Time)


type alias TodoList =
    { hasTodos : Bool
    , inputField : String
    , date : Date
    , name : String
    , ts : Time
    }


type alias TodoListDB =
    { hasTodos : Bool
    , name : String
    , ts : Time
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


createDefaultTodoList opts =
    { hasTodos = False
    , inputField = ""
    , date = opts.date
    , name = opts.name
    , ts = opts.ts
    }
