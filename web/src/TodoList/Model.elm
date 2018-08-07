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
    , id : String
    , isEditingName : Bool

    -- BEHOLD! Some complection!
    -- todos belong to a todo list based on their parent name.
    -- The todo LIST DAYS aren't in the database while the CUSTOM LISTS are
    -- When the app loads it generates the week (via `buildWeek`); which gives a name
    -- to each list. If a list has that name as it's parent it belongs to it.
    -- however, the custom lists can have their own name; and changing it will make the todo's
    -- that belonged to it DISSAPEAR  (>ლ) ... SO, we have the "original name " assigned.. which is
    -- kind of like an id, blah blah blah blurg.
    , originalName : String

    -- NOTE:  could use a union type for this? "custom" or "day" ?
    , listType : String
    }


type alias TodoListDB =
    { name : String
    , originalName : String
    , ts : Time
    , id : String
    , listType : String
    }


createDefaultTodoList opts =
    { hasTodos = False
    , inputField = ""
    , date = (Date.fromTime opts.ts)
    , name = opts.name
    , originalName = opts.originalName
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
