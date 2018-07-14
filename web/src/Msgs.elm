module Msgs exposing (..)

import Dom exposing (focus)
import RemoteData exposing (WebData, map)
import Models exposing (..)
import Time exposing (Time)
import TodoList.Model exposing (TodoList, TodoListDB)
import Http


type Msg
    = SetTimeAndWeek Time
    | OffsetDay Int
      -- Todo client state
    | TodoToggleComplete Todo Bool
    | TodoToggleEditing Int Bool
    | TodoStopEditing Todo Bool
    | TodoDelete Todo
    | TodoFocusInputFromEmpty TodoList
    | TodoFocusInputResult (Result Dom.Error ())
    | TodoEditName Int String
    | TodoCreate TodoList
    | TodoCreateWithTime Todo Time
    | TodoUpdateNewField TodoList String
    | HttpOnFetchTodos (WebData (List Todo))
    | HttpOnTodoSave (Result Http.Error Todo)
    | HttpOnTodoUpdate (Result Http.Error Todo)
    | HttpOnTodoDelete (Result Http.Error Todo)
      -- customlists https
    | CustomListToggleEditing TodoList
    | CustomListCreate
    | CustomListUpdateName TodoList String
    | CustomListStopEditing TodoList
    | CustomListFocusName (Result Dom.Error ())
    | HttpOnFetchTodoLists (WebData (List TodoListDB))
    | HttpOnCustomListUpdate (Result Http.Error TodoListDB)
    | HttpOnCustomListSave (Result Http.Error TodoListDB)
      -- Drag stuff
    | DragStart Todo
    | DragEnd Todo
    | DragOver Todo
    | Drop Todo
