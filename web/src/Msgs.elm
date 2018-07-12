module Msgs exposing (..)

import Dom exposing (focus)
import RemoteData exposing (WebData, map)
import Models exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import TodoList.Model exposing (TodoList)
import Http


type Msg
    = SetTimeAndWeek Time
    | TodoToggleComplete Todo Bool
    | TodoToggleEditing Int Bool
    | TodoStopEditing Todo Bool
    | TodoFocusInputFromEmpty TodoList
    | TodoDelete Todo
    | TodoFocusInputResult (Result Dom.Error ())
    | TodoEditName Int String
    | TodoCreate TodoList
    | TodoCreateWithTime Todo Time
    | TodoUpdateNewField TodoList String
      -- todo https
    | HttpOnFetchTodos (WebData (List Todo))
    | HttpOnTodoSave (Result Http.Error Todo)
    | HttpOnTodoUpdate (Result Http.Error Todo)
    | HttpOnTodoDelete (Result Http.Error Todo)
      -- customlists https
    | HttpOnFetchTodoLists (WebData (List TodoList))
    | OffsetDay Int
      -- Drag stuff
    | DragStart Todo
    | DragEnd Todo
    | DragOver Todo
    | Drop Todo
