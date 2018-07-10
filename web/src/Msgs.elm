module Msgs exposing (..)

import Dom exposing (focus)
import RemoteData exposing (WebData, map)
import Models exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import Todo.Drag as Drag exposing (..)
import Http


type Msg
    = SetTimeAndWeek Time
    | TodoToggleComplete Int Bool
    | TodoToggleEditing Int Bool
    | TodoStopEditing Todo Bool
    | TodoFocusInputFromEmpty TodoList
    | TodoDelete Todo
    | TodoFocusInputResult (Result Dom.Error ())
    | TodoEditName Int String
    | TodoCreate TodoList
    | TodoUpdateNewField TodoList String
    | HttpOnFetchTodos (WebData (List Todo))
    | HttpOnTodosSave (Result Http.Error Todo)
    | OffsetDay Int
    | DragStart Todo
    | DragEnd Todo
    | DragOver Todo
    | Drop Todo
