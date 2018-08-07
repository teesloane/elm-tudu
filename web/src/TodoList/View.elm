module TodoList.View exposing (..)

import Date exposing (..)
import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg)
import Todo.Model exposing (..)
import Todo.Model exposing (Todo, maybeTodos)
import Todo.View exposing (single, newInput, dropZoneEmpty)
import TodoList.Model exposing (TodoList)
import Utils exposing (onEnter, taskInDate, parseDate)


{-| Generates empty todo slots to keep lists at an even length.
-}
emptyTodos : Model -> TodoList -> Html Msg
emptyTodos model todolist =
    let
        maxRows =
            7

        todosPerTodoList =
            model.todos
                |> Todo.Model.maybeTodos
                |> List.filter (Todo.Model.getTodoInParent todolist)
                |> List.length

        rowsToCreate =
            (List.range 0 (maxRows - todosPerTodoList))

        renderRow _ idx =
            if model.beingDragged then
                dropZoneEmpty model todolist idx
            else
                div
                    [ class "todo", onClick (Msgs.TodoFocusInputFromEmpty todolist) ]
                    [ text "" ]
    in
        div [] (List.indexedMap renderRow rowsToCreate)


{-| A list of todos. Could be for Date based todos or customLists
-}
list : Model -> TodoList -> Html Msg
list model todoList =
    let
        todosSortedAndFiltered =
            model.todos
                |> Todo.Model.maybeTodos
                |> List.filter (Todo.Model.getTodoInParent todoList)
                |> List.sortBy .position

        styles =
            if todoList.date == (Date.fromTime model.timeAtLoad) then
                { day = "date-dayOfWeek--active"
                , moDayYear = "date-moDayYear--active"
                }
            else
                { day = "date-dayOfWeek"
                , moDayYear = "date-moDayYear"
                }

        deleteListBtn =
            if todoList.listType == "custom" then
                div [ class "right p1 pointer", onClick (Msgs.CustomListDelete todoList) ]
                    [ text "x" ]
            else
                div [] []

        todoListName =
            if todoList.listType == "custom" && todoList.isEditingName == True then
                [ input
                    [ onInput (Msgs.CustomListUpdateName todoList)
                    , onEnter (Msgs.CustomListStopEditing todoList)
                    , class "date-dayOfWeek--input"
                    , id (todoList.name ++ toString todoList.id)
                    , value (todoList.name)
                    ]
                    []
                ]
            else if todoList.listType == "custom" then
                [ div
                    [ class styles.day
                    , onDoubleClick (Msgs.CustomListToggleEditing todoList)
                    ]
                    [ text todoList.name ]
                ]
            else
                [ div [ class styles.day ] [ text (parseDate todoList.date "DayOfWeek") ]
                , div [ class styles.moDayYear ] [ text (parseDate todoList.date "MoDayYear") ]
                ]
    in
        div [ class "todoListColumn" ]
            [ div [ class "flex flex-auto flex-column m1" ]
                [ div []
                    [ deleteListBtn
                    , div [ class "todoListName" ] todoListName
                    ]
                , div [] (List.map (single model) todosSortedAndFiltered)
                , newInput model todoList
                , emptyTodos model todoList -- make a bunch of empty ones of this
                ]
            ]
