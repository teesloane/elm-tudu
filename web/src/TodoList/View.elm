module TodoList.View exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Msgs exposing (Msg)
import Date exposing (..)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import TodoList.Model exposing (TodoList)
import Todo.View exposing (single, newInput, dropZoneEmpty)
import Models exposing (Model, Todo)
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
                |> Models.maybeTodos
                |> List.filter (taskInDate todolist.date)
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
                |> Models.maybeTodos
                |> List.filter (taskInDate todoList.date)
                |> List.sortBy .order

        -- if todolist is currentDay style it nicely.
        styles =
            if todoList.date == (Date.fromTime model.timeAtLoad) then
                { day = "date-dayOfWeek--active"
                , moDayYear = "date-moDayYear--active"
                }
            else
                { day = "date-dayOfWeek"
                , moDayYear = "date-moDayYear"
                }

        todoListName =
            if todoList.listType == "custom" then
                [ div [ class styles.day ] [ text todoList.name ] ]
            else
                [ div [ class styles.day ] [ text (parseDate todoList.date "DayOfWeek") ]
                , div [ class styles.moDayYear ] [ text (parseDate todoList.date "MoDayYear") ]
                ]
    in
        div [ class "todoListColumn flex flex-auto" ]
            [ div [ class "flex flex-auto flex-column m1" ]
                [ div [ class "todoListName" ] todoListName
                , div [] (List.map (single model) todosSortedAndFiltered)
                , newInput model todoList
                , emptyTodos model todoList -- make a bunch of empty ones of this
                ]
            ]
