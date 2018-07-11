module Todo.View exposing (single, list)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Model, Todo, TodoList)
import Date exposing (..)
import Msgs exposing (Msg)
import Todo.Drag as Drag exposing (..)
import Utils exposing (onEnter, taskInDate, parseDate)


-- Single Todo Views [editing, incomplete, complete etc] ------------------------


singleEditing : Model -> Todo -> Html Msg
singleEditing model todo =
    input
        [ value todo.name
        , onInput (Msgs.TodoEditName todo.id)
        , onEnter (Msgs.TodoStopEditing todo (not todo.isEditing))
        , class "todo todo-input"
        ]
        []


singleIncomplete : Model -> Todo -> Html Msg
singleIncomplete model todo =
    div [ class "todo todo-incomplete" ]
        [ div
            [ class "flex flex-auto justify-between cursor-drag"
            , draggable "true"
            , onClick (Msgs.TodoToggleComplete todo (not todo.complete))
            , Drag.onStart (Msgs.DragStart todo)
            , Drag.onOver (Msgs.DragOver todo)
            , Drag.onEnd (Msgs.DragEnd todo)
            ]
            [ span
                [ class "todo-draggable"
                , Drag.onStart <| Msgs.DragStart todo
                ]
                [ text todo.name ]
            , span
                [ class "todo-edit-btn"
                , onClick (Msgs.TodoToggleEditing todo.id (not todo.isEditing))
                ]
                [ text "edit" ]
            ]
        ]


{-| A todo that can be deleted because it is complete
-}
singleComplete : Model -> Todo -> Html Msg
singleComplete model todo =
    div [ class "todo " ]
        [ div
            [ class "flex flex-auto justify-between cursor-drag"
            , draggable "true"
            , onClick (Msgs.TodoToggleComplete todo (not todo.complete))
            , Drag.onStart (Msgs.DragStart todo)
            , Drag.onOver (Msgs.DragOver todo)
            , Drag.onEnd (Msgs.DragEnd todo)
            ]
            [ span
                [ onClick (Msgs.TodoToggleComplete todo (not todo.complete))
                , class "todo-draggable todo-completed"
                , Drag.onStart <| Msgs.DragStart todo
                ]
                [ text todo.name ]
            , span
                [ class "todo-delete-btn"
                , onClick (Msgs.TodoDelete todo)
                ]
                [ text "delete" ]
            ]
        ]


{-| Fill empty todo space up to max (N).
For the current todoList , see how many todos (t) there are, and then add N - t empty todos.
-}
singleEmpty : Model -> TodoList -> Html Msg
singleEmpty model todolist =
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

        renderRow _ =
            if model.beingDragged then
                singleDropZoneEmpty model todolist
            else
                div
                    [ class "todo"
                    , onClick (Msgs.TodoFocusInputFromEmpty todolist)
                    ]
                    [ text "" ]
    in
        div [] (List.map renderRow rowsToCreate)


singleNew : Model -> TodoList -> Html Msg
singleNew model todoList =
    if model.beingDragged then
        singleDropZoneEmpty model todoList
    else
        input
            [ onEnter (Msgs.TodoCreate todoList)
            , value todoList.inputField
            , id (todoList.name ++ "focus-id")
            , onInput (Msgs.TodoUpdateNewField todoList)
            , class "todo todo-input"
            ]
            []


singleDropZoneEmpty : Model -> TodoList -> Html Msg
singleDropZoneEmpty model todoList =
    let
        lastItem : Maybe Todo
        lastItem =
            (Models.getTodosInList todoList model)
                |> List.sortBy .order
                |> List.reverse
                |> List.head

        buildNewTodo order =
            { id = model.uuid + 1
            , isEditing = False
            , name = "fake!"
            , complete = False
            , parentList = todoList.name
            , order = order
            , ts = (Date.toTime todoList.date)
            }

        dropZone order =
            div
                [ class "todo todo-dropzone"
                , Drag.onOver (Msgs.DragOver (buildNewTodo order))
                , Drag.onDrop (Msgs.Drop (buildNewTodo order))
                ]
                []
    in
        case lastItem of
            Nothing ->
                div []
                    [ (dropZone 0) ]

            Just lastItem_ ->
                div []
                    -- hack
                    [ (dropZone (lastItem_.order + 1)) ]


{-| for dropping todos on top of other todos and replacing them.
could be combined with above?
-}
singleDropZone : Model -> Todo -> Html Msg
singleDropZone model todo =
    let
        dropZone =
            div
                [ class "todo todo-dropzone"
                , Drag.onOver (Msgs.DragOver todo)
                , Drag.onDrop (Msgs.Drop todo)
                ]
                []
    in
        div []
            [ dropZone
            , singleIncomplete model todo
            ]



-- FINAL VIEW ------------------------------------------------------------------


{-| Final representation of a single Todo and the states it can be in.
-}
single : Model -> Todo -> Html Msg
single model todo =
    let
        defaultRenderState =
            if todo.isEditing then
                singleEditing model todo
            else if todo.complete == False then
                singleIncomplete model todo
            else
                singleComplete model todo
    in
        case ( model.dragTarget, model.draggedTodo ) of
            ( Just dragTarget_, Just draggedTodo_ ) ->
                if dragTarget_.id == todo.id && dragTarget_.id /= draggedTodo_.id then
                    -- if branch for maybe: this is how we display drop zones
                    -- if the dragged todo is over another todo
                    singleDropZone model todo
                else
                    defaultRenderState

            _ ->
                defaultRenderState


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
    in
        div []
            [ div [ class "m2" ]
                [ div [ class "todoListName" ]
                    [ div [ class styles.day ] [ text (parseDate todoList.date "DayOfWeek") ]
                    , div [ class styles.moDayYear ] [ text (parseDate todoList.date "MoDayYear") ]
                    ]
                , div [] (List.map (single model) todosSortedAndFiltered)
                , singleNew model todoList
                , singleEmpty model todoList -- make a bunch of empty ones of this
                ]
            ]
