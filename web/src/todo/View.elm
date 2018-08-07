module Todo.View exposing (single, newInput, dropZoneEmpty)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg)
import Todo.Drag as Drag exposing (..)
import Todo.Model exposing (Todo, maybeTodos, getTodoInParent)
import TodoList.Model exposing (TodoList)
import Utils exposing (onEnter, taskInDate, parseDate)


-- Single Todo Views [editing, incomplete, complete etc] ------------------------


editing : Model -> Todo -> Html Msg
editing model todo =
    input
        [ value todo.name
        , onInput (Msgs.TodoEditName todo.id)
        , onEnter (Msgs.TodoStopEditing todo (not todo.isEditing))
        , class "todo-input"
        ]
        []


incomplete : Model -> Todo -> Html Msg
incomplete model todo =
    div [ class "todo todo-incomplete" ]
        [ div
            [ class "flex flex-auto justify-between cursor-drag"
            , draggable "true"
            , Drag.onStart (Msgs.DragStart todo)
            , Drag.onOver (Msgs.DragOver todo)
            , Drag.onEnd (Msgs.DragEnd todo)
            ]
            [ span
                [ class "todo-draggable"
                , Drag.onStart <| Msgs.DragStart todo
                , onClick (Msgs.TodoToggleComplete todo (not todo.complete))
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
complete : Model -> Todo -> Html Msg
complete model todo =
    div [ class "todo " ]
        [ div
            [ class "flex flex-auto justify-between cursor-drag"
            , draggable "true"
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


newInput : Model -> TodoList -> Html Msg
newInput model todoList =
    if model.beingDragged then
        dropZoneEmpty model todoList 0
    else
        input
            [ onEnter (Msgs.TodoCreate todoList)
            , value todoList.inputField
            , id (todoList.name ++ "focus-id")
            , onInput (Msgs.TodoUpdateNewField todoList)
            , class "todo-input"
            ]
            []


{-| Displays an empty drop zone for a dragged Todo.
Basically, any empty slot via emptyTodos can render this.
NOTE: Hack: we create a fake / empty todo so that we can pass
this to Update so that our Drag.onDrop msg works.
-}
dropZoneEmpty : Model -> TodoList -> Int -> Html Msg
dropZoneEmpty model todoList idx =
    let
        _ =
            Debug.log "idx is " idx

        lastItem : Maybe Todo
        lastItem =
            (maybeTodos model.todos)
                |> List.filter (Todo.Model.getTodoInParent todoList)
                |> List.sortBy .position
                |> List.reverse
                |> List.head

        buildNewTodo n =
            Todo.Model.createDefaultTodo
                { id = model.uuid + 1
                , parentList = todoList
                , position = n
                }

        dropZone position =
            div
                [ class "todo todo-dropzone"
                , Drag.onOver (Msgs.DragOver (buildNewTodo position))
                , Drag.onDrop (Msgs.Drop (buildNewTodo position))
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
                    [ (dropZone (lastItem_.position + 1)) ]


{-| for dropping todos on top of other todos and replacing them.
could be combined with above?
-}
dropZone : Model -> Todo -> Html Msg
dropZone model todo =
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
            , incomplete model todo
            ]



-- FINAL VIEW ------------------------------------------------------------------


{-| Final representation of a single Todo and the states it can be in.
-}
single : Model -> Todo -> Html Msg
single model todo =
    let
        defaultRenderState =
            if todo.isEditing then
                editing model todo
            else if todo.complete == False then
                incomplete model todo
            else
                complete model todo
    in
        -- We are dragging something/ there is a drag target.
        case ( model.dragTarget, model.draggedTodo ) of
            ( Just dragTarget_, Just draggedTodo_ ) ->
                if dragTarget_.id == todo.id && dragTarget_.id /= draggedTodo_.id then
                    -- if branch for maybe: this is how we display drop zones
                    -- if the dragged todo is over another todo
                    dropZone model todo
                else
                    defaultRenderState

            _ ->
                defaultRenderState
