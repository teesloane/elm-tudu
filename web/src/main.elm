module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import Models as Models exposing (Model, initialModel, Todo, TodoList)
import Dom exposing (focus)
import Maybe exposing (..)
import Update as Msgs exposing (Msg, update)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import Drag as Drag exposing (..)
import RemoteData exposing (WebData, map)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Models


-- Boot up, on load commands


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ fetchTodos, getTime ] )



-- MESSAGES (possibly move this to  `models`)
-- View


view : Model -> Html Msg
view model =
    div [ class "flex flex-auto pt2 justify-center" ]
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []

        -- , viewTodoList model
        , div []
            [ div [ class "day-advance", onClick (Msgs.OffsetDay -1) ] [ text "<" ]
            , div [ class "week-advance", onClick (Msgs.OffsetDay -5) ] [ text "<<" ]
            , div [ class "go-home-week", onClick (Msgs.OffsetDay 0) ] [ text "home" ]
            ]
        , viewWeek model
        , div []
            [ div [ class "day-advance", onClick (Msgs.OffsetDay 1) ] [ text ">" ]
            , div [ class "week-advance", onClick (Msgs.OffsetDay 5) ] [ text ">>" ]
            ]
        ]


{-| Display a single Todo, in whatever state it may be in.
-}
viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
    let
        defaultRenderState =
            if todo.isEditing then
                viewTodoState_Editing model todo
            else if todo.complete == False then
                viewTodoState_Incomplete model todo
            else
                viewTodoState_Complete model todo
    in
        case ( model.dragTarget, model.draggedTodo ) of
            ( Just dragTarget_, Just draggedTodo_ ) ->
                if dragTarget_.id == todo.id && dragTarget_.id /= draggedTodo_.id then
                    -- if branch for maybe: this is how we display drop zones
                    -- if the dragged todo is over another todo
                    viewTodoDropZone model todo
                else
                    defaultRenderState

            _ ->
                defaultRenderState


viewTodoState_Editing : Model -> Todo -> Html Msg
viewTodoState_Editing model todo =
    input
        [ value todo.name
        , onInput (Msgs.TodoEditName todo.id)
        , onEnter (Msgs.TodoStopEditing todo (not todo.isEditing))
        , class "todo todo-input"
        ]
        []


viewTodoState_Incomplete : Model -> Todo -> Html Msg
viewTodoState_Incomplete model todo =
    div [ class "todo todo-incomplete" ]
        [ div
            [ class "flex flex-auto justify-between cursor-drag"
            , draggable "true"
            , Drag.onStart (Msgs.DragStart todo)
            , Drag.onOver (Msgs.DragOver todo)
            , Drag.onEnd (Msgs.DragEnd todo)
            ]
            [ span
                [ onClick (Msgs.TodoToggleComplete todo.id (not todo.complete))
                , class "todo-draggable"
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
viewTodoState_Complete : Model -> Todo -> Html Msg
viewTodoState_Complete model todo =
    div [ class "todo " ]
        [ div
            [ class "flex flex-auto justify-between cursor-drag"
            , draggable "true"
            , Drag.onStart (Msgs.DragStart todo)
            , Drag.onOver (Msgs.DragOver todo)
            , Drag.onEnd (Msgs.DragEnd todo)
            ]
            [ span
                [ onClick (Msgs.TodoToggleComplete todo.id (not todo.complete))
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


{-| TodoBlock capable of creating new Todos.
on render, corresponds to the inputField for the parent todolist.
If user is dragging, will instead show an empty Drop zone.
-}
viewTodoNew : Model -> TodoList -> Html Msg
viewTodoNew model todoList =
    if model.beingDragged then
        viewTodoDropZoneEmpty model todoList
    else
        div [ class "todo flex flex-auto justify-between" ]
            [ input
                [ onEnter (Msgs.TodoCreate todoList)
                , value todoList.inputField
                , id (todoList.name ++ "focus-id")
                , onInput (Msgs.TodoUpdateNewField todoList)
                , class "todo-input"
                ]
                []
            ]


{-| Fill empty todo space up to max (N).
For the current todoList , see how many todos (t) there are, and then add N - t empty todos.
-}
viewTodoEmpty : Model -> TodoList -> Html Msg
viewTodoEmpty model todolist =
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
                viewTodoDropZoneEmpty model todolist
            else
                div
                    [ class "todo"
                    , onClick (Msgs.TodoFocusInputFromEmpty todolist)
                    ]
                    [ text "" ]
    in
        div [] (List.map renderRow rowsToCreate)


{-| for dropping todos on top of other todos and replacing them.
-}
viewTodoDropZone : Model -> Todo -> Html Msg
viewTodoDropZone model todo =
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
            , viewTodoState_Incomplete model todo
            ]


{-| for dropping todos over empty space at top / end of lists.
-}
viewTodoDropZoneEmpty : Model -> TodoList -> Html Msg
viewTodoDropZoneEmpty model todoList =
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


{-| Displays the current week... should be able to partially apply the map...
FIXME: remove lambda
-}
viewWeek : Model -> Html Msg
viewWeek model =
    div [ class "flex mx3" ] (List.map (\d -> div [] [ (viewTodoList model d) ]) model.currentWeek)


{-| Renders a day: the date, the todos for the date, empty slots for new todos.
-}
viewTodoList : Model -> TodoList -> Html Msg
viewTodoList model todoList =
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
        div [ class "m2" ]
            [ div [ class "todoListName" ]
                [ div [ class styles.day ] [ text (parseDate todoList.date "DayOfWeek") ]
                , div [ class styles.moDayYear ] [ text (parseDate todoList.date "MoDayYear") ]
                ]
            , div [] (List.map (viewTodo model) todosSortedAndFiltered)
            , viewTodoNew model todoList
            , viewTodoEmpty model todoList -- make a bunch of empty ones of this
            ]



-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- COMMANDS ---


getTime : Cmd Msg
getTime =
    Task.perform Msgs.SetTimeAndWeek Time.now


fetchTodos : Cmd Msg
fetchTodos =
    Http.get "http://localhost:3000/todos" todosDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodos


todosDecoder : Decode.Decoder (List Todo)
todosDecoder =
    Decode.list todoDecoder


todoDecoder : Decode.Decoder Todo
todoDecoder =
    JsonPipe.decode Todo
        |> JsonPipe.required "id" Decode.int
        |> JsonPipe.required "isEditing" Decode.bool
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "complete" Decode.bool
        |> JsonPipe.required "parentList" Decode.string
        |> JsonPipe.required "order" Decode.int
        |> JsonPipe.required "ts" Decode.float
