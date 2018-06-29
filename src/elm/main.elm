module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Models exposing (Model, initialModel, Todo, TodoList, TodoList)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import Json.Decode as Json
import Dict exposing (..)
import Debug exposing (..)


-- Boot up, on load commands


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ getTime ] )


getTime : Cmd Msg
getTime =
    Task.perform SetTimeAndWeek Time.now



-- MESSAGES (possibly move this to  `models`)


type Msg
    = TodoToggleComplete Int Bool
    | SetTimeAndWeek Time
    | TodoToggleEditing Int Bool
    | TodoStopEditing Int Bool
    | TodoEditName Int String
    | TodoCreate TodoList
    | TodoUpdateNewField TodoList String
    | DragStart Todo
    | DragEnd
    | DragOver TodoList
    | Drop TodoList



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTimeAndWeek time ->
            let
                newWeek =
                    (buildWeek time)
            in
                { model
                    | timeAtLoad = time
                    , todoLists = newWeek
                }
                    ! []

        TodoToggleComplete id isCompleted ->
            let
                todoNew t =
                    if t.id == id then
                        { t | complete = isCompleted }
                    else
                        t
            in
                { model | todos = List.map todoNew model.todos }
                    ! []

        TodoToggleEditing id isEditing ->
            let
                updateTodos t =
                    if t.id == id then
                        { t | isEditing = isEditing }
                    else
                        t
            in
                { model | todos = List.map updateTodos model.todos }
                    ! []

        TodoStopEditing id isEditing ->
            let
                updateTodos t =
                    if t.id == id then
                        { t | isEditing = isEditing }
                    else
                        t
            in
                { model | todos = List.map updateTodos model.todos }
                    ! []

        TodoEditName id newChar ->
            let
                updateTodos t =
                    if t.id == id then
                        { t | name = newChar }
                    else
                        t
            in
                { model | todos = List.map updateTodos model.todos }
                    ! []

        TodoCreate todoList ->
            let
                newTodo =
                    { id = model.uuid + 1
                    , isEditing = False
                    , name = todoList.inputField
                    , complete = False
                    , parentList = todoList.name
                    , ts = (Date.toTime todoList.date)
                    }

                newTodoList =
                    { todoList
                        | inputField = ""
                        , todos = todoList.todos ++ [ newTodo ]
                    }

                -- current week is equal to the result of running inserts into the list
                newModel =
                    { model
                        | uuid = model.uuid + 1
                        , todoLists = Dict.insert todoList.name newTodoList model.todoLists
                    }
            in
                ( newModel, Cmd.none )

        TodoUpdateNewField todoList newChar ->
            let
                newTodoList =
                    { todoList | inputField = newChar }

                newModel =
                    { model
                        | todoLists = Dict.insert todoList.name newTodoList model.todoLists
                    }
            in
                newModel ! []

        DragStart todo ->
            { model
                | draggedTodo = Just todo
                , beingDragged = True
            }
                ! []

        DragEnd ->
            { model | beingDragged = False }
                ! []

        DragOver todoList ->
            { model | dragTarget = Just todoList }
                ! []

        Drop todo ->
            -- hmmmm. nested cases.
            case model.draggedTodo of
                Nothing ->
                    ( model, Cmd.none )

                Just todo ->
                    case model.dragTarget of
                        Nothing ->
                            ( model, Cmd.none )

                        Just targetDay ->
                            let
                                updateTodos t =
                                    if t.id == todo.id then
                                        { t | ts = (Date.toTime targetDay.date) }
                                    else
                                        t
                            in
                                { model
                                    | todos = List.map updateTodos model.todos
                                    , dragTarget = Nothing
                                    , beingDragged = False
                                }
                                    ! []



-- goal: not adding to another list, but changing the ts.
-- View


view : Model -> Html Msg
view model =
    div [ class "flex flex-auto pt4 justify-center" ]
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []

        -- Render a week of days.
        , div
            [ class "flex mx3" ]
            (List.map (viewTodoList model) (getSortedWeek model.todoLists))
        ]


{-| Display a single Todo. Handles conditional styling, editing state, completion state.
Updates: TodoToggleComplete, TodoToggleEditing, TodoEditName, TodoStopEditing
-}
viewTodo : Todo -> Html Msg
viewTodo todo =
    let
        styleState =
            if todo.complete then
                "todo-completed"
            else
                "todo-incomplete"

        styleClasses =
            String.concat [ "todo ", styleState ]

        todoEl =
            if todo.isEditing then
                input
                    [ value todo.name
                    , onInput (TodoEditName todo.id)
                    , onEnter (TodoStopEditing todo.id (not todo.isEditing))
                    , class "todo-input"
                    ]
                    []
            else
                div
                    [ class "flex flex-auto justify-between cursor-drag"
                    , draggable "true"
                    , onDragStart <| DragStart todo
                    ]
                    [ span
                        [ onClick (TodoToggleComplete todo.id (not todo.complete))
                        , class "todo-draggable"
                        , onDragStart <| DragStart todo
                        ]
                        [ text todo.name ]
                    , span
                        [ class "todo-edit-btn"
                        , onClick (TodoToggleEditing todo.id (not todo.isEditing))
                        ]
                        [ text "edit" ]
                    ]
    in
        div [ class styleClasses ] [ todoEl ]


{-| Creates a new todo; on render, creates a controlled input in inputFieldsByDate
-}
viewTodoNew : TodoList -> Html Msg
viewTodoNew todoList =
    div [ class "todo flex flex-auto justify-between" ]
        [ input
            [ onEnter (TodoCreate todoList)
            , value todoList.inputField
            , onInput (TodoUpdateNewField todoList)
            , class "todo-input"
            ]
            []
        ]


{-| Fill empty todo space up to max (N).
For the current day, see how many todos (t) there are, and then add N - t empty todos.
-}
viewTodoEmpty : { b | beingDragged : Bool } -> { c | todos : List a } -> Html msg
viewTodoEmpty model todoList =
    let
        maxRows =
            if model.beingDragged then
                6
            else
                7

        rowList =
            (List.range 0 (maxRows - (List.length todoList.todos)))

        renderRow _ =
            div [ class "todo" ] [ text "" ]
    in
        div [] (List.map renderRow rowList)


viewTodoDropZone : TodoList -> Html Msg
viewTodoDropZone todoList =
    div [ class "todo", onDragOver (DragOver todoList), onDrop (Drop todoList) ] [ text "" ]


{-| Renders a TodoList : the date, the todos for the date, empty slots for new todos.
-}
viewTodoList : Model -> TodoList -> Html Msg
viewTodoList model todoList =
    let
        conditionalDropZone =
            if model.beingDragged then
                viewTodoDropZone todoList
            else
                div [] []
    in
        div [ class "m2" ]
            [ span [ class "h5 caps" ] [ text todoList.name ]
            , conditionalDropZone
            , div [] (List.map viewTodo todoList.todos)
            , viewTodoNew todoList
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



-- Custom Event message for dragging


onDragStart : a -> Html.Attribute a
onDragStart msg =
    on "dragstart" <|
        Json.succeed msg


onDragOver : a -> Html.Attribute a
onDragOver msg =
    Events.onWithOptions "dragover"
        { stopPropagation = False
        , preventDefault = True
        }
    <|
        Json.succeed msg


onDrop : a -> Html.Attribute a
onDrop msg =
    Events.onWithOptions "drop"
        { stopPropagation = False
        , preventDefault = True
        }
    <|
        Json.succeed msg
