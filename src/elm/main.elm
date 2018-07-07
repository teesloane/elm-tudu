module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import Models as Models exposing (Model, initialModel, Todo, TodoList)
import Dom exposing (focus)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)


-- import Debug exposing (..)

import Utils exposing (..)
import Drag as Drag exposing (..)


-- Boot up, on load commands


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ getTime ] )


getTime : Cmd Msg
getTime =
    Task.perform SetTimeAndWeek Time.now



-- MESSAGES (possibly move this to  `models`)


type Msg
    = SetTimeAndWeek Time
    | TodoToggleComplete Int Bool
    | TodoToggleEditing Int Bool
    | TodoFocusInputFromEmpty TodoList
    | TodoFocusInputResult (Result Dom.Error ())
    | TodoStopEditing Int Bool
    | TodoEditName Int String
    | TodoCreate TodoList
    | TodoUpdateNewField TodoList String
    | DragStart Todo
    | DragEnd Todo
    | DragOver Todo
    | Drop Todo



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
                    , currentWeek = newWeek
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
                newTodoOrder =
                    List.length (Models.getTodosInList todoList model)

                lastItemOrder : Maybe Todo
                lastItemOrder =
                    (Models.getTodosInList todoList model)
                        |> List.sortBy .order
                        |> List.reverse
                        |> List.head

                buildNewTodo n =
                    { id = model.uuid + 1
                    , isEditing = False
                    , name = todoList.inputField
                    , complete = False
                    , parentList = todoList.name
                    , order = n
                    , ts = (Date.toTime todoList.date)
                    }

                cleartodoListField d =
                    if d.name == todoList.name then
                        { d | inputField = "" }
                    else
                        d
            in
                case lastItemOrder of
                    Nothing ->
                        { model
                            | todos =
                                if String.isEmpty todoList.inputField then
                                    model.todos
                                else
                                    model.todos ++ [ (buildNewTodo 0) ]
                            , currentWeek = List.map cleartodoListField model.currentWeek
                            , uuid = model.uuid + 1
                        }
                            ! []

                    Just lastItemOrder_ ->
                        { model
                            | todos =
                                if String.isEmpty todoList.inputField then
                                    model.todos
                                else
                                    model.todos ++ [ (buildNewTodo (lastItemOrder_.order + 1)) ]
                            , currentWeek = List.map cleartodoListField model.currentWeek
                            , uuid = model.uuid + 1
                        }
                            ! []

        TodoFocusInputFromEmpty todoList ->
            let
                id =
                    todoList.name ++ "focus-id"
            in
                model ! [ Task.attempt TodoFocusInputResult (focus id) ]

        TodoFocusInputResult res ->
            case res of
                -- could do error handling here.
                Err (Dom.NotFound id) ->
                    model ! []

                Ok () ->
                    model ! []

        TodoUpdateNewField todoList newChar ->
            let
                updateTodoList t =
                    if t.name == todoList.name then
                        { t | inputField = newChar }
                    else
                        t
            in
                { model | currentWeek = List.map updateTodoList model.currentWeek }
                    ! []

        DragStart todo ->
            Drag.start model (Just todo)

        DragEnd todo ->
            Drag.end model (Just todo)

        DragOver todo ->
            Drag.over model (Just todo)

        Drop todo ->
            Drag.drop model todo



-- View


view : Model -> Html Msg
view model =
    div [ class "flex flex-auto pt4 justify-center" ]
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []

        -- , viewTodoList model
        , viewWeek model
        ]


{-| Display a single Todo. Handles conditional styling, editing state, completion state.
Updates: TodoToggleComplete, TodoToggleEditing, TodoEditName, TodoStopEditing
-}
viewTodo model todo =
    case model.draggedTodo of
        Nothing ->
            viewTodoState_Default model todo

        Just draggedTodo_ ->
            case model.dragTarget of
                Nothing ->
                    viewTodoState_Default model todo

                Just targetTodo_ ->
                    if todo.isEditing then
                        viewTodoState_Editing model todo
                    else if targetTodo_.id == todo.id && targetTodo_.id /= draggedTodo_.id then
                        viewTodoDropZone model todo
                    else
                        viewTodoState_Default model todo


viewTodoState_Editing : Model -> Todo -> Html Msg
viewTodoState_Editing model todo =
    input
        [ value todo.name
        , onInput (TodoEditName todo.id)
        , onEnter (TodoStopEditing todo.id (not todo.isEditing))
        , class "todo-input"
        ]
        []


viewTodoState_Default : Model -> Todo -> Html Msg
viewTodoState_Default model todo =
    let
        styleState =
            if todo.complete then
                "todo-completed"
            else
                "todo-incomplete"

        styleClasses =
            String.concat [ "todo ", styleState ]
    in
        div [ class styleClasses ]
            [ div
                [ class "flex flex-auto justify-between cursor-drag"
                , draggable "true"
                , Drag.onStart (DragStart todo)
                , Drag.onOver (DragOver todo)
                , Drag.onEnd (DragEnd todo)
                ]
                [ span
                    [ onClick (TodoToggleComplete todo.id (not todo.complete))
                    , class "todo-draggable"
                    , Drag.onStart <| DragStart todo
                    ]
                    [ text (todo.name ++ " (" ++ (toString todo.order) ++ ")") ]
                , span
                    [ class "todo-edit-btn"
                    , onClick (TodoToggleEditing todo.id (not todo.isEditing))
                    ]
                    [ text "edit" ]
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
                [ onEnter (TodoCreate todoList)
                , value todoList.inputField
                , id (todoList.name ++ "focus-id")
                , onInput (TodoUpdateNewField todoList)
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
                    , onClick (TodoFocusInputFromEmpty todolist)
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
                , Drag.onOver (DragOver todo)
                , Drag.onDrop (Drop todo)
                ]
                []
    in
        div []
            [ dropZone
            , viewTodoState_Default model todo
            ]


{-| for dropping todos over empty space at top / end of lists.
-}
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
                , Drag.onOver (DragOver (buildNewTodo order))
                , Drag.onDrop (Drop (buildNewTodo order))
                ]
                []

        _ =
            Debug.log "order is " lastItem
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
                |> List.filter (taskInDate todoList.date)
                |> List.sortBy .order
    in
        div [ class "m2" ]
            [ span [ class "h5 caps" ] [ text (dateFmt todoList.date) ]
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
