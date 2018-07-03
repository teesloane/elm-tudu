module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import Models as Models exposing (Model, initialModel, Todo, TodoList)
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
    = TodoToggleComplete Int Bool
    | SetTimeAndWeek Time
    | TodoToggleEditing Int Bool
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

                newTodo =
                    { id = model.uuid + 1
                    , isEditing = False
                    , name = todoList.inputField
                    , complete = False
                    , parentList = todoList.name
                    , order = newTodoOrder
                    , ts = (Date.toTime todoList.date)
                    }

                cleartodoListField d =
                    if d.name == todoList.name then
                        { d | inputField = "" }
                    else
                        d
            in
                { model
                    | todos =
                        if String.isEmpty todoList.inputField then
                            model.todos
                        else
                            model.todos ++ [ newTodo ]
                    , currentWeek = List.map cleartodoListField model.currentWeek
                    , uuid = model.uuid + 1
                }
                    ! []

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


viewTodoState_Editing model todo =
    input
        [ value todo.name
        , onInput (TodoEditName todo.id)
        , onEnter (TodoStopEditing todo.id (not todo.isEditing))
        , class "todo-input"
        ]
        []


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
For the current todoList , see how many todos (t) there are, and then add N - t empty todos.
-}
viewTodoEmpty : Model -> Date -> Html msg
viewTodoEmpty model date =
    let
        maxRows =
            if model.beingDragged then
                6
            else
                7

        todosPerTodoList =
            model.todos
                |> List.filter (taskInDate date)
                |> List.length

        renderRow _ =
            div [ class "todo" ] [ text "" ]
    in
        div [] (List.map renderRow (List.range 0 (maxRows - todosPerTodoList)))


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
            , viewTodoNew todoList
            , viewTodoEmpty model todoList.date -- make a bunch of empty ones of this
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
