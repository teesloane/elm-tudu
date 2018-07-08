module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import Models as Models exposing (Model, initialModel, Todo, TodoList)
import Dom exposing (focus)
import Maybe exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import Drag as Drag exposing (..)


-- import Debug exposing (..)
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
    | TodoStopEditing Todo Bool
    | TodoFocusInputFromEmpty TodoList
    | TodoDelete Todo
    | TodoFocusInputResult (Result Dom.Error ())
    | TodoEditName Int String
    | TodoCreate TodoList
    | TodoUpdateNewField TodoList String
    | OffsetDay Int
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
                    (buildWeek model.dayOffset time)
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

        TodoStopEditing todo isEditing ->
            -- capable of editing or deleting if the edited string is empty.
            let
                isEmpty =
                    String.isEmpty todo.name

                updateTodos t =
                    if t.id == todo.id then
                        { t | isEditing = isEditing }
                    else
                        t

                finalUpdate todos =
                    if isEmpty then
                        List.filter (\t -> t.id /= todo.id) todos
                    else
                        List.map updateTodos todos
            in
                { model | todos = finalUpdate model.todos }
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

        TodoDelete todo ->
            { model | todos = List.filter (\t -> t.id /= todo.id) model.todos }
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

        OffsetDay day ->
            let
                newOffset =
                    if day == 0 then
                        -- "0" == "return home via offsets."
                        0
                    else
                        model.dayOffset + day
            in
                { model
                    | dayOffset = newOffset
                    , currentWeek = (buildWeek newOffset model.timeAtLoad)
                }
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
    div [ class "flex flex-auto pt2 justify-center" ]
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []

        -- , viewTodoList model
        , div []
            [ div [ class "day-advance", onClick (OffsetDay -1) ] [ text "<" ]
            , div [ class "week-advance", onClick (OffsetDay -5) ] [ text "<<" ]
            , div [ class "go-home-week", onClick (OffsetDay 0) ] [ text "home" ]
            ]
        , viewWeek model
        , div []
            [ div [ class "day-advance", onClick (OffsetDay 1) ] [ text ">" ]
            , div [ class "week-advance", onClick (OffsetDay 5) ] [ text ">>" ]
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
        , onInput (TodoEditName todo.id)
        , onEnter (TodoStopEditing todo (not todo.isEditing))
        , class "todo todo-input"
        ]
        []


viewTodoState_Incomplete : Model -> Todo -> Html Msg
viewTodoState_Incomplete model todo =
    div [ class "todo todo-incomplete" ]
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
                [ text todo.name ]
            , span
                [ class "todo-edit-btn"
                , onClick (TodoToggleEditing todo.id (not todo.isEditing))
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
            , Drag.onStart (DragStart todo)
            , Drag.onOver (DragOver todo)
            , Drag.onEnd (DragEnd todo)
            ]
            [ span
                [ onClick (TodoToggleComplete todo.id (not todo.complete))
                , class "todo-draggable todo-completed"
                , Drag.onStart <| DragStart todo
                ]
                [ text todo.name ]
            , span
                [ class "todo-delete-btn"
                , onClick (TodoDelete todo)
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
                , Drag.onOver (DragOver (buildNewTodo order))
                , Drag.onDrop (Drop (buildNewTodo order))
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
