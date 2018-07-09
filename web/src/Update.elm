module Update exposing (..)

import Dom exposing (focus)
import RemoteData exposing (WebData, map)
import Models exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import Drag exposing (..)


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
                { model | todos = RemoteData.map (\d -> List.map todoNew d) model.todos }
                    ! []

        TodoToggleEditing id isEditing ->
            let
                updateTodos t =
                    if t.id == id then
                        { t | isEditing = isEditing }
                    else
                        t
            in
                { model | todos = RemoteData.map (\d -> List.map updateTodos d) model.todos }
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
                { model | todos = RemoteData.map (\d -> (finalUpdate d)) model.todos }
                    ! []

        TodoEditName id newChar ->
            let
                updateTodos t =
                    if t.id == id then
                        { t | name = newChar }
                    else
                        t
            in
                { model | todos = RemoteData.map (\l -> List.map updateTodos l) model.todos }
                    ! []

        TodoDelete todo ->
            let
                filterTodos todoList =
                    List.filter (\t -> t.id /= todo.id) todoList
            in
                { model | todos = RemoteData.map filterTodos model.todos }
                    ! []

        TodoCreate todoList ->
            let
                -- create the order to assign to the new todo.
                newTodoOrder =
                    List.length (Models.getTodosInList todoList model)

                lastItemOrder : Maybe Todo
                lastItemOrder =
                    (Models.getTodosInList todoList model)
                        |> List.sortBy .order
                        |> List.reverse
                        |> List.head

                buildNewTodo n =
                    { id = model.uuid + 1 -- no good, change to uuid soon
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
                                    RemoteData.map (\d -> d) model.todos
                                else
                                    (RemoteData.map (\d -> d ++ [ (buildNewTodo 0) ]) model.todos)
                            , currentWeek = List.map cleartodoListField model.currentWeek
                            , uuid = model.uuid + 1
                        }
                            ! []

                    Just lastItemOrder_ ->
                        { model
                            | todos =
                                if String.isEmpty todoList.inputField then
                                    RemoteData.map (\d -> d) model.todos
                                else
                                    (RemoteData.map (\d -> d ++ [ (buildNewTodo (lastItemOrder_.order + 1)) ]) model.todos)
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

        HttpOnFetchTodos res ->
            let
                newWeek =
                    (buildWeek model.dayOffset model.timeAtLoad)
            in
                { model
                    | todos = res
                    , uuid = List.length (Models.maybeTodos res) + 1
                    , currentWeek = newWeek
                }
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
