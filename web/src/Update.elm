module Update exposing (..)

import Dom exposing (focus)
import RemoteData exposing (WebData, map)
import Models exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import Todo.Drag as Drag exposing (..)
import Msgs exposing (Msg)
import Todo.Http
import Http


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.SetTimeAndWeek time ->
            let
                newWeek =
                    (buildWeek model.dayOffset time)
            in
                { model
                    | timeAtLoad = time
                    , currentWeek = newWeek
                }
                    ! []

        Msgs.TodoToggleComplete id isCompleted ->
            let
                todoNew t =
                    if t.id == id then
                        { t | complete = isCompleted }
                    else
                        t
            in
                { model | todos = RemoteData.map (\d -> List.map todoNew d) model.todos }
                    ! []

        Msgs.TodoToggleEditing id isEditing ->
            let
                updateTodos t =
                    if t.id == id then
                        { t | isEditing = isEditing }
                    else
                        t
            in
                { model | todos = RemoteData.map (\d -> List.map updateTodos d) model.todos }
                    ! []

        Msgs.TodoStopEditing todo isEditing ->
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

        Msgs.TodoEditName id newChar ->
            let
                updateTodos t =
                    if t.id == id then
                        { t | name = newChar }
                    else
                        t
            in
                { model | todos = RemoteData.map (\l -> List.map updateTodos l) model.todos }
                    ! []

        Msgs.TodoDelete todo ->
            let
                filterTodos todoList =
                    List.filter (\t -> t.id /= todo.id) todoList
            in
                { model | todos = RemoteData.map filterTodos model.todos }
                    ! []

        Msgs.TodoCreate todoList ->
            if String.isEmpty todoList.inputField then
                model ! []
            else
                let
                    -- get last item's order, and use it for new todo.
                    -- The maybe value here makes the rest of the function pretty ugly
                    -- everything kind of depends on what the order is... so that's weird.
                    -- not sure how to clean up yet.
                    -- could use maybe withDefault of 0, even while accessing record attribute?
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
                        , order =
                            if n == 0 then
                                0
                            else
                                n + 1
                        , ts = (Date.toTime todoList.date)
                        }

                    -- update todos in local database
                    updateTodos order =
                        if order == 0 then
                            RemoteData.map (\d -> d ++ [ (buildNewTodo order) ]) model.todos
                        else
                            RemoteData.map (\d -> d ++ [ (buildNewTodo (order + 1)) ]) model.todos

                    -- clear the dom input after saving the input.
                    cleartodoListField d =
                        if d.name == todoList.name then
                            { d | inputField = "" }
                        else
                            d
                in
                    case lastItemOrder of
                        Nothing ->
                            ( { model
                                | todos = updateTodos 0
                                , currentWeek = List.map cleartodoListField model.currentWeek
                                , uuid = model.uuid + 1
                              }
                            , Todo.Http.postTodoCmd (buildNewTodo 0)
                            )

                        Just t ->
                            ( { model
                                | todos = updateTodos t.order
                                , currentWeek = List.map cleartodoListField model.currentWeek
                                , uuid = model.uuid + 1
                              }
                            , Todo.Http.postTodoCmd (buildNewTodo (t.order + 1))
                            )

        Msgs.TodoFocusInputFromEmpty todoList ->
            let
                id =
                    todoList.name ++ "focus-id"
            in
                model ! [ Task.attempt Msgs.TodoFocusInputResult (focus id) ]

        Msgs.TodoFocusInputResult res ->
            case res of
                -- could do error handling here.
                Err (Dom.NotFound id) ->
                    model ! []

                Ok () ->
                    model ! []

        Msgs.TodoUpdateNewField todoList newChar ->
            let
                updateTodoList t =
                    if t.name == todoList.name then
                        { t | inputField = newChar }
                    else
                        t
            in
                { model | currentWeek = List.map updateTodoList model.currentWeek }
                    ! []

        Msgs.HttpOnFetchTodos res ->
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

        Msgs.HttpOnTodosSave (Ok todos) ->
            model ! []

        Msgs.HttpOnTodosSave (Err error) ->
            model ! []

        Msgs.OffsetDay day ->
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

        Msgs.DragStart todo ->
            Drag.start model (Just todo)

        Msgs.DragEnd todo ->
            Drag.end model (Just todo)

        Msgs.DragOver todo ->
            Drag.over model (Just todo)

        Msgs.Drop todo ->
            Drag.drop model todo
