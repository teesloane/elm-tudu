module Update exposing (..)

import Dom exposing (focus)
import RemoteData exposing (WebData, map)
import Models exposing (..)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import Todo.Drag as Drag exposing (..)
import Msgs exposing (Msg)
import Todo.Http
import TodoList.Http


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

        Msgs.TodoToggleComplete todo isCompleted ->
            let
                todoNew t =
                    if t.id == todo.id then
                        { t | complete = isCompleted }
                    else
                        t
            in
                ( { model | todos = RemoteData.map (\d -> List.map todoNew d) model.todos }
                , Todo.Http.updateCmd { todo | complete = isCompleted }
                )

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
            -- Async | Triggered by pressing "Enter" when done changing a Todo name.
            -- If the todo name is empty, we should delete it (TODO).
            let
                isEmpty =
                    String.isEmpty todo.name

                updateTodos t =
                    if t.id == todo.id then
                        { t | isEditing = isEditing }
                    else
                        t

                -- FIXME: refactor final model/this with something less verbose.
                finalUpdate todos =
                    if isEmpty then
                        List.filter (\t -> t.id /= todo.id) todos
                    else
                        List.map updateTodos todos

                finalModel =
                    { model | todos = RemoteData.map (\d -> (finalUpdate d)) model.todos }

                pickCmd =
                    if isEmpty then
                        Todo.Http.deleteCmd { todo | isEditing = False }
                    else
                        Todo.Http.createCmd { todo | isEditing = False }
            in
                ( finalModel, pickCmd )

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
                ( { model | todos = RemoteData.map filterTodos model.todos }
                , Todo.Http.deleteCmd todo
                )

        Msgs.TodoCreate todoList ->
            if String.isEmpty todoList.inputField then
                model ! []
            else
                let
                    newTodo =
                        Models.createDefaultTodo
                            { id = model.uuid + 1
                            , parentList = todoList
                            , order =
                                (Models.maybeTodos model.todos)
                                    |> List.filter (\t -> t.parentList == todoList.id)
                                    |> List.length
                            }

                    -- clear the dom input after saving the input.
                    cleartodoListField d =
                        if d.name == todoList.name then
                            { d | inputField = "" }
                        else
                            d

                    newModel =
                        -- FIXME Document this:
                        -- we actually add the newTodo only when the server responds successfully.
                        -- if we have noticeable lag we can add it locally and then de-dupe on response.
                        { model
                            | currentWeek = List.map cleartodoListField model.currentWeek
                            , uuid = model.uuid + 1
                        }
                in
                    ( newModel, Task.perform (Msgs.TodoCreateWithTime newTodo) Time.now )

        Msgs.TodoCreateWithTime todo time ->
            ( model, Todo.Http.createCmd { todo | created_at = time } )

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
            -- FIXME: this doesn't pattern match like the others below do.
            Todo.Http.onFetchAll model res

        Msgs.HttpOnTodoSave res ->
            Todo.Http.onCreate model res

        Msgs.HttpOnTodoUpdate res ->
            Todo.Http.onUpdate model res

        Msgs.HttpOnTodoDelete res ->
            Todo.Http.onDelete model res

        Msgs.HttpOnFetchTodoLists res ->
            let
                _ =
                    Debug.log "res is " res
            in
                TodoList.Http.onFetchAll model res

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
