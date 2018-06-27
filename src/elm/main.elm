module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (Model, initialModel, Todo, Day)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Debug exposing (..)
import Utils exposing (..)
import Json.Decode as Json


-- Boot up, on load commands


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ getTime ] )


getTime : Cmd Msg
getTime =
    Task.perform SetTimeAndWeek Time.now



-- MESSAGES (possibly move this to  `models`)


type Msg
    = TodoToggleComplete String Bool
    | SetTimeAndWeek Time
    | TodoToggleEditing String Bool
    | TodoStopEditing String Bool
    | TodoEditName String String
    | TodoCreate Day
    | TodoUpdateNewField Date String
    | DragStart Todo
    | DragEnd
    | Drop



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "yo" (buildWeek model.timeAtLoad)
    in
        case msg of
            SetTimeAndWeek time ->
                -- todo - set the inputFieldsByDate
                let
                    newWeek =
                        (buildWeek time)
                in
                    { model
                        | timeAtLoad = time
                        , currentWeek = newWeek

                        -- , inputFieldsByDate = (buildInputs newWeek)
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

            TodoCreate day ->
                let
                    newTodo =
                        (Todo "3" False day.field False (Date.toTime day.date))

                    clearDayField d =
                        if d.date == day.date then
                            { d | field = "" }
                        else
                            d
                in
                    { model
                        | todos =
                            if String.isEmpty day.field then
                                model.todos
                            else
                                model.todos ++ [ newTodo ]
                        , currentWeek = List.map clearDayField model.currentWeek
                    }
                        ! []

            TodoUpdateNewField date newChar ->
                let
                    updateDay t =
                        if t.date == date then
                            { t | field = newChar }
                        else
                            t
                in
                    { model | currentWeek = List.map updateDay model.currentWeek }
                        ! []

            DragStart x ->
                let
                    _ =
                        Debug.log "dragging" x
                in
                    model
                        ! []

            DragEnd ->
                let
                    _ =
                        Debug.log "drag end"
                in
                    { model | beingDragged = Nothing }
                        ! []

            Drop ->
                let
                    _ =
                        Debug.log "dropping item"
                in
                    case model.beingDragged of
                        Nothing ->
                            model

                        Just todo ->
                            model



-- goal: not adding to another list, but changing the ts.
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

                    -- , onClick (TodoToggleComplete todo.id (not todo.complete))
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
viewTodoNew : Day -> Html Msg
viewTodoNew day =
    div [ class "todo flex flex-auto justify-between" ]
        [ input
            [ onEnter (TodoCreate day)
            , value day.field
            , onInput (TodoUpdateNewField day.date)
            , class "todo-input"
            ]
            []
        ]



-- viewTodoFiller : Html msg
-- viewTodoFiller model =


{-| Fill empty todo space up to max (N).
For the current day, see how many todos (t) there are, and then add N - t empty todos.
-}
viewTodoEmpty : { a | todos : List Todo } -> Date -> Html msg
viewTodoEmpty model date =
    let
        maxRows =
            10

        todosPerDay =
            model.todos
                |> List.filter (taskInDate date)
                |> List.length

        renderRow _ =
            div [ class "todo" ] [ text "" ]

        _ =
            Debug.log "todos per day " todosPerDay
    in
        div [] (List.map renderRow (List.range 0 (maxRows - todosPerDay)))


{-| Displays the current week... should be able to partially apply the map...
-}
viewWeek : Model -> Html Msg
viewWeek model =
    -- div [] (List.map (viewDay model) model.currentWeek) -- this should work?
    div [ class "flex mx3" ] (List.map (\d -> div [] [ (viewDay model d) ]) model.currentWeek)


{-| Renders a day: the date, the todos for the date, empty slots for new todos.
-}
viewDay : Model -> Day -> Html Msg
viewDay model day =
    let
        todosByDay =
            List.filter (taskInDate day.date) model.todos
    in
        div [ class "m2" ]
            [ span [ class "h5 caps" ] [ text (dateFmt day.date) ]
            , div [] (List.map viewTodo todosByDay)
            , viewTodoNew day
            , viewTodoEmpty model day.date -- make a bunch of empty ones of this
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


onDragStart msg =
    on "dragstart" <|
        Json.succeed msg
