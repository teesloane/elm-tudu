module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (Model, initialModel, Todo, Day)
import Json.Decode as Json
import Maybe exposing (Maybe(..))
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Debug exposing (..)


-- Constants


msInADay : Int
msInADay =
    86400000



-- Boot up, on load commands


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ getTime ] )


getTime : Cmd Msg
getTime =
    Task.perform SetTimeAndWeek Time.now



-- MESSAGES


type Msg
    = TodoToggleComplete String Bool
    | SetTimeAndWeek Time
    | TodoToggleEditing String Bool
    | TodoStopEditing String Bool
    | TodoEditName String String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "yo" (buildWeek model.timeAtLoad)
    in
        case msg of
            SetTimeAndWeek time ->
                { model | timeAtLoad = time, currentWeek = (buildWeek time) }
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



-- VIEW


view : Model -> Html Msg
view model =
    div []
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
                    , class "todo todo-editing"
                    ]
                    []
            else
                div [ class "flex flex-auto justify-between" ]
                    [ span
                        [ onClick (TodoToggleComplete todo.id (not todo.complete)) ]
                        [ text todo.name ]
                    , span
                        [ onClick (TodoToggleEditing todo.id (not todo.isEditing)) ]
                        [ text "edit" ]
                    ]
    in
        div [ class styleClasses ] [ todoEl ]


{-| Displays the current week... should be able to partially apply the map...
-}
viewWeek : Model -> Html Msg
viewWeek model =
    -- div [] (List.map (viewDay model) model.currentWeek) -- this should work?
    div [ class "flex mx3" ] (List.map (\d -> div [] [ (viewDay model d.date) ]) model.currentWeek)


{-| Renders a day: the date, and the todos for the date.
-}
viewDay : Model -> Date -> Html Msg
viewDay model date =
    let
        todosByDay =
            List.filter (taskInDate date) model.todos
    in
        div [ class "m2" ]
            [ span [ class "h5 caps" ] [ text (dateFmt date) ]
            , div [] (List.map viewTodo todosByDay)
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



-- Functions


dateFmt : Date -> String
dateFmt date =
    (toString <| Date.dayOfWeek date)
        ++ " "
        ++ (toString <| Date.day date)
        ++ " "
        ++ (toString <| Date.month date)
        ++ " "
        ++ (toString <| Date.year date)


{-| Construct a list of days
-}
buildWeek : Time -> List Day
buildWeek timestamp =
    let
        days =
            [ 0, 1, 2, 3, 4, 5 ]

        transformDays num =
            (Day False (Date.fromTime (timestamp + (toFloat (num * msInADay)))))
    in
        List.map transformDays days


{-| Return true if a todo's due date belongs to a Day
-}
taskInDate : Date -> Todo -> Bool
taskInDate date todo =
    let
        todoYear =
            todo.ts |> Date.fromTime |> Date.year

        todoMonth =
            todo.ts |> Date.fromTime |> Date.month

        todoDay =
            todo.ts |> Date.fromTime |> Date.day
    in
        if todoDay == (Date.day date) && (Date.year date) == todoYear && (Date.month date) == todoMonth then
            True
        else
            False


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    -- stolen from https://github.com/evancz/elm-todomvc/blob/166e5f2afc704629ee6d03de00deac892dfaeed0/Todo.elm#L237-L246
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)
