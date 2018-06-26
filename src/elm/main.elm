module App exposing (..)

import Html exposing (Html, button, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (Model, initialModel, Todo, Day)
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
    = ToggleTodo String Bool
    | SetTimeAndWeek Time



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

            ToggleTodo id isCompleted ->
                let
                    todoNew t =
                        if t.id == id then
                            { t | complete = isCompleted }
                        else
                            t
                in
                    { model | todos = List.map todoNew model.todos }
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


{-| Display a single Todo. Conditionally styles it.
Updates: ToggleTodo
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
            String.concat [ "todo mt2 p1 pointer", " ", styleState ]
    in
        div
            [ class styleClasses
            , onClick (ToggleTodo todo.id (not todo.complete))
            ]
            [ text todo.name ]


{-| Displays the current week... should be able to partially apply the map...
-}
viewWeek : Model -> Html Msg
viewWeek model =
    -- div [] (List.map (viewDay model) model.currentWeek) -- this should work?
    div [ class "flex mx3" ] (List.map (\d -> div [] [ (viewDay model d.date) ]) model.currentWeek)


{-| Renders a day: the date, and the todos for the date.
TODO: run a filter with function that checks: if the todo timestamp matches the day using Date.day, Date.year
-}
viewDay : Model -> Date -> Html Msg
viewDay model date =
    let
        todosByDay =
            List.filter (taskInDate date) model.todos
    in
        div [ class "m2" ]
            [ span [ class "caps" ] [ text (dateFmt date) ]
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
