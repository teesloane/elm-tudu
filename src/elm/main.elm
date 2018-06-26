module App exposing (..)

import Html exposing (Html, button, div, ul, text, program)
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
    ( initialModel, Cmd.batch [ getDate, getTime ] )


getTime : Cmd Msg
getTime =
    Task.perform SetTimeAndWeek Time.now


{-| what the...
<https://stackoverflow.com/questions/37910613/how-do-i-get-the-current-date-in-elm>
-}
getDate : Cmd Msg
getDate =
    Task.perform (Just >> SetDateOnLoad) Date.now



-- MESSAGES


type Msg
    = ToggleTodo String Bool
    | SetTimeAndWeek Time
    | SetDateOnLoad (Maybe Date)



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

            SetDateOnLoad date ->
                { model | dateAtLoad = date }
                    ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []
        , viewTodoList model
        , viewWeek model
        ]


{-| Display a list of Todos.
-}
viewTodoList : Model -> Html Msg
viewTodoList model =
    div [] (List.map viewTodo model.todos)


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


{-| Displays the current week
-}
viewWeek : Model -> Html Msg
viewWeek model =
    div [] (List.map (\d -> div [] [ text (toString d.date) ]) model.currentWeek)



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


dateString : Maybe Date -> String
dateString date =
    case date of
        Nothing ->
            "No date here"

        Just date ->
            "the date is "
                ++ (toString <| Date.dayOfWeek date)
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
            [ 1, 2, 3, 4, 5 ]

        transformDays num =
            (Day False (Date.fromTime (timestamp + (toFloat (num * msInADay)))))
    in
        List.map transformDays days
