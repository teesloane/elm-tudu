module Utils exposing (..)

import Date exposing (Date)
import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events exposing (..)
import Models exposing (Model, initialModel)
import Todo.Model exposing (Todo)
import Json.Decode as Json
import Date exposing (Date)


msInADay : Int
msInADay =
    86400000


buildWeek dayOffset timestamp =
    -- TODO FIX THIS
    --TODO: Move to Model for todo.
    let
        days =
            [ 0, 1, 2, 3, 4 ]

        buildTodoLists num =
            let
                daysOffset =
                    toFloat (dayOffset * msInADay)

                n_date =
                    (Date.fromTime (timestamp + (toFloat (num * msInADay)) + daysOffset))

                newTodoList =
                    { hasTodos = False
                    , inputField = ""
                    , name = (parseDate n_date "Full")
                    , date = n_date
                    , ts = (Date.toTime n_date)
                    , listType = "day"
                    , id = toString num
                    , isEditingName = False
                    }
            in
                newTodoList
    in
        List.map buildTodoLists days


{-| Return true if a todo's due date belongs to a Day
TODO: Move to Model for todo.
-}
taskInDate : Date -> Todo -> Bool
taskInDate date todo =
    let
        todoYear =
            todo.currentDay |> Date.fromTime |> Date.year

        todoMonth =
            todo.currentDay |> Date.fromTime |> Date.month

        todoDay =
            todo.currentDay |> Date.fromTime |> Date.day
    in
        if todoDay == (Date.day date) && (Date.year date) == todoYear && (Date.month date) == todoMonth then
            True
        else
            False


parseDate : Date -> String -> String
parseDate date shape =
    case shape of
        "DayOfWeek" ->
            (toString <| Date.dayOfWeek date)

        "DayNum" ->
            (toString <| Date.day date)

        "MoDayYear" ->
            ""
                ++ (toString <| Date.day date)
                ++ " "
                ++ (toString <| Date.month date)
                ++ " "
                ++ (toString <| Date.year date)

        "Full" ->
            (toString <| Date.dayOfWeek date)
                ++ " "
                ++ (toString <| Date.day date)
                ++ " "
                ++ (toString <| Date.month date)

        _ ->
            ""


onEnter : a -> Html.Attribute a
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
