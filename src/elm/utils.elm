module Utils exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Models exposing (Model, initialModel, Todo, Day)
import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events exposing (..)
import Models exposing (Model, initialModel, Todo, Day)
import Json.Decode as Json
import Date exposing (Date)
import Time exposing (Time)


msInADay : Int
msInADay =
    86400000


{-| Creates a List of Day Types.
-}



-- buildWeek : Time -> List Day


buildWeek timestamp =
    let
        days =
            [ 0, 1, 2, 3, 4 ]

        transformDays num =
            (Day False "" (Date.fromTime (timestamp + (toFloat (num * msInADay)))))
    in
        List.map transformDays days



-- takes a list of days and returns a dict of day to strings.
-- buildInputs currentWeek =
--     List.map (\day -> ( day, "" )) currentWeek
--         |> Dict.fromList
-- Dict.fromList ( currentWeek, "" )


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


dateFmt : Date -> String
dateFmt date =
    (toString <| Date.dayOfWeek date)
        ++ " "
        ++ (toString <| Date.day date)
        ++ " "
        ++ (toString <| Date.month date)
        ++ " "
        ++ (toString <| Date.year date)


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
