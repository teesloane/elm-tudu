module App exposing (..)

import Html exposing (program)
import Models as Models exposing (Model, initialModel)
import Task exposing (Task)
import Time exposing (Time)
import Todo.Http
import TodoList.Http
import Msgs exposing (Msg)
import Update as Msgs exposing (update)
import View exposing (appView)


-- Boot up, on load commands


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ Todo.Http.fetchAllCmd, TodoList.Http.fetchAllCmd, getTime ] )



-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = appView
        , update = update
        , subscriptions = subscriptions
        }



-- COMMANDS ---


getTime : Cmd Msg
getTime =
    Task.perform Msgs.SetTimeAndWeek Time.now
