module App exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import Models as Models exposing (Model, initialModel, Todo, TodoList)
import Update as Msgs exposing (Msg, update)
import Date exposing (Date)
import Time exposing (Time)
import Task exposing (Task)
import Utils exposing (..)
import RemoteData exposing (WebData, map)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Models
import Todo.View exposing (..)


-- Boot up, on load commands


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ fetchTodos, getTime ] )



-- MESSAGES (possibly move this to  `models`)
-- View


view : Model -> Html Msg
view model =
    div [ class "flex flex-auto pt2 justify-center" ]
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []

        -- , viewTodoList model
        , div []
            -- date opts
            [ div [ class "day-advance", onClick (Msgs.OffsetDay -1) ] [ text "<" ]
            , div [ class "week-advance", onClick (Msgs.OffsetDay -5) ] [ text "<<" ]
            , div [ class "go-home-week", onClick (Msgs.OffsetDay 0) ] [ text "home" ]
            ]

        -- the actual todos --:
        , div [ class "flex mx3" ]
            (List.map (Todo.View.list model) model.currentWeek)

        -- more date ops
        , div []
            [ div [ class "day-advance", onClick (Msgs.OffsetDay 1) ] [ text ">" ]
            , div [ class "week-advance", onClick (Msgs.OffsetDay 5) ] [ text ">>" ]
            ]
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



-- COMMANDS ---


getTime : Cmd Msg
getTime =
    Task.perform Msgs.SetTimeAndWeek Time.now


fetchTodos : Cmd Msg
fetchTodos =
    Http.get "http://localhost:3000/todos" todosDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodos


todosDecoder : Decode.Decoder (List Todo)
todosDecoder =
    Decode.list todoDecoder


todoDecoder : Decode.Decoder Todo
todoDecoder =
    JsonPipe.decode Todo
        |> JsonPipe.required "id" Decode.int
        |> JsonPipe.required "isEditing" Decode.bool
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "complete" Decode.bool
        |> JsonPipe.required "parentList" Decode.string
        |> JsonPipe.required "order" Decode.int
        |> JsonPipe.required "ts" Decode.float
