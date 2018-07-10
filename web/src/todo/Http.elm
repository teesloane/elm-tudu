module Todo.Http exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Models as Models exposing (Model, initialModel, Todo, TodoList)
import RemoteData exposing (WebData, map)
import Msgs exposing (Msg)


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


saveTodosUrl : String
saveTodosUrl =
    "http://localhost:4000/todos"


saveTodoRequest : Todo -> Http.Request Todo
saveTodoRequest todo =
    Http.request
        { body = todoEncoder todo |> Http.jsonBody
        , expect = Http.expectJson todoDecoder
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = saveTodosUrl
        , withCredentials = False
        }



-- saveTodosCmd : Player -> Cmd Msg
-- todosEncoder : Decode.Decoder (List Todo)
-- todosEncoder todos =
--     Encode.list (List.map todoEncoder todos)


todoEncoder : Todo -> Encode.Value
todoEncoder todo =
    let
        attributes =
            [ ( "id", Encode.int todo.id )
            , ( "isEditing", Encode.bool todo.isEditing )
            , ( "name", Encode.string todo.name )
            , ( "complete", Encode.bool todo.complete )
            , ( "parentList", Encode.string todo.parentList )
            , ( "order", Encode.int todo.order )
            , ( "ts", Encode.float todo.ts )
            ]
    in
        Encode.object attributes



-- PUBLIC ----------------------------------------------------------------------


fetchAll : Cmd Msg
fetchAll =
    Http.get "http://localhost:4000/todos" todosDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodos


postTodoCmd todo =
    saveTodoRequest todo
        |> Http.send Msgs.HttpOnTodosSave
