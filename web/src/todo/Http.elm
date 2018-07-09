module Todo.Http exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Models as Models exposing (Model, initialModel, Todo, TodoList)
import RemoteData exposing (WebData, map)
import Update as Msgs exposing (Msg, update)


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



-- PUBLIC ----------------------------------------------------------------------


fetchAll : Cmd Msg
fetchAll =
    Http.get "http://localhost:4000/todos" todosDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodos
