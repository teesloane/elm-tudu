module TodoList.Http exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Extra
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Models as Models exposing (Model, initialModel, Todo)
import TodoList.Model exposing (TodoList, maybeTodoLists)
import RemoteData exposing (RemoteData, WebData, map)
import Msgs exposing (Msg)


customListsDecoder : Decode.Decoder (List TodoList)
customListsDecoder =
    Decode.list customList



-- customList : Decode.Decoder TodoList


customList =
    JsonPipe.decode TodoList
        |> JsonPipe.required "hasTodos" Decode.bool
        |> JsonPipe.required "inputField" Decode.string
        |> JsonPipe.required "date" Json.Decode.Extra.date
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "ts" Decode.float



-- Public Reqs


fetchAllCmd : Cmd Msg
fetchAllCmd =
    Http.get "http://localhost:4000/customlists" customListsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodoLists



-- onFetchAll : Model -> WebData (List TodoList) -> ( Model, Cmd a )


onFetchAll model res =
    { model
        | customLists = RemoteData.succeed (maybeTodoLists res)
    }
        ! []
