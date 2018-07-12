module TodoList.Http exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Extra
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Models as Models exposing (Model, initialModel, Todo)
import TodoList.Model exposing (TodoList, TodoListDB, maybeTodoLists)
import RemoteData exposing (RemoteData, WebData, map)
import Date exposing (Date)
import Time exposing (Time)
import Msgs exposing (Msg)


customListsDecoder : Decode.Decoder (List TodoListDB)
customListsDecoder =
    Decode.list customList


customList =
    JsonPipe.decode TodoListDB
        |> JsonPipe.required "hasTodos" Decode.bool
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "ts" Decode.float
        |> JsonPipe.required "id" Decode.string
        |> JsonPipe.required "listType" Decode.string



-- Public Reqs


fetchAllCmd : Cmd Msg
fetchAllCmd =
    Http.get "http://localhost:4000/customlists" customListsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodoLists


onFetchAll : Model -> WebData (List TodoListDB) -> ( Model, Cmd a )
onFetchAll model res =
    let
        constructLists l =
            { hasTodos = False
            , inputField = ""
            , date = (Date.fromTime l.ts)
            , name = l.name
            , ts = l.ts
            , listType = l.listType
            , id = l.id
            }
    in
        { model
            | customLists = RemoteData.succeed (List.map constructLists (maybeTodoLists res))
        }
            ! []
