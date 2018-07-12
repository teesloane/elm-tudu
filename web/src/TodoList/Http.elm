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



-- customList : Decode.Decoder TodoList


customList =
    JsonPipe.decode TodoListDB
        |> JsonPipe.required "hasTodos" Decode.bool
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
    let
        constructLists l =
            let
                _ =
                    Debug.log "thing is " l
            in
                TodoList l.hasTodos "" (Date.fromTime l.ts) l.name l.ts
    in
        { model
            | customLists = RemoteData.succeed (List.map constructLists (maybeTodoLists res))
        }
            ! []
