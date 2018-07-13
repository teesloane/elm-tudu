module TodoList.Http exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Json.Encode as Encode
import Models as Models exposing (Model, initialModel, Todo)
import TodoList.Model exposing (TodoList, TodoListDB, maybeTodoLists, createDefaultTodoList)
import RemoteData exposing (RemoteData, WebData, map)
import Date exposing (Date)
import Msgs exposing (Msg)


-- ENCODERS / DECODERS ---------------------------------------------------------


customListsDecoder : Decode.Decoder (List TodoListDB)
customListsDecoder =
    Decode.list customListDecoder


customListDecoder =
    JsonPipe.decode TodoListDB
        |> JsonPipe.required "hasTodos" Decode.bool
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "ts" Decode.float
        |> JsonPipe.required "id" Decode.string
        |> JsonPipe.required "listType" Decode.string


customListEncoder : TodoList -> Encode.Value
customListEncoder todoList =
    let
        attributes =
            [ ( "id", Encode.string todoList.id )
            , ( "name", Encode.string todoList.name )
            , ( "ts", Encode.float todoList.ts )
            , ( "listType", Encode.string todoList.listType )
            ]
    in
        Encode.object attributes



-- Requests
-- REQUESTS / HANDLERS ---------------------------------------------------------
-- 1. Fetch All --


fetchAllCmd : Cmd Msg
fetchAllCmd =
    Http.get "http://localhost:4000/customlists" customListsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodoLists


onFetchAll : Model -> WebData (List TodoListDB) -> ( Model, Cmd a )
onFetchAll model res =
    { model
        | customLists = RemoteData.succeed (List.map createDefaultTodoList (maybeTodoLists res))
    }
        ! []



-- 2. Create


createReq : TodoList -> Http.Request TodoListDB
createReq todoList =
    Http.request
        { body = customListEncoder todoList |> Http.jsonBody
        , expect = Http.expectJson customListDecoder
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "http://localhost:4000/todos"
        , withCredentials = False
        }


createCmd : TodoList -> Cmd Msg
createCmd todoList =
    createReq todoList
        |> Http.send Msgs.HttpOnCustomListSave


onCreate : Model -> Result Http.Error TodoListDB -> ( Model, Cmd Msg )
onCreate model res =
    case res of
        Ok res ->
            let
                newCustomLists =
                    RemoteData.map (\d -> d ++ [ (createDefaultTodoList res) ]) model.customLists
            in
                { model | customLists = newCustomLists } ! []

        Err err ->
            -- FIXME - handle error.
            model ! []
