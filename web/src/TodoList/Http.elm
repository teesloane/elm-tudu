module TodoList.Http exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Json.Encode as Encode
import Models as Models exposing (Model, initialModel, Todo)
import TodoList.Model exposing (TodoList, TodoListDB, maybeTodoLists, createDefaultTodoList)
import RemoteData exposing (RemoteData, WebData, map)
import Msgs exposing (Msg)


-- ENCODERS / DECODERS ---------------------------------------------------------


customListsDecoder : Decode.Decoder (List TodoListDB)
customListsDecoder =
    Decode.list customListDecoder


customListDecoder : Decode.Decoder TodoListDB
customListDecoder =
    JsonPipe.decode TodoListDB
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "ts" Decode.float
        |> JsonPipe.required "id" Decode.int
        |> JsonPipe.required "listType" Decode.string


customListEncoder : TodoList -> Encode.Value
customListEncoder todoList =
    let
        attributes =
            [ ( "id", Encode.int todoList.id )
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
        , url = "http://localhost:4000/customlists"
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



-- 3. Update


updateSingleUrl : TodoList -> String
updateSingleUrl todoList =
    let
        _ =
            Debug.log "thing is " todoList.id
    in
        "http://localhost:4000/customlists/" ++ (toString todoList.id)


updateReq : TodoList -> Http.Request TodoListDB
updateReq todo =
    Http.request
        { body = customListEncoder todo |> Http.jsonBody
        , expect = Http.expectJson customListDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = updateSingleUrl todo
        , withCredentials = False
        }


updateCmd : TodoList -> Cmd Msg
updateCmd todoList =
    updateReq todoList
        |> Http.send Msgs.HttpOnCustomListUpdate


onUpdate : Model -> Result Http.Error TodoListDB -> ( Model, Cmd Msg )
onUpdate model res =
    case res of
        Ok todoList ->
            -- loops through all todos and replaces the one with id with the updated.
            let
                updateTodoList t =
                    if t.id == todoList.id then
                        createDefaultTodoList todoList
                    else
                        t
            in
                { model | customLists = RemoteData.map (\l -> List.map updateTodoList l) model.customLists }
                    ! []

        Err error ->
            -- TODO!
            model ! []
