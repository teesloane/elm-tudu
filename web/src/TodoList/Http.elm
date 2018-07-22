module TodoList.Http exposing (..)

import Dom exposing (focus)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Json.Encode as Encode
import Models as Models exposing (Model, initialModel, Todo)
import Msgs exposing (Msg)
import RemoteData exposing (RemoteData, WebData, map)
import Task exposing (Task)
import TodoList.Model exposing (TodoList, TodoListDB, maybeTodoLists, createDefaultTodoList)


-- ENCODERS / DECODERS ---------------------------------------------------------


customListsDecoder : Decode.Decoder (List TodoListDB)
customListsDecoder =
    Decode.list customListDecoder


customListDecoder : Decode.Decoder TodoListDB
customListDecoder =
    JsonPipe.decode TodoListDB
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "originalName" Decode.string
        |> JsonPipe.required "ts" Decode.float
        |> JsonPipe.required "id" Decode.int
        |> JsonPipe.required "listType" Decode.string


customListEncoder : TodoList -> Encode.Value
customListEncoder todoList =
    let
        attributes =
            [ ( "id", Encode.int todoList.id )
            , ( "name", Encode.string todoList.name )
            , ( "originalName", Encode.string todoList.originalName )
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
    Http.get "http://localhost:8001/customlists" customListsDecoder
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
        , url = "http://localhost:8001/customlists"
        , withCredentials = False
        }


createCmd : TodoList -> Cmd Msg
createCmd todoList =
    createReq todoList
        |> Http.send Msgs.HttpOnCustomListSave


onCreate : Model -> Result Http.Error TodoListDB -> ( Model, Cmd Msg )
onCreate model res =
    case res of
        -- Creates a new todolist and starts editing the name immediately.
        Ok res ->
            let
                buildNewList r =
                    -- annoying hacking around the use of createDefaultTodoList
                    let
                        new =
                            createDefaultTodoList r
                    in
                        { new | isEditingName = True }

                newCustomLists =
                    RemoteData.map (\d -> d ++ [ (buildNewList res) ]) model.customLists

                focusId =
                    res.name ++ toString res.id
            in
                ( { model | customLists = newCustomLists }
                , Task.attempt Msgs.CustomListFocusName (focus focusId)
                )

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
        "http://localhost:8001/customlists/" ++ (toString todoList.id)


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
