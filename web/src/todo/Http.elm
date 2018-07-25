module Todo.Http exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as JsonPipe exposing (decode, required)
import Models as Models exposing (Model, initialModel, Todo)
import RemoteData exposing (RemoteData, WebData, map)
import Msgs exposing (Msg)
import Utils exposing (buildWeek)


-- DECODERS / ENCODERS ---------------------------------------------------------


prefix =
    "/api/todos/"


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
        |> JsonPipe.required "position" Decode.int
        |> JsonPipe.required "createdAt" Decode.float
        |> JsonPipe.required "originalDay" Decode.float
        |> JsonPipe.required "currentDay" Decode.float
        |> JsonPipe.required "hasRolledOver" Decode.bool


todoEncoder : Todo -> Encode.Value
todoEncoder todo =
    let
        attributes =
            [ ( "id", Encode.int todo.id )
            , ( "isEditing", Encode.bool todo.isEditing )
            , ( "name", Encode.string todo.name )
            , ( "complete", Encode.bool todo.complete )
            , ( "parentList", Encode.string todo.parentList )
            , ( "position", Encode.int todo.position )
            , ( "createdAt", Encode.float todo.createdAt )
            , ( "originalDay", Encode.float todo.originalDay )
            , ( "currentDay", Encode.float todo.currentDay )
            , ( "hasRolledOver", Encode.bool todo.hasRolledOver )
            ]
    in
        Encode.object attributes



-- REQUESTS


createReq : Todo -> Http.Request Todo
createReq todo =
    Http.request
        { body = todoEncoder todo |> Http.jsonBody
        , expect = Http.expectJson todoDecoder
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = prefix
        , withCredentials = False
        }


updateSingleUrl : Todo -> String
updateSingleUrl todo =
    prefix ++ (toString todo.id)


updateReq : Todo -> Http.Request Todo
updateReq todo =
    Http.request
        { body = todoEncoder todo |> Http.jsonBody
        , expect = Http.expectJson todoDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = updateSingleUrl todo
        , withCredentials = False
        }


deleteSingleUrl : Todo -> String
deleteSingleUrl todo =
    prefix ++ (toString todo.id)


deleteReq : Todo -> Http.Request Todo
deleteReq todo =
    Http.request
        { body = todoEncoder todo |> Http.jsonBody
        , expect = Http.expectJson todoDecoder
        , headers = []
        , method = "DELETE"
        , timeout = Nothing
        , url = deleteSingleUrl todo
        , withCredentials = False
        }



-- PUBLIC ----------------------------------------------------------------------
-- Fetch All


fetchAllCmd : Cmd Msg
fetchAllCmd =
    Http.get prefix todosDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.HttpOnFetchTodos


{-| On fetching all todos, we set them into state, and set the currentWeek.
Also, if a todo is overdue and incomplete, move it into current day.
-}
onFetchAll : Model -> WebData (List Todo) -> ( Model, Cmd a )
onFetchAll model res =
    let
        newWeek =
            (buildWeek model.dayOffset model.timeAtLoad)

        rolledOverTodos t =
            if t.currentDay <= model.timeAtLoad && t.complete == False then
                { t
                    | currentDay = model.timeAtLoad

                    -- this is not persisting on the backend until the todo is updated or saved...
                    , hasRolledOver = True
                }
            else
                t
    in
        { model
            | todos = RemoteData.succeed (List.map rolledOverTodos (Models.maybeTodos res))
            , uuid = List.length (Models.maybeTodos res) + 1
            , currentWeek = newWeek
        }
            ! []



-- Create One Todo


createCmd : Todo -> Cmd Msg
createCmd todo =
    createReq todo
        |> Http.send Msgs.HttpOnTodoSave


onCreate : Model -> Result Http.Error Todo -> ( Model, Cmd Msg )
onCreate model res =
    case res of
        Ok res ->
            let
                newTodos =
                    RemoteData.map (\d -> d ++ [ res ]) model.todos
            in
                { model | todos = newTodos } ! []

        Err err ->
            -- FIXME - handle error.
            model ! []



-- Update One Todo


updateCmd : Todo -> Cmd Msg
updateCmd todo =
    updateReq todo
        |> Http.send Msgs.HttpOnTodoUpdate


onUpdate : Model -> Result Http.Error Todo -> ( Model, Cmd Msg )
onUpdate model res =
    let
        _ =
            Debug.log "onupdate" res
    in
        case res of
            Ok todo ->
                -- loops through all todos and replaces the one with id with the updated.
                let
                    updateTodos t =
                        if t.id == todo.id then
                            todo
                        else
                            t
                in
                    { model | todos = RemoteData.map (\l -> List.map updateTodos l) model.todos }
                        ! []

            Err error ->
                -- TODO!
                model ! []



-- Delete one Todo


deleteCmd : Todo -> Cmd Msg
deleteCmd todo =
    deleteReq todo
        |> Http.send Msgs.HttpOnTodoDelete


onDelete : Model -> Result Http.Error Todo -> ( Model, Cmd Msg )
onDelete model res =
    case res of
        Ok _ ->
            --NOTE  current JSON-api doesn't return anything for delete, but the real api will eventaully.
            -- let
            --     filterTodos todoList =
            --         List.filter (\t -> t.id /= todo.id) todoList
            -- in
            --     { model | todos = RemoteData.map filterTodos model.todos }
            --         ! []
            model ! []

        Err error ->
            -- TODO!
            model ! []
