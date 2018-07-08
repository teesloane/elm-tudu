module Drag exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Date exposing (..)
import Debug exposing (..)
import Models exposing (..)
import RemoteData exposing (WebData)


onStart : a -> Html.Attribute a
onStart msg =
    on "dragstart" <|
        Json.succeed msg


onOver : a -> Html.Attribute a
onOver msg =
    Events.onWithOptions "dragover"
        { stopPropagation = False
        , preventDefault = True
        }
    <|
        Json.succeed msg


onEnd : a -> Html.Attribute a
onEnd msg =
    Events.on "dragend" <|
        Json.succeed msg


onDrop : a -> Html.Attribute a
onDrop msg =
    Events.onWithOptions "drop"
        { stopPropagation = False
        , preventDefault = True
        }
    <|
        Json.succeed msg



-- Update functions -------------------------------------------------------------
-- intended as `import Drag as Drag` => drag.start, drag.over, drag.drop etc.


start : Model -> Maybe Todo -> ( Model, Cmd a )
start model todo =
    case todo of
        Nothing ->
            ( model, Cmd.none )

        todo_ ->
            { model
                | draggedTodo = todo_
                , beingDragged = True
            }
                ! []


end : Model -> Maybe Todo -> ( Model, Cmd a )
end model todoTarget =
    case model.draggedTodo of
        Nothing ->
            ( model, Cmd.none )

        Just draggedTodo_ ->
            case todoTarget of
                Nothing ->
                    ( model, Cmd.none )

                Just todoTarget_ ->
                    { model
                        | beingDragged = False
                        , dragTarget = Nothing
                        , draggedTodo = Nothing
                        , dragTargetExists = False
                    }
                        ! []


{-| On dragging over, we copy the dragTarget's order to the draggedTodo's order
This way, when it's dropped we can just have every item after it in the list update + 1.
FIXME: use maybe.map instead of having two switch cases.
-}
over : Model -> Maybe Todo -> ( Model, Cmd a )
over model targetTodo =
    case model.draggedTodo of
        Nothing ->
            ( model, Cmd.none )

        Just draggedTodo_ ->
            case targetTodo of
                Nothing ->
                    ( model, Cmd.none )

                Just targetTodo_ ->
                    let
                        todoWithNewOrder =
                            { draggedTodo_ | order = targetTodo_.order }
                    in
                        ( { model
                            | draggedTodo = (Just todoWithNewOrder)
                            , dragTarget =
                                if targetTodo_.id /= draggedTodo_.id then
                                    (Just targetTodo_)
                                else
                                    Nothing

                            -- Nothing
                            , dragTargetExists = True
                          }
                        , Cmd.none
                        )


{-| Changes timestamp of dropped todo
Causes it to be re-rendered in a different List.
FIXME: use maybe.map instead of having two switch cases.
-}
drop : Model -> Todo -> ( Model, Cmd a )
drop model todo =
    case model.draggedTodo of
        Nothing ->
            ( model, Cmd.none )

        Just draggedTodo_ ->
            case model.dragTarget of
                Nothing ->
                    ( { model | beingDragged = False }, Cmd.none )

                Just dragTarget_ ->
                    case model.todos of
                        RemoteData.NotAsked ->
                            model ! []

                        RemoteData.Loading ->
                            model ! []

                        RemoteData.Failure err ->
                            model ! []

                        RemoteData.Success todos_ ->
                            let
                                updateTodos i t =
                                    -- if iterated t.id is the dragged todo, we need to update order, ts, and parentlist
                                    if t.id == draggedTodo_.id then
                                        { t
                                            | ts = dragTarget_.ts
                                            , order = dragTarget_.order
                                            , parentList = dragTarget_.parentList
                                        }
                                        -- if todos are in list where draggedTodo was Dropped, update the order for them.
                                    else if t.parentList == dragTarget_.parentList && t.order >= draggedTodo_.order then
                                        { t | order = t.order + 1 }
                                    else
                                        t
                            in
                                model ! []



-- { model
--     | todos = List.indexedMap updateTodos (List.sortBy .parentList todos_)
--     , dragTarget = Nothing
--     , draggedTodo = Nothing
--     , beingDragged = False
-- }
--     ! []
