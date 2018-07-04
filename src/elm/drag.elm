module Drag exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Date exposing (..)
import Debug exposing (..)
import Models exposing (..)


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



-- Update functions
-- intended as `import Drag as Drag` => drag.start, drag.over, drag.drop etc.


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



-- FIXME - could do something useful here.
-- end : Model -> ( Model, Cmd a )


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


{-| Changes timestamp of a todo when it's dropped
Causes it to be re-rendered in a different List.
-}
drop model todo =
    case model.draggedTodo of
        Nothing ->
            ( model, Cmd.none )

        Just draggedTodo_ ->
            case model.dragTarget of
                Nothing ->
                    ( { model | beingDragged = False }, Cmd.none )

                Just dragTarget_ ->
                    let
                        updateTodos t =
                            -- moves dropped todo into correct list / updates order.
                            if t.id == draggedTodo_.id then
                                { t
                                    | ts = dragTarget_.ts
                                    , order = dragTarget_.order
                                }
                                -- updates order for every other todo.
                            else if t.parentList == dragTarget_.parentList && t.order >= draggedTodo_.order then
                                { t | order = t.order + 1 }
                            else
                                t
                    in
                        { model
                            | todos = List.map updateTodos model.todos
                            , dragTarget = Nothing
                            , draggedTodo = Nothing
                            , beingDragged = False
                        }
                            ! []
