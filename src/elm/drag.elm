module Drag exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Date exposing (..)


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


onDrop : a -> Html.Attribute a
onDrop msg =
    Events.onWithOptions "drop"
        { stopPropagation = False
        , preventDefault = True
        }
    <|
        Json.succeed msg



-- Update functions


start model todo =
    { model
        | draggedTodo = Just todo
        , beingDragged = True
    }
        ! []


end model =
    { model | beingDragged = False }
        ! []


over model date =
    { model | dragTarget = Just date }
        ! []


drop model todo =
    case model.draggedTodo of
        Nothing ->
            ( model, Cmd.none )

        Just todo ->
            case model.dragTarget of
                Nothing ->
                    ( model, Cmd.none )

                Just targetDay ->
                    let
                        updateTodos t =
                            if t.id == todo.id then
                                { t | ts = (Date.toTime targetDay.date) }
                            else
                                t
                    in
                        { model
                            | todos = List.map updateTodos model.todos
                            , dragTarget = Nothing
                            , beingDragged = False
                        }
                            ! []
