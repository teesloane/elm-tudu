module Todo.Drag exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Json.Decode as Json
import Models exposing (..)
import Msgs
import RemoteData exposing (WebData, map)
import Todo.Http


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
NOTE: Decided to avoid ugly model updates "locally" to client to represent the drop;
when a successful API request will make the change anyway on response + re-render
-- FIXME: todo drop order is broken? Yes, within the same list it seems.
-}
drop : Model -> Todo -> ( Model, Cmd Msgs.Msg )
drop model todo =
    case ( model.draggedTodo, model.dragTarget ) of
        ( Just draggedTodo_, Just dragTarget_ ) ->
            let
                updatedTodo =
                    { draggedTodo_
                        | currentDay = dragTarget_.currentDay
                        , order = dragTarget_.order
                        , parentList = dragTarget_.parentList
                    }
            in
                ( model, Todo.Http.updateCmd updatedTodo )

        _ ->
            { model | draggedTodo = Nothing, dragTarget = Nothing } ! []
