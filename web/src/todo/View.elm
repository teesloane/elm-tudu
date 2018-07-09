module Todo.View exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Events as Events exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Todo)


viewTodoState_Editing : Model -> Todo -> Html Msg
viewTodoState_Editing model todo =
    input
        [ value todo.name
        , onInput (TodoEditName todo.id)
        , onEnter (TodoStopEditing todo (not todo.isEditing))
        , class "todo todo-input"
        ]
        []
