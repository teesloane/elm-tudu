module View exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Models exposing (Model)
import Todo.View
import Update as Msgs exposing (Msg, update)


{-| Top level app-wide view.
-}
topView : Model -> Html Msg
topView model =
    div [ class "flex flex-auto pt2 justify-center" ]
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []

        -- , viewTodoList model
        , div []
            -- date opts
            [ div [ class "day-advance", onClick (Msgs.OffsetDay -1) ] [ text "<" ]
            , div [ class "week-advance", onClick (Msgs.OffsetDay -5) ] [ text "<<" ]
            , div [ class "go-home-week", onClick (Msgs.OffsetDay 0) ] [ text "home" ]
            ]

        -- the actual todos --:
        , div [ class "flex mx3" ]
            (List.map (Todo.View.list model) model.currentWeek)

        -- more date ops
        , div []
            [ div [ class "day-advance", onClick (Msgs.OffsetDay 1) ] [ text ">" ]
            , div [ class "week-advance", onClick (Msgs.OffsetDay 5) ] [ text ">>" ]
            ]
        ]
