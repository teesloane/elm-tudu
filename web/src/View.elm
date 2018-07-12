module View exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Models exposing (Model)
import Todo.View
import Msgs exposing (Msg)
import Update as Msgs exposing (update)


{-| Top level app-wide view.
-}
topView : Model -> Html Msg
topView model =
    let
        arrowBigStyle =
            style [ ( "width", "32px" ) ]

        arrowSmallStyle =
            style [ ( "width", "18px" ) ]

        homeStyle =
            style [ ( "width", "18px" ), ( "margin-top", "18px" ), ( "cursor", "pointer" ) ]
    in
        div [ class "flex flex-column flex-auto justify-center" ]
            -- hack to add a stylesheet for elm reactor.
            [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
            , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []
            , div [ class "tudu-nav" ] [ text "Tudu" ]
            , div [ class "flex" ]
                [ div [ class "advancer-wrapper" ]
                    -- date opts
                    [ div [ class "day-advance", onClick (Msgs.OffsetDay -1) ]
                        [ Html.img [ arrowBigStyle, src "imgs/arrow-left-circle.svg" ] [] ]
                    , div [ class "week-advance", onClick (Msgs.OffsetDay -5) ]
                        [ Html.img [ arrowSmallStyle, src "imgs/arrow-left.svg" ] [] ]
                    , div [ class "go-home-week", onClick (Msgs.OffsetDay 0) ]
                        [ Html.img [ homeStyle, src "imgs/home.svg" ] [] ]
                    ]

                -- the actual todos --:
                , div [ class "flex flex-auto flex-column" ]
                    [ div [ class "flex flex-auto justify-around" ]
                        (List.map (Todo.View.list model) model.currentWeek)
                    , div [ class "list-divider" ] []
                    , div [ class "flex flex-auto justify-around" ]
                        (List.map (Todo.View.list model) model.currentWeek)
                    ]

                -- more date ops
                , div [ class "advancer-wrapper" ]
                    [ div [ class "day-advance", onClick (Msgs.OffsetDay 1) ]
                        [ Html.img [ arrowBigStyle, src "imgs/arrow-right-circle.svg" ] [] ]
                    , div [ class "week-advance", onClick (Msgs.OffsetDay 5) ]
                        [ Html.img [ arrowSmallStyle, src "imgs/arrow-right.svg" ] [] ]
                    ]
                ]
            ]
