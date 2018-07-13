module View exposing (..)

import Html exposing (Html, button, input, div, ul, text, program, span)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Models exposing (Model)
import TodoList.View
import TodoList.Model exposing (maybeTodoLists)
import Msgs exposing (Msg)


iconBig : Html.Attribute msg
iconBig =
    style [ ( "width", "32px" ) ]


iconSmall : Html.Attribute msg
iconSmall =
    style [ ( "width", "16px" ) ]


dateNavigationLeft : Html Msg
dateNavigationLeft =
    div [ class "advancer-wrapper" ]
        [ div [ class "day-advance", onClick (Msgs.OffsetDay -1) ]
            [ Html.img [ iconBig, src "imgs/arrow-left-circle.svg" ] [] ]
        , div [ class "week-advance", onClick (Msgs.OffsetDay -5) ]
            [ Html.img [ iconSmall, src "imgs/arrow-left.svg" ] [] ]
        , div [ class "go-home-week", onClick (Msgs.OffsetDay 0) ]
            [ Html.img [ iconSmall, src "imgs/home.svg" ] [] ]
        ]


dateNavigationRight : Html Msg
dateNavigationRight =
    div [ class "advancer-wrapper" ]
        [ div [ class "day-advance", onClick (Msgs.OffsetDay 1) ]
            [ Html.img [ iconBig, src "imgs/arrow-right-circle.svg" ] [] ]
        , div [ class "week-advance", onClick (Msgs.OffsetDay 5) ]
            [ Html.img [ iconSmall, src "imgs/arrow-right.svg" ] [] ]
        ]


customListView model =
    let
        customLists =
            maybeTodoLists model.customLists
    in
        div []
            [ div [ class "list-divider" ]
                [ div
                    [ class "pointer"
                    , onClick Msgs.CustomListCreate
                    ]
                    [ text "+" ]
                ]
            , div [ class "flex flex-auto justify-around" ]
                (List.map (TodoList.View.list model) customLists)
            ]


{-| Top level app-wide view.
-}
appView : Model -> Html Msg
appView model =
    div [ class "flex flex-column flex-auto justify-center" ]
        -- hack to add a stylesheet for elm reactor.
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/basscss/8.0.4/css/basscss.min.css" ] []
        , div [ class "tudu-nav" ] [ text "Tudu" ]
        , div [ class "flex" ]
            [ dateNavigationLeft
            , div [ class "flex flex-auto flex-column" ]
                -- todos lists by date. --:
                [ div [ class "flex flex-auto justify-around" ]
                    (List.map (TodoList.View.list model) model.currentWeek)

                -- divider
                , customListView model
                ]
            , dateNavigationRight
            ]
        ]
