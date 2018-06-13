module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Place exposing (..)
import Action exposing (..)


view : Model -> Html Msg
view model =
    svg [ width "1200", height "800", viewBox "0 0 1200 800" ]
        (arrowHead
            :: (List.append
                    (List.map2 viewEdge model.edges model.placedEdges)
                    (List.foldr List.append [] (List.map2 viewNode model.nodes model.placedNodes))
               )
        )


arrowHead : Html Msg
arrowHead =
    Svg.defs []
        [ Svg.marker
            [ id "arrow"
            , orient "auto"
            , markerWidth "3"
            , markerHeight "6"
            , refX "0.1"
            , refY "3"
            ]
            [ Svg.path [ d "M0,0 V6 L3,3 Z", fill "#b0b0b0" ] [] ]
        , Svg.marker
            [ id "selectedArrow"
            , orient "auto"
            , markerWidth "3"
            , markerHeight "6"
            , refX "0.1"
            , refY "3"
            ]
            [ Svg.path [ d "M0,0 V6 L3,3 Z", fill "#808080" ] [] ]
        ]


getStrokeWidth : Bool -> String
getStrokeWidth selected =
    if selected then
        "3"
    else
        "0"


viewNode : Node -> PlacedNode -> List (Svg Msg)
viewNode node placedNode =
    [ circle
        [ onClick (ClickNode node.id)
        , cx (toString (placedNode.x + 50))
        , cy (toString (placedNode.y + 50))
        , r "50"
        , fill "#f0f0f0"
        , stroke "#b0b0b0"
        , strokeWidth (getStrokeWidth node.selected)
        ]
        []
    , Svg.text_
        [ x (toString (placedNode.x + 25))
        , y (toString (placedNode.y + 35))
        , fill "#808080"
        , fontSize "20"
        , fontFamily "sans-serif"
        ]
        [ Svg.text node.name ]
    ]


getStrokeColor : Bool -> ( String, String )
getStrokeColor selected =
    if selected then
        ( "url(#selectedArrow)", "#808080" )
    else
        ( "url(#arrow)", "#b0b0b0" )


viewEdge : Edge -> PlacedEdge -> Html Msg
viewEdge edge placedEdge =
    let
        ( marker, strokeColor ) =
            getStrokeColor edge.selected
    in
        Svg.path
            [ onClick (ClickEdge ( edge.from, edge.to ))
            , markerEnd marker
            , fill "none"
            , strokeWidth "3"
            , stroke strokeColor
            , d
                ("M"
                    ++ (toString placedEdge.x1)
                    ++ " "
                    ++ (toString placedEdge.y1)
                    ++ " Q "
                    ++ (toString placedEdge.cx)
                    ++ " "
                    ++ (toString placedEdge.cy)
                    ++ " "
                    ++ (toString placedEdge.x2)
                    ++ " "
                    ++ (toString placedEdge.y2)
                )
            ]
            []
