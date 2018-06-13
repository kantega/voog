module View exposing (..)

import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Place exposing (..)
import Action exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "sidebar" ] []
        , svg [ width "1200", height "800", viewBox "0 0 1200 800" ]
            ((defs model)
                :: (List.append
                        (List.foldr List.append [] (List.map2 viewNode model.nodes model.placedNodes))
                        (List.foldr List.append [] (List.map2 viewEdge model.edges model.placedEdges))

                   )
            )
        ]


defs : Model -> Html Msg
defs model =
    let
        imageNodes =
            List.filter (\n -> Dict.member "image" n.info) model.nodes

        imageIds =
            List.map (\n -> n.id) imageNodes

        placedImageNodes =
            List.filter (\n -> List.member n.id imageIds) model.placedNodes
    in
        Svg.defs []
            (List.append
                ([ Svg.marker
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
                )
                (List.map2
                    (\node placedNode ->
                        Svg.pattern
                            [ id ("img" ++ (toString node.id))
                            , patternUnits "userSpaceOnUse"
                            , x (toString (placedNode.x + 10))
                            , y (toString (placedNode.y + 10))
                            , width "40"
                            , height "40"
                            ]
                            [ Svg.image
                                [ xlinkHref (Maybe.withDefault "" (Dict.get "image" node.info))
                                ]
                                []
                            ]
                    )
                    imageNodes
                    placedImageNodes
                )
            )


getStrokeWidth : Bool -> String
getStrokeWidth selected =
    if selected then
        "3"
    else
        "0"


getTextX : Node -> PlacedNode -> String
getTextX node placedNode =
    if Dict.member "image" node.info then
        toString (placedNode.x + 55)
    else
        toString (placedNode.x + 25)


viewNode : Node -> PlacedNode -> List (Svg Msg)
viewNode node placedNode =
    [ circle
        [ onClick (ClickNode node.id)
        , cx (toString (placedNode.x + 50))
        , cy (toString (placedNode.y + 50))
        , r "50"
        , fill "#f0f0f0"
        , stroke (if Dict.get "error" node.info == Just "True" then "#f44336" else "#b0b0b0" )
        , strokeWidth (getStrokeWidth node.selected)
        ]
        []
    , circle
        [ cx (toString (placedNode.x + 30))
        , cy (toString (placedNode.y + 30))
        , r "20"
        , fill ("url(#img" ++ (toString node.id) ++ ")")
        ]
        []
    , Svg.text_
        [ x (getTextX node placedNode)
        , y (toString (placedNode.y + 36))
        , fill "#b0b0b0"
        , fontFamily "sans-serif"
        ]
        [ Svg.text (Maybe.withDefault "" node.name) ]
    ]


getStrokeColor : Bool -> ( String, String )
getStrokeColor selected =
    if selected then
        ( "url(#selectedArrow)", "#808080" )
    else
        ( "url(#arrow)", "#b0b0b0" )


viewEdge : Edge -> PlacedEdge -> List (Html Msg)
viewEdge edge placedEdge =
    let
        ( marker, strokeColor ) =
            getStrokeColor edge.selected
    in
        [ Svg.path
            [ onClick (ClickEdge ( edge.from, edge.to ))
            , id ((toString (Tuple.first edge.id)) ++ "_"  ++ (toString (Tuple.second edge.id)))
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
        , Svg.text_
            [ fill "#808080"
            , fontSize "20"
            , fontFamily "sans-serif"
            , textAnchor "middle"
            , dy "-5"
            ]
            [ Svg.textPath
                [ xlinkHref ("#" ++ (toString (Tuple.first edge.id)) ++ "_" ++ (toString (Tuple.second edge.id)))
                , startOffset "50%"
                ]
                [ Svg.text (Maybe.withDefault "" (Dict.get "speed" edge.info))
                ]
            ]
        ]
