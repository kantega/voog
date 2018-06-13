module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Model exposing (..)
import Place exposing (..)


view : Model -> Html Msg
view model =
    let
        placed =
            place model
    in
        svg [ width "1200", height "800", viewBox "0 0 1200 800" ]
            (arrowHead
                :: (List.append
                        (List.map viewEdge placed.edges)
                        (List.foldr List.append [] (List.map viewNode placed.nodes))
                   )
            )


arrowHead : Html Msg
arrowHead =
    Svg.defs [] [ Svg.marker [ id "arrow", orient "auto", markerWidth "3", markerHeight "6", refX "0.1", refY "3" ] [ Svg.path [ d "M0,0 V6 L3,3 Z", fill "#999999" ] [] ] ]


viewNode : PlacedNode -> List (Svg Msg)
viewNode node =
    [ circle
        [ cx (toString (node.x + 50)), cy (toString (node.y + 50)), r "50", fill "#f0f0f0" ]
        []
    , Svg.text_ [ x (toString (node.x + 25)), y (toString (node.y + 35)), fill "#999999", fontSize "20", fontFamily "sans-serif" ] [ Svg.text node.name ]
    ]


viewEdge : PlacedEdge -> Html Msg
viewEdge edge =
    Svg.path
        [ markerEnd "url(#arrow)"
        , fill "none"
        , strokeWidth "3"
        , stroke "#b0b0b0"
        , d
            ("M"
                ++ (toString edge.x1)
                ++ " "
                ++ (toString edge.y1)
                ++ " Q "
                ++ (toString edge.cx)
                ++ " "
                ++ (toString edge.cy)
                ++ " "
                ++ (toString edge.x2)
                ++ " "
                ++ (toString edge.y2)
            )
        ]
        []
