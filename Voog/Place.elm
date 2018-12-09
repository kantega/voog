module Voog.Place exposing (getLabelPosition, place, placeEdge, placeMultiLineEdge, placeSingleLineEdge)

import Dict exposing (Dict)
import Voog.Helpers exposing (..)
import Voog.Model exposing (..)
import Voog.View exposing (..)


place : Model -> Model
place ({ nodes, edges } as model) =
    let
        nodeDict =
            nodes
                |> List.map (\n -> ( n.id, n ))
                |> Dict.fromList

        edgeDict =
            edges
                |> List.map (\e -> ( e.id, e ))
                |> Dict.fromList

        newNodes =
            List.map
                (\n -> { n | viewNode = viewNodeWithHref model.name n })
                nodes
    in
    { model | edges = List.map (placeEdge nodeDict edgeDict) model.edges, nodes = newNodes }


placeEdge : Dict Int Node -> Dict ( Int, Int ) Edge -> Edge -> Edge
placeEdge nodes edges edge =
    let
        ( position, labelPosition ) =
            case edge.position of
                Just (Multi line) ->
                    placeMultiLineEdge line edges edge

                _ ->
                    placeSingleLineEdge nodes edges edge
    in
    { edge | position = position, labelPosition = labelPosition }


placeMultiLineEdge : MultiLine -> Dict ( Int, Int ) Edge -> Edge -> ( Maybe Line, Maybe Point )
placeMultiLineEdge line edges edge =
    let
        width =
            Maybe.withDefault 8 edge.width / 2

        offset =
            if Dict.get (reverseId edge.id) edges == Nothing then
                0

            else if Tuple.first edge.id > Tuple.second edge.id then
                -width - 1

            else
                width + 1
    in
    ( Just
        (Multi
            (List.map
                (\{ x, y } ->
                    { x = x + offset
                    , y = y
                    }
                )
                line
            )
        )
    , getLabelPosition line offset
    )


placeSingleLineEdge : Dict Int Node -> Dict ( Int, Int ) Edge -> Edge -> ( Maybe Line, Maybe Point )
placeSingleLineEdge nodes edges edge =
    case ( Dict.get edge.from nodes, Dict.get edge.to nodes ) of
        ( Just fromNode, Just toNode ) ->
            case ( fromNode.position, toNode.position ) of
                ( Just from, Just to ) ->
                    if from == to then
                        ( Nothing, Nothing )

                    else if not <| Dict.get ( toNode.id, fromNode.id ) edges == Nothing then
                        let
                            width =
                                Maybe.withDefault 8 edge.width / 2

                            offset =
                                width + 1

                            labelOffset =
                                if Tuple.first edge.id > Tuple.second edge.id then
                                    -offset

                                else
                                    offset

                            direction =
                                if from.x == to.x then
                                    sign labelOffset

                                else
                                    sign (to.x - from.x)

                            angle =
                                atan2 (to.y - from.y) (to.x - from.x)
                        in
                        ( Just
                            (Straight
                                { from =
                                    { x = from.x + offset * cos (angle + 3.1415 / 2)
                                    , y = from.y + offset * sin (angle + 3.1415 / 2)
                                    }
                                , to =
                                    { x = to.x + offset * cos (angle + 3.1415 / 2)
                                    , y = to.y + offset * sin (angle + 3.1415 / 2)
                                    }
                                }
                            )
                        , Just
                            { x =
                                (from.x + to.x) / 2 + labelOffset
                            , y =
                                (from.y + to.y) / 2 + (sign direction * (labelHeight / 2) + 2)
                            }
                        )

                    else
                        ( Just
                            (Straight
                                { from =
                                    from
                                , to =
                                    to
                                }
                            )
                        , Just
                            { x = (from.x + to.x) / 2
                            , y = (from.y + to.y) / 2
                            }
                        )

                _ ->
                    ( Nothing, Nothing )

        _ ->
            ( Nothing, Nothing )


getLabelPosition : MultiLine -> Float -> Maybe Point
getLabelPosition line offset =
    if modBy 2 (List.length line) == 0 then
        let
            mid =
                round (toFloat (List.length line) / 2)

            midPoints =
                line
                    |> List.indexedMap (\i p -> ( i, p ))
                    |> List.filter (\( i, p ) -> i == mid || i == mid - 1)
        in
        case midPoints of
            ( i, nodeA ) :: ( j, nodeB ) :: rest ->
                Just
                    { x =
                        (nodeA.x + nodeB.x) / 2 + offset
                    , y =
                        (nodeA.y + nodeB.y) / 2 + sign offset * (labelHeight / 2) + 2
                    }

            _ ->
                Nothing

    else
        let
            mid =
                round (toFloat (List.length line) / 2) - 1

            midPoint =
                line
                    |> List.indexedMap (\i p -> ( i, p ))
                    |> List.filter (\( i, p ) -> i == mid)
                    |> List.head

            pos =
                case midPoint of
                    Just ( i, p ) ->
                        Just
                            { x = p.x + offset
                            , y = p.y + sign offset * ((labelHeight / 2) + 2)
                            }

                    _ ->
                        Nothing
        in
        pos
