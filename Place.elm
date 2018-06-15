module Place exposing (..)

import Model exposing (..)
import Depth exposing (..)


place : Model -> Model
place model =
    let
        placedNodes =
            placeNodes model.edges model.nodes
    in
        { model
            | nodes = placedNodes
            , edges = placeEdges model.edges placedNodes
        }


placeNodes : Edges -> Nodes -> Nodes
placeNodes edges nodes =
    nodes
        |> List.sortWith (\a b -> compare (outEdges edges a.id) (outEdges edges b.id))
        |> List.reverse
        |> List.indexedMap setPos


setPos : Int -> Node -> Node
setPos index node =
    let
        p =
            { x = index * 150
            , y = node.depth * 150
            }
    in
        { node
            | position = Just p
        }


placeEdges : Edges -> Nodes -> Edges
placeEdges edges nodes =
    List.map (\e -> { e | position = placeEdge nodes edges e }) edges


placeEdge : Nodes -> Edges -> Edge -> Maybe Line
placeEdge nodes edges edge =
    let
        from =
            List.head (List.filter (\node -> node.id == edge.from) nodes)

        to =
            List.head (List.filter (\node -> node.id == edge.to) nodes)
    in
        case ( from, to ) of
            ( Just fromNode, Just toNode ) ->
                case ( fromNode.position, toNode.position ) of
                    ( Just from, Just to ) ->
                        if from /= to then
                            let
                                angle =
                                    atan2 (toFloat (to.y - from.y)) (toFloat (to.x - from.x))

                                distance =
                                    sqrt ((toFloat (to.y - from.y)) ^ 2 + (toFloat (to.x - from.x)) ^ 2)
                            in
                                if List.any (\e -> e.id == ( toNode.id, fromNode.id )) edges then
                                    let
                                        f =
                                            { x = from.x + 50 + round (50 * cos (angle)) + round (8 * cos (angle + 3.1415 / 2))
                                            , y = from.y + 50 + round (50 * sin (angle)) + round (8 * sin (angle + 3.1415 / 2))
                                            }

                                        t =
                                            { x = to.x + 50 - round (60 * cos (angle)) + round (8 * cos (angle + 3.1415 / 2))
                                            , y = to.y + 50 - round (60 * sin (angle)) + round (8 * sin (angle + 3.1415 / 2))
                                            }

                                        v =
                                            { x = from.x + 50 + round (distance / 2 * cos (angle + 3.1415 / 32)) + round (8 * cos (angle + 3.1415 / 2))
                                            , y = from.y + 50 + round (distance / 2 * sin (angle + 3.1415 / 32)) + round (8 * sin (angle + 3.1415 / 2))
                                            }
                                    in
                                        Just
                                            (Curved
                                                { from = f
                                                , to = t
                                                , via = v
                                                }
                                            )
                                else
                                    let
                                        f =
                                            { x = from.x + 50 + round (60 * cos (angle))
                                            , y = from.y + 50 + round (60 * sin (angle))
                                            }

                                        t =
                                            { x = to.x + 50 - round (60 * cos (angle))
                                            , y = to.y + 50 - round (60 * sin (angle))
                                            }
                                    in
                                        Just
                                            (Straight
                                                { from = f
                                                , to = t
                                                }
                                            )
                        else
                            let
                                f =
                                    { x = from.x - 12
                                    , y = from.y + 50
                                    }

                                t =
                                    { x = from.x - 10
                                    , y = from.y + 50
                                    }
                            in
                                Just
                                    (Straight
                                        { from = f
                                        , to = t
                                        }
                                    )

                    _ ->
                        Nothing

            _ ->
                Nothing
