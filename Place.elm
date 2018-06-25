module Place exposing (..)

import Model exposing (..)


nodeRadius =
    45


distance =
    4 * nodeRadius


arrowDistance =
    10


arrowWidth =
    2


place : Model -> Model
place model =
    let
        placedNodes =
            placeNodes model.nodes
    in
        { model
            | nodes = placedNodes
            , edges = placeEdges model.edges placedNodes
        }


outEdges : Edges -> Int -> Int
outEdges edges id =
    edges
        |> List.filter (\e -> e.from == id && e.from /= e.to)
        |> List.length


inEdges : Edges -> Int -> Int
inEdges edges id =
    edges
        |> List.filter (\e -> e.to == id && e.to /= e.from)
        |> List.length


placeNodes : Nodes -> Nodes
placeNodes nodes =
    nodes
        |> List.map setPos


setPos : Node -> Node
setPos ({ position } as node) =
    let
        p =
            case position of
                Just position ->
                    Just
                        { x = position.x * distance
                        , y = position.y * distance
                        }

                Nothing ->
                    position
    in
        { node
            | position = p
        }


placeEdges : Edges -> Nodes -> Edges
placeEdges edges nodes =
    List.map (placeEdge nodes edges) edges


getLabelPosition : MultiLine -> Maybe Point
getLabelPosition line =
    let

        second =
            line
                |> List.indexedMap (\i p -> ( i, p ))
                |> List.filter (\( i, p ) -> i == 1)
                |> List.head
        pos =
            case second of
                Just (i, p) ->
                    Just { x = distance * p.x + nodeRadius, y = distance * p.y + nodeRadius }
                _ ->
                    Nothing
    in
        pos


placeEdge : Nodes -> Edges -> Edge -> Edge
placeEdge nodes edges edge =
    let
        ( position, labelPosition ) =
            case edge.position of
                Just (Multi line) ->
                    ( Just
                        (Multi
                            (line
                                |> List.map (\{ x, y } -> { x = distance * x + nodeRadius, y = distance * y + nodeRadius })
                            )
                        )
                    , getLabelPosition line
                    )

                _ ->
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
                                                        offset = Maybe.withDefault 8 edge.width / 2 + 1
                                                        f =
                                                            { x = from.x + nodeRadius + round (offset * cos (angle + 3.1415 / 2))
                                                            , y = from.y + nodeRadius + round (offset * sin (angle + 3.1415 / 2))
                                                            }

                                                        t =
                                                            { x = to.x + nodeRadius + round (offset * cos (angle + 3.1415 / 2))
                                                            , y = to.y + nodeRadius + round (offset * sin (angle + 3.1415 / 2))
                                                            }
                                                    in
                                                        ( Just
                                                            (Straight
                                                                { from = f
                                                                , to = t
                                                                }
                                                            )
                                                        , Just t
                                                        )
                                                else
                                                    let
                                                        f =
                                                            { x = from.x + nodeRadius
                                                            , y = from.y + nodeRadius
                                                            }

                                                        t =
                                                            { x = to.x + nodeRadius
                                                            , y = to.y + nodeRadius
                                                            }

                                                        m =
                                                            { x = nodeRadius + round (toFloat (from.x + to.x) / 2)
                                                            , y = nodeRadius + round (toFloat (from.y + to.y) / 2)
                                                            }
                                                    in
                                                        ( Just
                                                            (Straight
                                                                { from = f
                                                                , to = t
                                                                }
                                                            )
                                                        , Just m
                                                        )
                                        else
                                            let
                                                f =
                                                    { x = from.x - arrowDistance - 2
                                                    , y = from.y + nodeRadius
                                                    }

                                                t =
                                                    { x = from.x - arrowDistance
                                                    , y = from.y + nodeRadius
                                                    }
                                            in
                                                ( Just
                                                    (Straight
                                                        { from = f
                                                        , to = t
                                                        }
                                                    )
                                                , Nothing
                                                )

                                    _ ->
                                        ( Nothing, Nothing )

                            _ ->
                                ( Nothing, Nothing )
    in
        { edge | position = position, labelPosition = labelPosition }
