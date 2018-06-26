module Place exposing (..)

import Model exposing (..)


nodeRadius =
    45


distance =
    4 * nodeRadius


labelWidth =
    60


labelHeight =
    30


sign : Int -> Int
sign a =
    if a < 0 then
        -1
    else if a > 0 then
        1
    else
        0


reverseId : ( Int, Int ) -> ( Int, Int )
reverseId id =
    ( Tuple.second id, Tuple.first id )


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


getLabelPosition : MultiLine -> Int -> Maybe Point
getLabelPosition line offset =
    if List.length line % 2 == 0 then
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
                            round (distance * (toFloat (nodeA.x + nodeB.x) / 2))
                                + nodeRadius
                                + offset
                        , y =
                            round (distance * (toFloat (nodeA.y + nodeB.y) / 2))
                                + nodeRadius
                                + (sign offset * (round (toFloat labelHeight / 2) + 2))
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
                            { x = distance * p.x + nodeRadius + offset
                            , y = distance * p.y + nodeRadius + (sign offset * (round (toFloat labelHeight / 2) + 2))
                            }

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
                    let
                        width =
                            round ((Maybe.withDefault 8 edge.width) / 2)

                        offset =
                            if not (List.any (\e -> e.id == reverseId edge.id) edges) then
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
                                        { x = distance * x + nodeRadius + offset
                                        , y = distance * y + nodeRadius
                                        }
                                    )
                                    line
                                )
                            )
                        , getLabelPosition line offset
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
                                                        width =
                                                            (Maybe.withDefault 8 edge.width) / 2

                                                        offset =
                                                            width + 1

                                                        labelOffset =
                                                            if Tuple.first edge.id > Tuple.second edge.id then
                                                                -(round offset)
                                                            else
                                                                round offset

                                                        direction =
                                                            if from.x == to.x then
                                                                sign labelOffset
                                                            else
                                                                sign (to.x - from.x)
                                                    in
                                                        ( Just
                                                            (Straight
                                                                { from =
                                                                    { x = from.x + nodeRadius + round (offset * cos (angle + 3.1415 / 2))
                                                                    , y = from.y + nodeRadius + round (offset * sin (angle + 3.1415 / 2))
                                                                    }
                                                                , to =
                                                                    { x = to.x + nodeRadius + round (offset * cos (angle + 3.1415 / 2))
                                                                    , y = to.y + nodeRadius + round (offset * sin (angle + 3.1415 / 2))
                                                                    }
                                                                }
                                                            )
                                                        , Just
                                                            { x = nodeRadius + round (toFloat (from.x + to.x) / 2) + labelOffset
                                                            , y = nodeRadius + round (toFloat (from.y + to.y) / 2) + (sign (direction) * (round (toFloat labelHeight / 2) + 2))
                                                            }
                                                        )
                                                else
                                                    ( Just
                                                        (Straight
                                                            { from =
                                                                { x = from.x + nodeRadius
                                                                , y = from.y + nodeRadius
                                                                }
                                                            , to =
                                                                { x = to.x + nodeRadius
                                                                , y = to.y + nodeRadius
                                                                }
                                                            }
                                                        )
                                                    , Just
                                                        { x = nodeRadius + round (toFloat (from.x + to.x) / 2)
                                                        , y = nodeRadius + round (toFloat (from.y + to.y) / 2)
                                                        }
                                                    )
                                        else
                                            ( Nothing, Nothing )

                                    _ ->
                                        ( Nothing, Nothing )

                            _ ->
                                ( Nothing, Nothing )
    in
        { edge | position = position, labelPosition = labelPosition }
