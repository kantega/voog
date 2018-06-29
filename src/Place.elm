module Place exposing (..)

import Model exposing (..)


nodeRadius =
    45


defaultDistance =
    4 * nodeRadius


labelWidth =
    60


labelHeight =
    30


sign : Float -> Float
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
        distance =
            (Maybe.withDefault defaultDistance model.nodeDistance)

        placedNodes =
            List.map (setPos distance) model.nodes
    in
        { model
            | nodes = placedNodes
            , edges = List.map (placeEdge placedNodes distance model.edges) model.edges
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


setPos : Float -> Node -> Node
setPos distance ({ position } as node) =
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


getLabelPosition : MultiLine -> Float -> Float -> Maybe Point
getLabelPosition line offset distance =
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
                            distance
                                * ((nodeA.x + nodeB.x) / 2)
                                + nodeRadius
                                + offset
                        , y =
                            distance
                                * ((nodeA.y + nodeB.y) / 2)
                                + nodeRadius
                                + (sign offset * (toFloat labelHeight / 2) + 2)
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
                            , y = distance * p.y + nodeRadius + sign offset * ((toFloat labelHeight / 2) + 2)
                            }

                    _ ->
                        Nothing
        in
            pos


placeEdge : Nodes -> Float -> Edges -> Edge -> Edge
placeEdge nodes distance edges edge =
    let
        ( position, labelPosition ) =
            case edge.position of
                Just (Multi line) ->
                    placeMultiLineEdge line edges edge distance

                _ ->
                    placeSingleLineEdge nodes edges edge
    in
        { edge | position = position, labelPosition = labelPosition }


placeMultiLineEdge : MultiLine -> Edges -> Edge -> Float -> ( Maybe Line, Maybe Point )
placeMultiLineEdge line edges edge distance =
    let
        width =
            (Maybe.withDefault 8 edge.width) / 2

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
        , getLabelPosition line offset distance
        )


placeSingleLineEdge : Nodes -> Edges -> Edge -> ( Maybe Line, Maybe Point )
placeSingleLineEdge nodes edges edge =
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
                        if from == to then
                            ( Nothing, Nothing )
                        else if List.any (\e -> e.id == ( toNode.id, fromNode.id )) edges then
                            let
                                width =
                                    (Maybe.withDefault 8 edge.width) / 2

                                offset =
                                    width + 1

                                labelOffset =
                                    if Tuple.first edge.id > Tuple.second edge.id then
                                        -(offset)
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
                                            { x = from.x + nodeRadius + offset * cos (angle + 3.1415 / 2)
                                            , y = from.y + nodeRadius + offset * sin (angle + 3.1415 / 2)
                                            }
                                        , to =
                                            { x = to.x + nodeRadius + offset * cos (angle + 3.1415 / 2)
                                            , y = to.y + nodeRadius + offset * sin (angle + 3.1415 / 2)
                                            }
                                        }
                                    )
                                , Just
                                    { x =
                                        nodeRadius
                                            + ((from.x + to.x) / 2)
                                            + labelOffset
                                    , y =
                                        nodeRadius
                                            + ((from.y + to.y) / 2)
                                            + (sign (direction) * (labelHeight / 2) + 2)
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
                                { x = nodeRadius + (from.x + to.x) / 2
                                , y = nodeRadius + (from.y + to.y) / 2
                                }
                            )

                    _ ->
                        ( Nothing, Nothing )

            _ ->
                ( Nothing, Nothing )
