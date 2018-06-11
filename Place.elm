module Place exposing (..)

import Model exposing (..)


place : Model -> Placed
place model =
    let
        placedNodes =
            placeNodes model.edges model.nodes
    in
        { model | nodes = placedNodes, edges = placeEdges model.edges placedNodes }


placeNodes : Edges -> Nodes -> PlacedNodes
placeNodes edges nodes =
    let
        index =
            List.range 0 (List.length nodes)
    in
        List.map2 (placeNode edges) index nodes


placeEdges : Edges -> PlacedNodes -> PlacedEdges
placeEdges edges placedNodes =
    List.map (placeEdge placedNodes) edges


placeNode : Edges -> Int -> Node -> PlacedNode
placeNode edges index node =
    { id = node.id
    , name = node.name
    , x = 120 * index
    , y = 120 * (depth edges node.id)
    }


placeEdge : PlacedNodes -> Edge -> PlacedEdge
placeEdge placedNodes edge =
    let
        from =
            List.head (List.filter (\node -> node.id == edge.from) placedNodes)

        to =
            List.head (List.filter (\node -> node.id == edge.to) placedNodes)
    in
        case ( from, to ) of
            ( Just from, Just to ) ->
                let
                    angle =
                        atan2 (toFloat (to.y - from.y)) (toFloat (to.x - from.x))
                in
                    { x1 = from.x + 50
                    , y1 = from.y + 50
                    , x2 = to.x + 50 - round (65 * cos (angle))
                    , y2 = to.y + 50 - round (65 * sin (angle))
                    }

            _ ->
                { x1 = 0
                , y1 = 0
                , x2 = 0
                , y2 = 0
                }


depth : Edges -> Int -> Int
depth edges nodeID =
    innerDepth edges [] nodeID


innerDepth : Edges -> List Int -> Int -> Int
innerDepth edges visited nodeID =
    let
        connectedEdges =
            List.filter (\edge -> edge.to == nodeID && not (List.member edge.from visited) && edge.to /= edge.from) edges

        froms =
            List.map (\edge -> edge.from) connectedEdges
    in
        if connectedEdges == [] then
            0
        else
            1 + List.foldr max 0 (List.map (innerDepth edges (nodeID :: visited)) froms)
