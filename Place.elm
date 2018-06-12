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
        initializedNodes =
            List.indexedMap initializeNode nodes
    in
        placeNodesIteration edges initializedNodes


initializeNode : Int -> Node -> PlacedNode
initializeNode index node =
    { id = node.id, name = node.name, x = 120 * index, y = -1 }


outEdges : Edges -> Int -> Int
outEdges edges id =
    List.filter (\e -> e.from == id) edges
        |> List.length


placeNodesIteration : Edges -> PlacedNodes -> PlacedNodes
placeNodesIteration edges nodes =
    let
        head =
            (List.filter (\n -> n.y < 0) nodes)
                |> List.sortWith (\a b -> compare (outEdges edges a.id) (outEdges edges b.id))
                |> List.reverse
                |> List.head
    in
        case head of
            Just head ->
                placeNodesIteration edges (placeNodesInner edges nodes head 0)

            Nothing ->
                nodes


placeNodesInner : Edges -> PlacedNodes -> PlacedNode -> Int -> PlacedNodes
placeNodesInner edges nodes node depth =
    let
        newNodes =
            setDepth node.id depth nodes
    in
        newNodes
            |> List.filter (\n -> List.member { from = node.id, to = n.id } edges)
            |> List.filter (\n -> n.y < 0)
            |> foldPlace edges (depth + 1) newNodes


foldPlace : Edges -> Int -> PlacedNodes -> PlacedNodes -> PlacedNodes
foldPlace edges depth nodes foldNodes =
    case List.head foldNodes of
        Just head ->
            foldPlace edges depth (placeNodesInner edges nodes head depth) (List.drop 1 foldNodes)

        Nothing ->
            nodes


setDepth : Int -> Int -> PlacedNodes -> PlacedNodes
setDepth id depth nodes =
    List.map
        (\n ->
            if n.id == id && n.y < 0 then
                { n | y = depth * 120 }
            else
                n
        )
        nodes


placeEdges : Edges -> PlacedNodes -> PlacedEdges
placeEdges edges placedNodes =
    List.map (placeEdge placedNodes) edges


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
                if from /= to then
                    let
                        angle =
                            atan2 (toFloat (to.y - from.y)) (toFloat (to.x - from.x))
                    in
                        { x1 = from.x + 50
                        , y1 = from.y + 50
                        , x2 = to.x + 50 - round (65 * cos (angle))
                        , y2 = to.y + 50 - round (65 * sin (angle))
                        }
                else
                    { x1 = from.x - 12
                    , y1 = from.y + 50
                    , x2 = from.x - 8
                    , y2 = from.y + 50
                    }

            _ ->
                { x1 = 0
                , y1 = 0
                , x2 = 0
                , y2 = 0
                }
