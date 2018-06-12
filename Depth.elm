module Depth exposing (..)

import Model exposing (..)


calculateDepth : Nodes -> Edges -> DepthNodes
calculateDepth nodes edges =
    depthIteration (List.map nodeToDepthNode nodes) edges


nodeToDepthNode : Node -> DepthNode
nodeToDepthNode node =
    { id = node.id, parent = -1, depth = -1 }


outEdges : Edges -> Int -> Int
outEdges edges id =
    edges
        |> List.filter (\e -> e.from == id)
        |> List.length


inEdges : Edges -> Int -> Int
inEdges edges id =
    edges
        |> List.filter (\e -> e.to == id)
        |> List.length


depthIteration : DepthNodes -> Edges -> DepthNodes
depthIteration nodes edges =
    let
        head =
            (List.filter (\n -> n.depth < 0) nodes)
                |> List.sortWith (\a b -> compare (inEdges edges a.id) (inEdges edges b.id))
                |> List.head
    in
        case head of
            Just head ->
                depthIteration (updateChildren edges nodes head 0) edges

            Nothing ->
                nodes


updateChildren : Edges -> DepthNodes -> DepthNode -> Int -> DepthNodes
updateChildren edges nodes node depth =
    let
        newNodes =
            setDepth edges node.id depth nodes
    in
        newNodes
            |> List.filter (\n -> List.member { from = node.id, to = n.id } edges)
            |> List.filter (\n -> n.depth < 0)
            |> depthFold edges (depth + 1) newNodes


depthFold : Edges -> Int -> DepthNodes -> DepthNodes -> DepthNodes
depthFold edges depth nodes foldNodes =
    case List.head foldNodes of
        Just head ->
            depthFold edges depth (updateChildren edges nodes head depth) (List.drop 1 foldNodes)

        Nothing ->
            nodes


setDepth : Edges -> Int -> Int -> DepthNodes -> DepthNodes
setDepth edges id depth nodes =
    let
        partition =
            List.partition (\n -> n.id == id) nodes

        node =
            List.head (Tuple.first partition)

        others =
            Tuple.second partition
    in
        case node of
            Just node ->
                if node.depth < 0 then
                    { node | depth = depth } :: others
                else if node.depth > depth then
                    updateChildren edges ({ node | depth = depth } :: others) node depth
                else
                    node :: others

            Nothing ->
                nodes
