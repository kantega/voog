module Depth exposing (..)

import Model exposing (..)


calculateDepth : Edges -> Nodes -> DepthNodes
calculateDepth edges nodes =
    depthIteration (List.map nodeToDepthNode nodes) edges


nodeToDepthNode : Node -> DepthNode
nodeToDepthNode node =
    { id = node.id, parent = -1, depth = -1 }


outEdges : Edges -> Int -> Int
outEdges edges id =
    edges
        |> List.filter (\e -> e.from == id && e.from /= e.to)
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
                depthIteration (updateNode edges nodes head { id = -1, parent = -1, depth = -1 }) edges

            Nothing ->
                nodes


updateNode : Edges -> DepthNodes -> DepthNode -> DepthNode -> DepthNodes
updateNode edges nodes node parent =
    let
        ( updated, newNodes ) =
            updateDepth edges nodes node parent
        newNode = newNodes
            |> List.filter (\n -> n.id == node.id)
            |> List.head
    in
        if updated then
            case newNode of
                Just newNode ->
                    newNodes
                        |> List.filter (\n -> List.member { from = node.id, to = n.id } edges && n.id /= node.id)
                        |> depthChildren edges newNodes newNode
                Nothing ->
                    newNodes
        else
            nodes


depthChildren : Edges -> DepthNodes -> DepthNode -> DepthNodes -> DepthNodes
depthChildren edges nodes parent foldNodes =
    case List.head foldNodes of
        Just head ->
            depthChildren edges (updateNode edges nodes head parent) parent (List.drop 1 foldNodes)

        Nothing ->
            nodes


updateDepth : Edges -> DepthNodes -> DepthNode -> DepthNode -> ( Bool, DepthNodes )
updateDepth edges nodes node parent =
    if node.depth < 0 || node.depth > parent.depth + 1 then
        ( True
        , List.map
            (\n ->
                if n.id == node.id then
                    { n | depth = parent.depth + 1, parent = parent.id }
                else
                    n
            )
            nodes
        )
    else
        ( False, nodes )
