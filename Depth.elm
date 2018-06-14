module Depth exposing (..)

import Dict
import Model exposing (..)


calculateDepth : Edges -> Nodes -> Nodes
calculateDepth edges nodes =
    depthIteration edges (initNodes nodes)
        |> List.sortWith (\a b -> compare a.id b.id)


initNodes : Nodes -> Nodes
initNodes nodes =
    List.map (\n -> { n | depth = -1, parent = -1 }) nodes


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


depthIteration : Edges -> Nodes -> Nodes
depthIteration edges nodes =
    let
        head =
            (List.filter (\n -> n.depth < 0 && inEdges edges n.id == 0) nodes)
                |> List.head
    in
        case head of
            Just head ->
                depthIteration edges (updateNode edges nodes head Nothing)

            Nothing ->
                nodes


updateNode : Edges -> Nodes -> Node -> Maybe Node -> Nodes
updateNode edges nodes node parent =
    let
        ( updated, newNodes ) =
            updateDepth edges nodes node parent

        newNode =
            newNodes
                |> List.filter (\n -> n.id == node.id)
                |> List.head
    in
        if updated then
            case newNode of
                Just newNode ->
                    newNodes
                        |> List.filter (\n -> List.member ( node.id, n.id ) (List.map (\e -> e.id) edges) && n.id /= node.id)
                        |> depthChildren edges newNodes newNode

                Nothing ->
                    newNodes
        else
            nodes


depthChildren : Edges -> Nodes -> Node -> Nodes -> Nodes
depthChildren edges nodes parent foldNodes =
    case List.head foldNodes of
        Just head ->
            depthChildren edges (updateNode edges nodes head (Just parent)) parent (List.drop 1 foldNodes)

        Nothing ->
            nodes


updateDepth : Edges -> Nodes -> Node -> Maybe Node -> ( Bool, Nodes )
updateDepth edges nodes node parent =
    let
        ( parentID, parentDepth ) =
            case parent of
                Just parent ->
                    ( parent.id, parent.depth )

                Nothing ->
                    ( -1, -1 )
    in
        if node.parent == -1 || node.depth > parentDepth + 1 then
            ( True
            , List.map
                (\n ->
                    if n.id == node.id then
                        { n | depth = parentDepth + 1, parent = parentID }
                    else
                        n
                )
                nodes
            )
        else
            ( False, nodes )
