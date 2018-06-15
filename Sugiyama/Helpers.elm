module Sugiyama.Helpers exposing (..)

import Sugiyama.Model exposing (..)


toBasic : Graph -> BasicGraph
toBasic graph =
    { nodes = List.map (\n -> n.id) graph.nodes, edges = List.map (\{ from, to } -> ( from, to )) graph.edges }


reversedEdge : Edge -> Edge
reversedEdge edge =
    { edge
        | to = edge.from
        , from = edge.to
        , reversed = not edge.reversed
    }


getChildren : Graph -> Node -> Nodes
getChildren graph node =
    let
        ids =
            graph.edges
                |> List.filter (\{ from, to } -> from == node.id)
                |> List.map (\{ from, to } -> to)
    in
        List.filter (\n -> List.member n.id ids) graph.nodes


getParents : Graph -> Node -> Nodes
getParents graph node =
    let
        ids =
            graph.edges
                |> List.filter (\{ from, to } -> to == node.id)
                |> List.map (\{ from, to } -> from)
    in
        List.filter (\n -> List.member n.id ids) graph.nodes


childrenChainLength : Graph -> Node -> Int
childrenChainLength graph node =
    childrenChainLengthInner graph node []


childrenChainLengthInner : Graph -> Node -> List Int -> Int
childrenChainLengthInner graph node visited =
    let
        newVisited =
            node.id :: visited

        children =
            getChildren graph node
                |> List.filter (\n -> not (List.member n.id newVisited))
    in
        1 + (Maybe.withDefault -1 (List.maximum (List.map (\c -> childrenChainLengthInner graph c newVisited) children)))


setNodeDepth : Graph -> Int -> Int -> Graph
setNodeDepth graph id depth =
    { graph
        | nodes =
            List.map
                (\n ->
                    if n.id == id then
                        { n | y = Just depth }
                    else
                        n
                )
                graph.nodes
    }
