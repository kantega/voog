module Sugiyama.Helpers exposing (..)

import Sugiyama.Model exposing (..)


edgeIn : Edge -> Edges -> Bool
edgeIn { from, to } edges =
    edges
        |> List.map (\{ from, to } -> ( from, to ))
        |> List.member ( from, to )


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


toBasic : Graph -> BasicGraph
toBasic graph =
    { nodes = List.map (\n -> n.id) graph.nodes, edges = List.map (\{ from, to } -> ( from, to )) graph.edges }
