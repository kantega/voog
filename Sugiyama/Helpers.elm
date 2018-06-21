module Sugiyama.Helpers exposing (..)

import Dict exposing (..)
import Sugiyama.Model exposing (..)


toBasic : Graph -> BasicGraph
toBasic graph =
    { nodes = List.map (\n -> n.id) graph.nodes, edges = List.map (\{ from, to } -> ( from, to )) graph.edges }


reverse : Edge -> Edge
reverse ({ from, to, reversed } as edge) =
    { edge
        | to = from
        , from = to
        , reversed = not reversed
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


reverseAll : Graph -> Graph
reverseAll ({ edges } as graph) =
    { graph | edges = List.map reverse edges }


getNode : Graph -> Int -> Maybe Node
getNode { nodes } id =
    nodes
        |> List.filter (\n -> n.id == id)
        |> List.head


getChildren : Graph -> Int -> Nodes
getChildren graph nodeId =
    let
        ids =
            graph.edges
                |> List.filter (\{ from, to } -> from == nodeId)
                |> List.map (\{ from, to } -> to)
    in
        List.filter (\n -> List.member n.id ids) graph.nodes


getParents : Graph -> Int -> Nodes
getParents graph nodeId =
    let
        ids =
            graph.edges
                |> List.filter (\{ from, to } -> to == nodeId)
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
            getChildren graph node.id
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


getXPos : Graph -> Int -> Dict Int (Maybe Int)
getXPos { nodes } layer =
    nodes
        |> List.filter (\n -> n.y == Just layer)
        |> List.map (\n -> ( n.id, n.x ))
        |> Dict.fromList


getLayerPos : Graph -> Dict Int (Maybe Int)
getLayerPos { nodes } =
    nodes
        |> List.map (\n -> ( n.id, n.y ))
        |> Dict.fromList
