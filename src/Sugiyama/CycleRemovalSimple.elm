module Sugiyama.CycleRemovalSimple exposing (removeCyclesSimple)

{-| A DFS search, reversing all edges opposite of the traversal direction
-}

import Sugiyama.Helpers exposing (..)
import Sugiyama.Model exposing (..)


{-| Initialize with no visited nodes
-}
removeCyclesSimple : Graph -> Graph
removeCyclesSimple graph =
    removeCyclesIteration graph []


{-| Find the "root", the unvisited node with longest children chain length and start the search
If there exists multiple unconnected subgraphs it will search from "root" of each subgraph
-}
removeCyclesIteration : Graph -> List Int -> Graph
removeCyclesIteration graph visited =
    let
        root =
            graph.nodes
                |> List.filter (\n -> not (List.member n.id visited))
                |> List.sortWith (\a b -> compare (childrenChainLength graph a) (childrenChainLength graph b))
                |> List.reverse
                |> List.head
    in
    case root of
        Just rootInner ->
            let
                ( newVisited, newGraph ) =
                    removeCyclesInConnectedGraph graph [ rootInner ] visited
            in
            removeCyclesIteration newGraph newVisited

        Nothing ->
            graph


{-| Traverse graph
Whenever a node with edges pointing in to it is encountered reverse the edges so that they point out
-}
removeCyclesInConnectedGraph : Graph -> Nodes -> List Int -> ( List Int, Graph )
removeCyclesInConnectedGraph graph nodes visited =
    case nodes of
        head :: rest ->
            if List.member head.id visited then
                removeCyclesInConnectedGraph graph rest visited

            else
                let
                    newVisited =
                        head.id :: visited

                    children =
                        getChildren graph head.id
                            |> List.filter (\n -> not (List.member n.id newVisited))

                    parents =
                        getParents graph head.id
                            |> List.filter (\n -> not (List.member n.id newVisited))

                    parentIds =
                        List.map (\n -> n.id) parents

                    newEdges =
                        graph.edges
                            |> List.map
                                (\e ->
                                    if e.to == head.id && List.member e.from parentIds then
                                        reverse e

                                    else
                                        e
                                )

                    newChildren =
                        List.append children parents

                    newGraph =
                        { graph | edges = newEdges }

                    newNodes =
                        List.append newChildren rest
                in
                removeCyclesInConnectedGraph newGraph newNodes newVisited

        _ ->
            ( visited, graph )
