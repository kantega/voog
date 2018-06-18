module Sugiyama.CycleRemoval exposing (..)

import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


removeCycles : Graph -> Graph
removeCycles graph =
    removeCyclesIteration graph []


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
            Just root ->
                let
                    ( newVisited, newGraph ) =
                        removeCyclesInConnectedGraph graph [ root ] visited
                in
                    removeCyclesIteration newGraph newVisited

            Nothing ->
                graph


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
                        getChildren graph head
                            |> List.filter (\n -> not (List.member n.id newVisited))

                    parents =
                        getParents graph head
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
