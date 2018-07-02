module Sugiyama.Layering exposing (..)

import Set
import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


layer : Graph -> Graph
layer graph =
    graph
        |> layerDown
        |> layerUp


layerDown : Graph -> Graph
layerDown graph =
    let
        edges =
            graph.edges
                |> List.map (\e -> Tuple.second e.id)
                |> Set.fromList

        layerZeros =
            graph.nodes
                |> List.filter (\n -> not <| Set.member n.id edges)
    in
        layerDownInner [] graph layerZeros


layerDownInner : List Int -> Graph -> List Node -> Graph
layerDownInner visited graph layerZeros =
    case layerZeros of
        head :: rest ->
            let
                newGraph =
                    setNodeDepth graph head.id 0

                ( newVisited, new2Graph ) =
                    layerDownIteration newGraph [ { head | y = Just 0 } ] visited
            in
                layerDownInner newVisited new2Graph rest

        _ ->
            graph


layerDownIteration : Graph -> Nodes -> List Int -> ( List Int, Graph )
layerDownIteration graph iterationNodes visited =
    case iterationNodes of
        head :: rest ->
            let
                newVisited =
                    head.id :: visited

                childrenIds =
                    getChildren graph head.id
                        |> List.map (\n -> n.id)

                updatedNodes =
                    List.map (updateChild head childrenIds) graph.nodes

                newNodes =
                    updatedNodes
                        |> List.map (\( b, n ) -> n)

                nextNodes =
                    updatedNodes
                        |> List.filter (\( b, n ) -> b)
                        |> List.map (\( b, n ) -> n)
                        |> List.append rest

                newGraph =
                    { graph | nodes = newNodes }
            in
                layerDownIteration newGraph nextNodes newVisited

        _ ->
            ( visited, graph )


updateChild : Node -> List Int -> Node -> ( Bool, Node )
updateChild parent children node =
    if List.member node.id children then
        let
            y =
                Maybe.withDefault -1 node.y

            parentY =
                Maybe.withDefault -1 parent.y
        in
            if y < parentY + 1 then
                ( True, { node | y = Just (parentY + 1) } )
            else
                ( False, node )
    else
        ( False, node )


layerUp : Graph -> Graph
layerUp graph =
    let
        layers =
            graph.nodes
                |> List.map (\n -> Maybe.withDefault 0 n.y)
                |> List.maximum
                |> Maybe.withDefault 0
    in
        layerUpIteration (layers - 1) graph


layerUpIteration : Int -> Graph -> Graph
layerUpIteration layer ({ nodes } as graph) =
    if layer < 0 then
        graph
    else
        let
            newGraph =
                { graph | nodes = List.map (updateParent graph) nodes }
        in
            layerUpIteration (layer - 1) newGraph


updateParent : Graph -> Node -> Node
updateParent graph node =
    let
        children =
            getChildren graph node.id

        minChildLayer =
            children
                |> List.map (\n -> Maybe.withDefault -1 n.y)
                |> List.minimum
                |> Maybe.withDefault 1
    in
        if Maybe.withDefault -1 node.y < minChildLayer - 1 then
            { node | y = Just (minChildLayer - 1) }
        else
            node
