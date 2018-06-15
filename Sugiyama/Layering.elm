module Sugiyama.Layering exposing (..)

import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


layer : Graph -> Graph
layer graph =
    graph
        |> layerDown []
        |> layerUp


layerDown : List Int -> Graph -> Graph
layerDown visited graph =
    let
        root =
            graph.nodes
                |> List.filter (\n -> not (List.member n.id visited))
                |> List.sortWith (\a b -> compare (childrenChainLength graph a) (childrenChainLength graph b))
                |> List.reverse
                |> List.head

        _ =
            Debug.log "r" root
    in
        case root of
            Just root ->
                let
                    newGraph =
                        setNodeDepth graph root.id 0

                    ( newVisited, new2Graph ) =
                        layerDownIteration newGraph [ { root | y = Just 0 } ] visited
                in
                    layerDown newVisited new2Graph

            Nothing ->
                graph


layerDownIteration : Graph -> Nodes -> List Int -> ( List Int, Graph )
layerDownIteration graph iterationNodes visited =
    case List.head iterationNodes of
        Just head ->
            let
                newVisited =
                    head.id :: visited

                childrenIds =
                    getChildren graph head
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
                        |> List.append (List.drop 1 iterationNodes)

                newGraph =
                    { graph | nodes = newNodes }
            in
                layerDownIteration newGraph nextNodes newVisited

        Nothing ->
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
            getChildren graph node

        minChildLayer =
            children
                |> List.map (\n -> Maybe.withDefault -1 n.y)
                |> List.minimum
                |> Maybe.withDefault 0
    in
        if Maybe.withDefault -1 node.y < minChildLayer - 1 then
            { node | y = Just (minChildLayer - 1) }
        else
            node
