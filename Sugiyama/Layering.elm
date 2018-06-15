module Sugiyama.Layering exposing (..)

import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


layer : Graph -> Graph
layer graph =
    layerIteration graph []


layerIteration : Graph -> List Int -> Graph
layerIteration graph visited =
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
                    newGraph =
                        setNodeDepth graph root.id 0

                    ( newVisited, new2Graph ) =
                        layerIterationInner newGraph [ { root | y = Just 0 } ] visited
                in
                    layerIteration new2Graph newVisited

            Nothing ->
                graph


layerIterationInner : Graph -> Nodes -> List Int -> ( List Int, Graph )
layerIterationInner graph nodes visited =
    case List.head nodes of
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
                        |> List.append (List.drop 1 nodes)

                newGraph =
                    { graph | nodes = newNodes }
            in
                layerIterationInner newGraph nextNodes newVisited

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
