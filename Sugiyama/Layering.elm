module Sugiyama.Layering exposing (layer, layerDown, layerDownInner, layerDownIteration, layerUp, layerUpIteration, updateChild, updateParent)

{-| Assign a layer to each node
-}

import Set
import Sugiyama.Helpers exposing (..)
import Sugiyama.Model exposing (..)


{-| Iterate down placing each child under its parent
Iterate up pulling parents down just above their children
-}
layer : Graph -> Graph
layer graph =
    graph
        |> layerDown
        |> layerUp


{-| Find edges of all nodes without parents and run down iteration
-}
layerDown : Graph -> Graph
layerDown graph =
    let
        edges =
            graph.edges
                |> List.map .to
                |> Set.fromList

        layerZeros =
            graph.nodes
                |> List.filter (\n -> not <| Set.member n.id edges)
    in
    layerDownInner [] graph layerZeros


{-| Start iteration from each of the layerZero nodes
-}
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


{-| Update all children of this node and add them to iteration list
-}
layerDownIteration : Graph -> Nodes -> List Int -> ( List Int, Graph )
layerDownIteration graph iterationNodes visited =
    case iterationNodes of
        head :: rest ->
            let
                newVisited =
                    head.id :: visited

                childrenIds =
                    getChildren graph head.id
                        |> List.map .id

                updatedNodes =
                    List.map (updateChild head childrenIds) graph.nodes

                newNodes =
                    updatedNodes
                        |> List.map (\( _, n ) -> n)

                nextNodes =
                    updatedNodes
                        |> List.filter (\( wasUpdated, _ ) -> wasUpdated)
                        |> List.map (\( _, n ) -> n)
                        |> List.append rest

                newGraph =
                    { graph | nodes = newNodes }
            in
            layerDownIteration newGraph nextNodes newVisited

        _ ->
            ( visited, graph )


{-| Move child under parent
-}
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


{-| layerUp drags parents of nodes down to the lowers layer above its children
Starts at bottom and iterates upwards
-}
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


{-| Update all parents of this layer then call next layer
-}
layerUpIteration : Int -> Graph -> Graph
layerUpIteration currentLayer ({ nodes } as graph) =
    if currentLayer < 0 then
        graph

    else
        let
            newGraph =
                { graph | nodes = List.map (updateParent graph currentLayer) nodes }
        in
        layerUpIteration (currentLayer - 1) newGraph


{-| Place parent just above children
-}
updateParent : Graph -> Int -> Node -> Node
updateParent graph currentLayer node =
    if node.y == Just currentLayer then
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

    else
        node
