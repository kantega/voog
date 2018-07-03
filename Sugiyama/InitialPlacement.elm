module Sugiyama.InitialPlacement exposing (..)

{-| Place the nodes from 0 to width of layer
-}

import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


{-| Find all layers and place nodes in each layer
-}
setInitialPosition : Graph -> Graph
setInitialPosition ({ nodes, edges } as graph) =
    let
        layers =
            nodes
                |> List.map (\n -> Maybe.withDefault -1 n.y)
                |> List.maximum
                |> Maybe.withDefault -1
                |> List.range 0

        sortedNodes =
            nodes
                |> List.sortWith (\a b -> compare (outEdges edges a.id) (outEdges edges b.id))
                |> List.reverse
    in
        setInitialPositionForLayer layers { graph | nodes = sortedNodes }


{-| Place all nodes in this layer
-}
setInitialPositionForLayer : List Int -> Graph -> Graph
setInitialPositionForLayer layers ({ nodes } as graph) =
    case layers of
        layer :: rest ->
            let
                ids =
                    graph.nodes
                        |> List.filter (\n -> n.y == Just layer)
                        |> List.map (\n -> n.id)
            in
                setInitialPositionForLayer rest { graph | nodes = setInitialPositionIteration 0 ids nodes }

        _ ->
            graph


{-| Kind of mapping of each node
Place the node at the next x position if it is in this layer
-}
setInitialPositionIteration : Int -> List Int -> Nodes -> Nodes
setInitialPositionIteration x ids nodes =
    case nodes of
        node :: rest ->
            if List.member node.id ids then
                { node | x = Just x } :: (setInitialPositionIteration (x + 1) ids rest)
            else
                node :: (setInitialPositionIteration x ids rest)

        [] ->
            []
