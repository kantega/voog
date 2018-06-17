module Sugiyama.InitialPlacement exposing (..)

import Sugiyama.Model exposing (..)


nodesInLayer : Graph -> Int -> Nodes
nodesInLayer { nodes } layer =
    List.filter
        (\n -> n.y == Just layer)
        nodes


setInitialPosition : Graph -> Graph
setInitialPosition ({ nodes } as graph) =
    let
        layers =
            nodes
                |> List.map (\n -> Maybe.withDefault -1 n.y)
                |> List.maximum
                |> Maybe.withDefault -1
                |> List.range 0
    in
        setInitialPositionForLayer layers graph


setInitialPositionForLayer : List Int -> Graph -> Graph
setInitialPositionForLayer layers ({ nodes } as graph) =
    case List.head layers of
        Just layer ->
            let
                ids =
                    nodesInLayer graph layer
                        |> List.map (\n -> n.id)

                nextLayers =
                    List.drop 1 layers
            in
                setInitialPositionForLayer nextLayers { graph | nodes = setInitialPositionIteration 0 ids nodes }

        Nothing ->
            graph


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
