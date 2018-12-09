module Sugiyama.InitialPlacement exposing (getNodeX, indexOf, minParentX, parentsOfChildren, setInitialPosition, setInitialPositionForLayer, setInitialPositionIteration)

{-| Place the nodes from 0 to width of layer
-}

import Sugiyama.Helpers exposing (..)
import Sugiyama.Model exposing (..)


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
                |> List.sortWith
                    (\a b ->
                        compare
                            (outEdges edges a.id + parentsOfChildren edges a.id)
                            (outEdges edges b.id + parentsOfChildren edges b.id)
                    )
                |> List.reverse
    in
    setInitialPositionForLayer layers { graph | nodes = sortedNodes }


{-| Place all nodes in this layer
-}
setInitialPositionForLayer : List Int -> Graph -> Graph
setInitialPositionForLayer layers ({ nodes, edges } as graph) =
    case layers of
        layer :: rest ->
            let
                ids =
                    graph.nodes
                        |> List.filter (\n -> n.y == Just layer)
                        |> List.map (\n -> n.id)

                newNodes =
                    if layer > 0 then
                        List.sortWith
                            (\a b ->
                                compare
                                    (minParentX nodes edges a.id)
                                    (minParentX nodes edges b.id)
                            )
                            nodes

                    else
                        nodes
            in
            setInitialPositionForLayer rest { graph | nodes = setInitialPositionIteration 0 ids newNodes }

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
                { node | x = Just x } :: setInitialPositionIteration (x + 1) ids rest

            else
                node :: setInitialPositionIteration x ids rest

        [] ->
            []


{-| Find id of (one of the) parent(s)
-}
indexOf : List Int -> Int -> Int
indexOf elements target =
    let
        indexOfInner : List Int -> Int -> Int -> Int
        indexOfInner elementsInner targetInner index =
            case elementsInner of
                element :: rest ->
                    if element == targetInner then
                        index

                    else
                        indexOfInner rest targetInner (index + 1)

                [] ->
                    -1
    in
    indexOfInner elements target 0


{-| Get x pos of node
-}
getNodeX : Nodes -> Int -> Int
getNodeX nodes id =
    case nodes of
        node :: rest ->
            if node.id == id then
                Maybe.withDefault 0 node.x

            else
                getNodeX rest id

        [] ->
            0


{-| Find min x of all parents
-}
minParentX : Nodes -> Edges -> Int -> Int
minParentX nodes edges id =
    let
        ret =
            edges
                |> List.filter (\e -> e.to == id)
                |> List.map (\e -> e.from)
                |> List.map (getNodeX nodes)
                |> List.maximum
                |> Maybe.withDefault -1
    in
    ret


{-| Find sum [number of parents of child] for each child
-}
parentsOfChildren : Edges -> Int -> Int
parentsOfChildren edges id =
    let
        ret =
            edges
                |> List.filter (\e -> e.from == id)
                |> List.map (\e -> e.to)
                |> List.map
                    (\childId ->
                        edges
                            |> List.filter (\e -> e.to == childId)
                            |> List.length
                    )
                |> List.sum
    in
    ret
