module Sugiyama.CycleRemoval exposing (removeCycles)

{-| A modification of
GreedyFAS as described in <http://www.vldb.org/pvldb/vol10/p133-simpson.pdf>
-}

import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


{-| Identify minimal set of edges that needs to be flipped and flip them
Store that they have been flipped for later restoration
-}
removeCycles : Graph -> Graph
removeCycles ({ edges } as graph) =
    let
        flipEdges =
            findFlipEdges graph

        newEdges =
            List.map
                (\e ->
                    if List.member e.id flipEdges then
                        reverse e
                    else
                        e
                )
                edges
    in
        { graph | edges = newEdges }


{-| Modified GreedyFAS
-}
findFlipEdges : Graph -> List ( Int, Int )
findFlipEdges ({ nodes, edges } as graph) =
    if List.length nodes <= 1 then
        []
    else
        let
            sinks =
                nodes
                    |> List.filter (\n -> List.length (getChildren graph n.id) == 0)
                    |> List.map (\n -> n.id)

            sources =
                nodes
                    |> List.filter (\n -> List.length (getParents graph n.id) == 0)
                    |> List.map (\n -> n.id)

            removeNodes =
                List.append sinks sources
        in
            if List.length removeNodes > 0 then
                let
                    newNodes =
                        List.filter
                            (\n -> not (List.member n.id removeNodes))
                            nodes
                in
                    findFlipEdges { graph | nodes = newNodes }
            else
                let
                    max =
                        nodes
                            |> List.map (\n -> ( n.id, List.length (getChildren graph n.id), List.length (getParents graph n.id) ))
                            |> List.sortWith (\( n1, c1, p1 ) ( n2, c2, p2 ) -> compare (c1 - p1) (c2 - p2))
                            |> List.reverse
                            |> List.head
                in
                    case max of
                        Just ( nodeId, c, p ) ->
                            let
                                flipEdges =
                                    edges
                                        |> List.filter (\e -> e.to == nodeId)
                                        |> List.map .id

                                newNodes =
                                    List.filter (\n -> n.id /= nodeId) nodes

                                newEdges =
                                    List.filter (\e -> e.from /= nodeId && e.to /= nodeId) edges
                            in
                                List.append flipEdges (findFlipEdges { graph | nodes = newNodes, edges = newEdges })

                        _ ->
                            []
