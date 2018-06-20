module Sugiyama.CycleRemoval exposing (..)

import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


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


findFlipEdges : Graph -> List ( Int, Int )
findFlipEdges ({ nodes, edges } as graph) =
    if List.length nodes == 2 then
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
                                        |> List.map (\e -> e.id)

                                newNodes =
                                    List.filter (\n -> n.id /= nodeId) nodes

                                newEdges =
                                    List.filter (\e -> e.from /= nodeId && e.to /= nodeId) edges
                            in
                                List.append flipEdges (findFlipEdges { graph | nodes = newNodes, edges = newEdges })

                        _ ->
                            []
