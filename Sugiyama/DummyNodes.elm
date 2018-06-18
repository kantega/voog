module Sugiyama.DummyNodes exposing (..)

import Dict
import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


addDummies : Graph -> Graph
addDummies ({ edges } as graph) =
    addDummiesIteration graph edges


addDummiesIteration : Graph -> Edges -> Graph
addDummiesIteration graph iterationEdges =
    case iterationEdges of
        edge :: rest ->
            let
                newGraph =
                    addDummy graph edge
            in
                addDummiesIteration newGraph rest

        _ ->
            graph


addDummy : Graph -> Edge -> Graph
addDummy ({ nodes, edges } as graph) ({ from, to } as edge) =
    let
        fromNode =
            getNode graph from

        toNode =
            getNode graph to
    in
        case ( fromNode, toNode ) of
            ( Just fromNode, Just toNode ) ->
                case ( fromNode.y, toNode.y ) of
                    ( Just y1, Just y2 ) ->
                        let
                            direction =
                                y1 < y2

                            ( p1, p2 ) =
                                if y1 > y2 then
                                    ( y2, y1 )
                                else
                                    ( y1, y2 )

                            missingLayers =
                                List.range (p1 + 1) (p2 - 1)
                        in
                            if List.length missingLayers > 0 then
                                let
                                    id =
                                        nodes
                                            |> List.map (\n -> n.id)
                                            |> List.maximum
                                            |> Maybe.withDefault 0
                                            |> \n -> n + 1

                                    dummyNodes =
                                        createDummyNodes id missingLayers

                                    ids =
                                        List.append
                                            (fromNode.id :: (List.range id (id + List.length missingLayers - 1)))
                                            [ toNode.id ]

                                    dummyEdges =
                                        createDummyEdges (fromNode.id, toNode.id) 0 ids direction

                                    newNodes =
                                        List.append nodes dummyNodes

                                    newEdges =
                                        edges
                                            |> List.filter (\{ from, to } -> ( from, to ) /= ( fromNode.id, toNode.id ))
                                            |> List.append dummyEdges
                                in
                                    { graph
                                        | nodes = newNodes
                                        , edges = newEdges
                                    }
                            else
                                graph

                    _ ->
                        graph

            _ ->
                graph


createDummyNodes : Int -> List Int -> Nodes
createDummyNodes id missingLayers =
    case missingLayers of
        layer :: rest ->
            let
                node =
                    { id = id
                    , x = Nothing
                    , y = Just layer
                    , dummy = True
                    }
            in
                node :: createDummyNodes (id + 1) rest

        _ ->
            []


createDummyEdges : (Int, Int) -> Int -> List Int -> Bool -> Edges
createDummyEdges originalId num ids reversed =
    case ids of
        from :: to :: rest ->
            let
                edge =
                    { id = originalId
                    , num = Just num
                    , from = from
                    , to = to
                    , reversed = reversed
                    }
            in
                edge :: createDummyEdges originalId (num + 1) (to :: rest) reversed

        _ ->
            []
