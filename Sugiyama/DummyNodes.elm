module Sugiyama.DummyNodes exposing (..)

import Dict
import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


addDummies : Graph -> Graph
addDummies ({ edges } as graph) =
    addDummiesIteration graph edges


addDummiesIteration : Graph -> Edges -> Graph
addDummiesIteration graph iterationEdges =
    case List.head iterationEdges of
        Just edge ->
            let
                newGraph =
                    addDummy graph edge

                newIterationEdges =
                    List.drop 1 iterationEdges
            in
                addDummiesIteration newGraph newIterationEdges

        Nothing ->
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

                                    dummyEdges =
                                        { from = fromNode.id
                                        , to = id
                                        , reversed = direction
                                        }
                                            :: ({ from = id + List.length dummyNodes - 1
                                                , to = toNode.id
                                                , reversed = direction
                                                }
                                                    :: createDummyEdges id (List.drop 1 missingLayers) direction
                                               )

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
    case List.head missingLayers of
        Just layer ->
            let
                node =
                    { id = id
                    , x = Nothing
                    , y = Just layer
                    , dummy = True
                    }

                rest =
                    List.drop 1 missingLayers
            in
                node :: (createDummyNodes (id + 1) rest)

        _ ->
            []


createDummyEdges : Int -> List Int -> Bool -> Edges
createDummyEdges id missingLayers reversed =
    case List.head missingLayers of
        Just layer ->
            let
                edge =
                    { from = id
                    , to = id + 1
                    , reversed = reversed
                    }

                rest =
                    List.drop 1 missingLayers
            in
                edge :: (createDummyEdges (id + 1) rest reversed)

        _ ->
            []
