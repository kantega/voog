module Sugiyama.Sugiyama exposing (..)

import Set
import Sugiyama.CrossReduction exposing (..)
import Sugiyama.InitialPlacement exposing (..)
import Sugiyama.DummyNodes exposing (..)
import Sugiyama.Layering exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.CycleRemoval exposing (..)
import Sugiyama.Helpers exposing (..)
import Sugiyama.Placement exposing (..)


sugiyama : BasicGraph -> Graph
sugiyama graph =
    graph
        |> mergeBidirectional
        |> initialize
        |> removeCycles
        |> layer
        |> addDummies
        |> setInitialPosition
        |> reduceCrossing
        |> setPosition


mergeBidirectional : BasicGraph -> BasicGraph
mergeBidirectional ({ edges } as graph) =
    let
        edgeSet =
            Set.fromList edges

        newEdges =
            List.filterMap
                (\( from, to ) ->
                    if from < to && Set.member ( to, from ) edgeSet then
                        Nothing
                    else if from == to then
                        Nothing
                    else
                        Just ( from, to )
                )
                edges
    in
        { graph | edges = newEdges }


initialize : BasicGraph -> Graph
initialize { nodes, edges } =
    let
        newNodes =
            List.map (\n -> { id = n, dummy = False, x = Nothing, y = Nothing }) nodes

        newEdges =
            List.map (\( from, to ) -> { from = from, to = to, reversed = False, num = Nothing, id = ( from, to ) }) edges
    in
        { nodes = newNodes, edges = newEdges }
