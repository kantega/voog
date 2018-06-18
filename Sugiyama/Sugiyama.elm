module Sugiyama.Sugiyama exposing (..)

import Sugiyama.CrossReduction exposing (..)
import Sugiyama.InitialPlacement exposing (..)
import Sugiyama.DummyNodes exposing (..)
import Sugiyama.Layering exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.CycleRemoval exposing (..)
import Sugiyama.Helpers exposing (..)


sugiyama : BasicGraph -> Graph
sugiyama graph =
    graph
        |> initialize
        |> removeCycles
        |> layer
        |> addDummies
        |> setInitialPosition
        |> reduceCrossing


initialize : BasicGraph -> Graph
initialize { nodes, edges } =
    let
        newNodes =
            List.map (\n -> { id = n, dummy = False, x = Nothing, y = Nothing }) nodes

        newEdges =
            List.map (\( from, to ) -> { from = from, to = to, reversed = False }) edges
    in
        { nodes = newNodes, edges = newEdges }
