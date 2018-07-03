module Sugiyama.Sugiyama exposing (..)

{-| SUGIYAMA

The Sugiyama method is a general layout method for layered graphs. It consists of 4 Steps:

1.  Cycle Removal: Reverse the direction of certain edges
    a. Achieve an acyclic graph.

    One approach is to reverse a minimal amount of edges. A modification of
    GreedyFAS as described in <http://www.vldb.org/pvldb/vol10/p133-simpson.pdf>
    is implemented in "CycleRemoval".
    A DFS search, reversing all edges opposite of the traversal direction is
    implemented in "CycleRemovalSimple".

2.  Layering: Assign each node to a vertical layer.
    a. All edges should be pointing downwards (not to the side or up).
    b. Reduce total vertical edge length.

3.  Ordering: Order the nodes horizontally in each layer.
    a. Minimize the number of crossing edges.

    The nodes in one layer is fixed while improving the layer above or below.
    This is achieved by bubble-sorting the nodes in each layer. Just like bubble
    sort we evaluate if the number of crossing edges is reduced by flipping the
    position of two adjacent nodes.
    This is repeated upwards and downwards

4.  Positioning: Assign x coordinates to each node.
    a. Minimize the horizontal distance between a node and its connected neighbours in adjacent layers.
    b. Center a node relative to its children or parents

    The first condition is achieved by minimizing Δx. By minimizing Δx^2 the
    second condition is also achieved.

-}

import Set
import Sugiyama.CrossReduction exposing (..)
import Sugiyama.InitialPlacement exposing (..)
import Sugiyama.DummyNodes exposing (..)
import Sugiyama.Layering exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.CycleRemoval exposing (..)
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


sugiyamaCustom : BasicGraph -> (Graph -> Graph) -> Graph
sugiyamaCustom graph cycleRemoval =
    graph
        |> mergeBidirectional
        |> initialize
        |> cycleRemoval
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
