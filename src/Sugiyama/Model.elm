module Sugiyama.Model exposing (BasicEdge, BasicEdges, BasicGraph, BasicNode, BasicNodes, CrossEdges, Direction(..), DoubleTrippleInt, Edge, Edges, Graph, IdPos, Node, Nodes, TrippleInt)

import Dict


type alias BasicGraph =
    { nodes : BasicNodes
    , edges : BasicEdges
    }


type alias BasicNodes =
    List BasicNode


type alias BasicEdges =
    List BasicEdge


type alias BasicNode =
    Int


type alias BasicEdge =
    ( Int, Int )


type alias TrippleInt =
    ( Int, Int, Int )


type alias DoubleTrippleInt =
    ( TrippleInt, TrippleInt )


type alias Graph =
    { nodes : Nodes
    , edges : Edges
    }


type alias Nodes =
    List Node


type alias Edges =
    List Edge


type alias Node =
    { id : Int
    , dummy : Bool
    , x : Maybe Int
    , y : Maybe Int
    }


type alias Edge =
    { id : ( Int, Int )
    , num : Maybe Int
    , from : Int
    , to : Int
    , reversed : Bool
    }


type alias CrossEdges =
    Dict.Dict TrippleInt (Dict.Dict Int Int)


type alias IdPos =
    Dict.Dict Int Int


type Direction
    = Down
    | Up
