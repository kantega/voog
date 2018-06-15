module Sugiyama.Model exposing (..)


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
    { from : Int
    , to : Int
    , reversed : Bool
    }
