module Model exposing (..)

import Keyboard exposing (..)


type Msg
    = GenerateNode
    | GenerateEdge
    | GenerateEdgeCallback ( Int, Int )
    | KeyMsg Keyboard.KeyCode


type alias Model =
    { nodes : Nodes
    , edges : Edges
    , depthNodes : DepthNodes
    }


type alias Placed =
    { nodes : PlacedNodes
    , edges : PlacedEdges
    , depthNodes : DepthNodes
    }


type alias PlacedNodes =
    List PlacedNode


type alias DepthNodes =
    List DepthNode


type alias PlacedEdges =
    List PlacedEdge


type alias Nodes =
    List Node


type alias Edges =
    List Edge


type alias Node =
    { id : Int
    , name : String
    }


type alias Edge =
    { from : Int
    , to : Int
    }


type alias DepthNode =
    { id : Int
    , parent : Int
    , depth : Int
    }


type alias PlacedNode =
    { id : Int
    , name : String
    , x : Int
    , y : Int
    }


type alias PlacedEdge =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    , cx : Int
    , cy : Int
    }
