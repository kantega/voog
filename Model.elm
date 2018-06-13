module Model exposing (..)

import Dict
import Keyboard exposing (..)


type Msg
    = ClickNode Int
    | ClickEdge (Int, Int)
    | SocketMsg String


type alias Model =
    { nodes : Nodes
    , edges : Edges
    , depthNodes : DepthNodes
    , placedNodes : PlacedNodes
    , placedEdges : PlacedEdges
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
    , name : Maybe String
    , selected : Bool
    , info : Info
    }


type alias Info =
    Dict.Dict String String


type alias Edge =
    { id: (Int, Int)
    , from: Int
    , to: Int
    , selected : Bool
    , info : Info
    }


type alias DepthNode =
    { id : Int
    , parent : Int
    , depth : Int
    }


type alias PlacedNode =
    { id : Int
    , x : Int
    , y : Int
    }


type alias PlacedEdge =
    { id: (Int, Int)
    , x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    , cx : Int
    , cy : Int
    }
