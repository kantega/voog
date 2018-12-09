module Voog.Model exposing (Edge, Edges, Flags, Info, InfoElement, Input, InputEdge, InputMovement, InputNode, Line(..), Model, Movement, MultiLine, Node, Nodes, Point, StraightLine, SystemEdge, SystemNode)

import Svg exposing (Svg)
import Voog.Messages exposing (..)


type alias Model =
    { flags : Flags
    , name : String
    , nodes : Nodes
    , edges : Edges
    , elementPosition : ( Float, Float )
    , position : { x : Float, y : Float }
    , mouse : Maybe Point
    , drag : Bool
    , windowSize : Maybe ( Int, Int )
    , zoom : Float
    , layout : Maybe String
    , nodeDistance : Maybe Float
    , attraction : Float
    , repulsion : Float
    , forceDampFactor : Float
    , force : Float
    , initiallyCentered : Bool
    , movements : List (Movement InputMovement)
    , center : Bool
    , invalidInput : Bool
    }


type alias Flags =
    { webSocket : Maybe String
    , disableWindowResize : Bool
    }


type alias Nodes =
    List Node


type alias Edges =
    List Edge


type alias Node =
    SystemNode InputNode


type alias Edge =
    SystemEdge InputEdge


type alias SystemNode a =
    { a
        | selected : Bool
        , position : Maybe Point
        , viewNode : Maybe (List (Svg Msg))
    }


type alias SystemEdge a =
    { a
        | id : ( Int, Int )
        , selected : Bool
        , position : Maybe Line
        , labelPosition : Maybe Point
        , dashOffset : Float
    }


type alias Input =
    { name : String
    , clear : Bool
    , size : Maybe ( Int, Int )
    , position : Maybe ( Int, Int )
    , layout : Maybe String
    , nodeDistance : Maybe Float
    , attraction : Float
    , repulsion : Float
    , forceDampFactor : Float
    , center : Maybe Bool
    , addMovement : List InputMovement
    , setNodes : List InputNode
    , setEdges : List InputEdge
    , removeNodes : List Int
    , removeEdges : List ( Int, Int )
    }


type alias InputNode =
    { id : Int
    , info : Info
    , classes : List String
    , name : Maybe String
    , href : Maybe String
    , shape : Maybe String
    , image : Maybe String
    , width : Maybe Float
    , height : Maybe Float
    , x : Maybe Float
    , y : Maybe Float
    }


type alias InputEdge =
    { from : Int
    , to : Int
    , info : Info
    , classes : List String
    , label : Maybe String
    , width : Maybe Float
    , speed : Maybe Float
    }


type alias Info =
    List ( String, String )


type alias InfoElement a =
    { a | info : Info }


type Line
    = Straight StraightLine
    | Multi MultiLine


type alias StraightLine =
    { from : Point
    , to : Point
    }


type alias MultiLine =
    List Point


type alias Point =
    { x : Float
    , y : Float
    }


type alias InputMovement =
    { from : Int
    , to : Int
    , duration : Float
    , icon : String
    , classes : List String
    }


type alias Movement a =
    { a
        | runningTime : Float
    }
