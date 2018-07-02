module Model exposing (..)

import Time
import Keyboard exposing (..)


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
    }


type alias Flags =
    { webSocket : Maybe String
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
    , size : Maybe ( Int, Int )
    , position : Maybe ( Int, Int )
    , layout : Maybe String
    , nodeDistance : Maybe Float
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
    , shape : Maybe String
    , image : Maybe String
    , size : Maybe Float
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
