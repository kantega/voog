module Model exposing (..)

import Time
import Keyboard exposing (..)


type alias Model =
    { nodes : Nodes
    , edges : Edges
    , position : { x : Float, y : Float }
    , mouse : Maybe Point
    , drag : Bool
    , windowSize : Maybe ( Int, Int )
    , zoom : Float
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
    { addNodes : List InputNode
    , addEdges : List InputEdge
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
    , size : Maybe Int
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
    { x : Int
    , y : Int
    }
