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
        , categoryColor : Maybe String
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
    , name : Maybe String
    , shape : Maybe String
    , image : Maybe String
    , category : Maybe String
    , color : Maybe String
    , size : Maybe Int
    }


type alias InputEdge =
    { from : Int
    , to : Int
    , info : Info
    , label : Maybe String
    , width : Maybe Float
    , color : Maybe String
    , speed : Maybe Float
    , dashColor : Maybe String
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
