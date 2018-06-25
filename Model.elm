module Model exposing (..)

import Dict
import Window
import Keyboard exposing (..)


type Msg
    = ClickNode Int
    | ClickEdge ( Int, Int )
    | SocketMsg String
    | Tick
    | WindowSize Window.Size
    | MouseMove Point
    | MouseUp Point
    | MouseDown Point


type alias Model =
    { nodes : Nodes
    , edges : Edges
    , position : { x : Float, y : Float }
    , drag : Maybe Point
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
    | Curved CurvedLine
    | Multi MultiLine


type alias StraightLine =
    { from : Point
    , to : Point
    }


type alias MultiLine =
    List Point


type alias CurvedLine =
    { from : Point
    , to : Point
    , via : Point
    }


type alias Point =
    { x : Int
    , y : Int
    }
