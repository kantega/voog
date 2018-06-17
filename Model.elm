module Model exposing (..)

import Dict
import Keyboard exposing (..)


type Msg
    = ClickNode Int
    | ClickEdge ( Int, Int )
    | SocketMsg String


type alias Model =
    { nodes : Nodes
    , edges : Edges
    }


type alias Nodes =
    List Node


type alias Edges =
    List Edge


type alias Node =
    InfoElement NodeCore


type alias InfoNode =
    InfoElement NodeCore


type alias NodeCore =
    { id : Int
    , selected : Bool
    , position : Maybe Point
    }


type alias Info =
    Dict.Dict String String


type alias InfoElement a =
    { a | info : Info }


type alias Edge =
    InfoElement
        { id : ( Int, Int )
        , from : Int
        , to : Int
        , selected : Bool
        , position : Maybe Line
        }


type Line
    = Straight StraightLine
    | Curved CurvedLine


type alias StraightLine =
    { from : Point
    , to : Point
    }


type alias CurvedLine =
    { from : Point
    , to : Point
    , via : Point
    }


type alias Point =
    { x : Int
    , y : Int
    }
