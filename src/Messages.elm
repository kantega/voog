module Messages exposing (..)

import Model exposing (..)
import Window
import Time


type Msg
    = ClickNode Int
    | ClickEdge ( Int, Int )
    | InputMsg String
    | Tick Time.Time
    | WindowSize Window.Size
    | MouseMove Point
    | MouseUp Point
    | MouseDown Point
