module Messages exposing (..)

import Html.Events exposing (on)
import Html
import Json.Decode exposing (map, at, float)
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
    | MouseWheel Float


onMouseWheel : (Float -> msg) -> Html.Attribute msg
onMouseWheel tagger =
    on "mousewheel" (map tagger (at ["wheelDelta"] float))
