module Voog.Messages exposing (..)

import Html.Events exposing (on)
import Html
import Json.Decode exposing (map, map2, map3, at, int)
import Voog.Model exposing (..)
import Window
import Time


type Msg
    = ClickNode Int
    | ClickEdge ( Int, Int )
    | CloseInfo (Int, Int, Int)
    | InputMsg String
    | Tick Time.Time
    | WindowSize Window.Size
    | MouseMove (Int, Int)
    | MouseUp (Int, Int, Int)
    | MouseDown (Int, Int, Int)
    | MouseWheel Int


onMouseWheel : (Int -> msg) -> Html.Attribute msg
onMouseWheel tagger =
    on "mousewheel" (map tagger (at ["wheelDelta"] int))

onMouseMove : ((Int, Int) -> msg) -> Html.Attribute msg
onMouseMove tagger =
    on "mousemove" (map tagger (map2 (,) (at ["clientX"] int) (at ["clientY"] int)) )

onMouseDown : ((Int, Int, Int) -> msg) -> Html.Attribute msg
onMouseDown tagger =
    on "mousedown" (map tagger (map3 (,,) (at ["which"] int) (at ["clientX"] int) (at ["clientY"] int)) )

onMouseUp : ((Int, Int, Int) -> msg) -> Html.Attribute msg
onMouseUp tagger =
    on "mouseup" (map tagger (map3 (,,) (at ["which"] int) (at ["clientX"] int) (at ["clientY"] int)) )
