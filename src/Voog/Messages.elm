module Voog.Messages exposing (Msg(..), onMouseDown, onMouseMove, onMouseUp, onMouseWheel)

import Html
import Html.Events exposing (on)
import Json.Decode exposing (at, int, map, map2, map3)
import Time
import Browser.Events as Window
import Browser


type alias TrippleInt =
    ( Int, Int, Int )


type alias WindowSize =
    ( Int, Int )


type Msg
    = ClickNode Int
    | ClickEdge ( Int, Int )
    | CloseInfo TrippleInt
    | InputMsg String
    | Tick Float
    | UpdateWindowSize WindowSize
    | MouseMove ( Int, Int )
    | MouseUp TrippleInt
    | MouseDown TrippleInt
    | MouseWheel Int
    | AcceptInvalidInput


onMouseWheel : (Int -> msg) -> Html.Attribute msg
onMouseWheel tagger =
    on "mousewheel" (map tagger (at [ "wheelDelta" ] int))


onMouseMove : (( Int, Int ) -> msg) -> Html.Attribute msg
onMouseMove tagger =
    on "mousemove" (map tagger (map2 (\a b -> ( a, b )) (at [ "clientX" ] int) (at [ "clientY" ] int)))


onMouseDown : (TrippleInt -> msg) -> Html.Attribute msg
onMouseDown tagger =
    on "mousedown" (map tagger (map3 (\a b c -> ( a, b, c )) (at [ "which" ] int) (at [ "clientX" ] int) (at [ "clientY" ] int)))


onMouseUp : (TrippleInt -> msg) -> Html.Attribute msg
onMouseUp tagger =
    on "mouseup" (map tagger (map3 (\a b c -> ( a, b, c )) (at [ "which" ] int) (at [ "clientX" ] int) (at [ "clientY" ] int)))
