module Main exposing (..)

import Html
import Time
import WebSocket
import Window
import Task
import Mouse
import Model exposing (..)
import View exposing (..)
import Update exposing (..)
import Keyboard exposing (..)
import Sugiyama.Sugiyama exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( empty, Task.perform WindowSize Window.size )


empty : Model
empty =
    { nodes = [], edges = [], position = { x = 0, y = 0 }, drag = Nothing, windowSize = Nothing, zoom = 1 }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen "ws://127.0.0.1:8000/" SocketMsg
        , Time.every (30 * Time.millisecond) (always Tick)
        , Window.resizes WindowSize
        , Mouse.moves MouseMove
        , Mouse.ups MouseUp
        , Mouse.downs MouseDown
        ]
