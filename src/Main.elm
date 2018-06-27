module Main exposing (..)

import AnimationFrame
import Html
import WebSocket
import Window
import Task
import Mouse
import Ports exposing (..)
import Model exposing (..)
import Messages exposing (..)
import View
import Update


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( empty, Task.perform WindowSize Window.size )


empty : Model
empty =
    { nodes = []
    , edges = []
    , position = { x = 0, y = 0 }
    , mouse = Nothing
    , drag = False
    , windowSize = Nothing
    , zoom = 1
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen "ws://127.0.0.1:8000/" InputMsg
        , Ports.input InputMsg
        , AnimationFrame.diffs Tick
        , Window.resizes WindowSize
        ]
