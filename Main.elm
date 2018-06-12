module Main exposing (..)

import Html
import Time
import WebSocket
import Model exposing (..)
import View exposing (..)
import Update exposing (..)
import Keyboard exposing (..)


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
    ( empty, Cmd.none )


empty : Model
empty =
    { nodes = [], edges = [], depthNodes = [] }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen "ws://127.0.0.1:8000/" SocketMsg
        ]
