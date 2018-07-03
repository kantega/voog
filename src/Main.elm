module Main exposing (..)

import AnimationFrame
import Html
import WebSocket
import Window
import Task
import Ports exposing (..)
import Voog.Model exposing (..)
import Voog.Messages exposing (..)
import Voog.View
import Voog.Update


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = Voog.View.view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { flags = flags
            , name = ""
            , elementPosition = ( 0, 0 )
            , nodes = []
            , edges = []
            , position = { x = 0, y = 0 }
            , mouse = Nothing
            , drag = False
            , windowSize = Nothing
            , zoom = 1
            , layout = Nothing
            , nodeDistance = Nothing
            , attraction = Nothing
            , repulsion = Nothing
            , doForce = False
            , initiallyCentered = False
            }
    in
        ( model, Task.perform WindowSize Window.size )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ws =
            case model.flags.webSocket of
                Just webSocket ->
                    [ WebSocket.listen webSocket InputMsg ]

                _ ->
                    []
    in
        Sub.batch <|
            List.append ws
                [ Ports.input InputMsg
                , AnimationFrame.diffs Tick
                , Window.resizes WindowSize
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( Voog.Update.update msg model, Cmd.none )
