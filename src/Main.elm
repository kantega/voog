module Main exposing (init, main, subscriptions, update)

import Browser.Events exposing (..)
import Html
import Ports exposing (..)
import Task
import Voog.Messages exposing (..)
import Voog.Model exposing (..)
import Voog.Update
import Voog.View
import PortFunnel.WebSocket as WebSocket
import Browser.Events as Window
import Browser.Dom exposing (getViewport)
import Browser


main : Program Flags Model Msg
main =
    Browser.element
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
            , movements = []
            , nodes = []
            , edges = []
            , position = { x = 0, y = 0 }
            , mouse = Nothing
            , drag = False
            , windowSize = Nothing
            , zoom = 1
            , layout = Nothing
            , nodeDistance = Nothing
            , attraction = 0
            , repulsion = 0
            , force = 0
            , forceDampFactor = 0
            , initiallyCentered = False
            , center = False
            , invalidInput = False
            }
    in
    ( model, Task.perform WindowSize getViewport )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ws =
            case model.flags.webSocket of
                Just webSocket ->
                    [ WebSocket.listen webSocket InputMsg ]

                _ ->
                    []

        resize =
            if model.flags.disableWindowResize then
                []

            else
                [ Window.resizes WindowSize ]
    in
    Sub.batch <|
        ([ Ports.input InputMsg
         , diffs Tick
         ]
            |> List.append ws
            |> List.append resize
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( Voog.Update.update msg model, Cmd.none )
