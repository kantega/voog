module Main exposing (..)

import Html
import Time
import Model exposing (..)
import View exposing (..)
import Update exposing (..)
import Creators exposing (..)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        ]
