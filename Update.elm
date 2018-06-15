module Update exposing (..)

import Random
import Char exposing (..)
import Model exposing (..)
import Communication exposing (..)
import Action exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickNode id ->
            ( toggleNode model id, Cmd.none )

        ClickEdge id ->
            ( toggleEdge model id, Cmd.none )

        SocketMsg msg ->
            handleCommunication model msg
