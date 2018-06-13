module Update exposing (..)

import Random
import Char exposing (..)
import Model exposing (..)
import Communication exposing (..)
import Depth exposing (..)
import Action exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNodes nodes ->
            ( addNodes model nodes, Cmd.none )

        AddEdges edges ->
            ( addEdges model edges, Cmd.none )

        ClickNode id ->
            ( toggleNode model id, Cmd.none )

        ClickEdge id ->
            ( toggleEdge model id, Cmd.none )

        SocketMsg msg ->
            handleCommunication model msg
