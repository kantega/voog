module Update exposing (..)

import Model exposing (..)
import Creators exposing (..)
import Random
import Char exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNode ->
            ( addNode model, Cmd.none )

        GenerateEdgeCallback ( from, to ) ->
            ( addEdge model from to, Cmd.none )

        GenerateEdge ->
            let
                n =
                    List.length model.nodes - 1
            in
                if n >= 0 then
                    ( model, Random.generate GenerateEdgeCallback (Random.pair (Random.int 0 n) (Random.int 0 n)) )
                else
                    ( model, Cmd.none )

        KeyMsg code ->
            case fromCode code of
                'W' ->
                    update GenerateNode model

                'E' ->
                    update GenerateEdge model

                _ ->
                    ( model, Cmd.none )
