module Communication exposing (..)

import Dict
import Json.Decode exposing (..)
import Model exposing (..)
import Action exposing (..)


handleCommunication : Model -> String -> ( Model, Cmd Msg )
handleCommunication model msg =
    case decodeString (maybe (field "command" string)) msg of
        Ok (Just command) ->
            let
                update =
                    case decodeString (maybe (field "update" string)) msg of
                        Ok (Just update) ->
                            True

                        _ ->
                            False
            in
                case command of
                    "addNodes" ->
                        case decodeString (maybe (field "nodes" (dict (dict string)))) msg of
                            Ok (Just rawNodes) ->
                                ( addNodes model update (Dict.toList rawNodes) False, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    "removeNodes" ->
                        case decodeString (maybe (field "nodes" (list int))) msg of
                            Ok (Just nodes) ->
                                ( removeNodes model nodes, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    "addEdges" ->
                        case decodeString (maybe (field "edges" (dict (dict string)))) msg of
                            Ok (Just edges) ->
                                let
                                    parsedEdges =
                                        (List.filterMap
                                            (\( k, v ) ->
                                                let
                                                    key =
                                                        decodeString (maybe (map2 (,) (index 0 int) (index 1 int))) k
                                                in
                                                    case key of
                                                        Ok (Just key) ->
                                                            Just ( key, v )

                                                        _ ->
                                                            Nothing
                                            )
                                            (Dict.toList edges)
                                        )
                                in
                                    ( addEdges model update parsedEdges False, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    "removeEdges" ->
                        case decodeString (maybe (field "edges" (list (map2 (,) (index 0 int) (index 1 int))))) msg of
                            Ok (Just edges) ->
                                ( removeEdges model edges, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
