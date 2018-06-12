module Communication exposing (..)

import Json.Decode exposing (..)
import Model exposing (..)
import Action exposing (..)


handleCommunication : Model -> String -> ( Model, Cmd Msg )
handleCommunication model msg =
    case decodeString (maybe (field "command" string)) msg of
        Ok (Just command) ->
            case command of
                "addNodes" ->
                    case decodeString (maybe (field "nodes" (keyValuePairs string))) msg of
                        Ok (Just rawNodes) ->
                            ( addNodes model (List.filterMap validateIntStringPair rawNodes), Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                "removeNodes" ->
                    case decodeString (maybe (field "nodes" (list int))) msg of
                        Ok (Just nodes) ->
                            ( removeNodes model nodes, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                "addEdges" ->
                    case decodeString (maybe (field "edges" (list (map2 (,) (index 0 int) (index 1 int))))) msg of
                        Ok (Just edges) ->
                            ( addEdges model edges , Cmd.none )
                        _ ->
                            ( model, Cmd.none )

                "removeEdges" ->
                    case decodeString (maybe (field "edges" (list (map2 (,) (index 0 int) (index 1 int))))) msg of
                        Ok (Just edges) ->
                            ( removeEdges model edges , Cmd.none )
                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


validateIntStringPair : ( String, String ) -> Maybe ( Int, String )
validateIntStringPair ( int, str ) =
    case (String.toInt int) of
        Ok int ->
            Just ( int, str )

        _ ->
            Nothing
