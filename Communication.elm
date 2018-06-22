module Communication exposing (..)

import Dict
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model exposing (..)
import Action exposing (..)


stringStringListDecoder : Decoder (List ( String, String ))
stringStringListDecoder =
    (list (map2 (,) (index 0 string) (index 1 string)))


nodeDecoder : Decoder InputNode
nodeDecoder =
    decode InputNode
        |> required "id" int
        |> optional "info" stringStringListDecoder []
        |> optional "name" (maybe string) Nothing
        |> optional "shape" (maybe string) Nothing
        |> optional "image" (maybe string) Nothing
        |> optional "category" (maybe string) Nothing
        |> optional "color" (maybe string) Nothing
        |> optional "size" (maybe int) Nothing


edgeDecoder : Decoder InputEdge
edgeDecoder =
    decode InputEdge
        |> required "from" int
        |> required "to" int
        |> optional "info" stringStringListDecoder []
        |> optional "label" (maybe string) Nothing
        |> optional "width" (maybe float) Nothing
        |> optional "color" (maybe string) Nothing
        |> optional "speed" (maybe float) Nothing
        |> optional "dashColor" (maybe string) Nothing


handleCommunication : Model -> String -> ( Model, Cmd Msg )
handleCommunication model msg =
    case decodeString (maybe (field "command" string)) msg of
        Ok (Just command) ->
            case command of
                "addNodes" ->
                    case decodeString (maybe (field "nodes" (list nodeDecoder))) msg of
                        Ok (Just inputNodes) ->
                            ( addNodes model inputNodes False, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                "removeNodes" ->
                    case decodeString (maybe (field "nodes" (list int))) msg of
                        Ok (Just nodes) ->
                            ( removeNodes model nodes, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                "addEdges" ->
                    case decodeString (maybe (field "edges" (list edgeDecoder))) msg of
                        Ok (Just inputEdges) ->
                            ( addEdges model inputEdges False, Cmd.none )

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
