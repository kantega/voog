module Input exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Model exposing (..)
import Action exposing (..)


stringStringListDecoder : Decoder (List ( String, String ))
stringStringListDecoder =
    (list (map2 (,) (index 0 string) (index 1 string)))


inputDecoder : Decoder Input
inputDecoder =
    decode Input
        |> optional "addNodes" (list nodeDecoder) []
        |> optional "addEdges" (list edgeDecoder) []
        |> optional "removeNodes" (list int) []
        |> optional "removeEdges" (list (map2 (,) (index 0 int) (index 1 int))) []


nodeDecoder : Decoder InputNode
nodeDecoder =
    decode InputNode
        |> required "id" int
        |> optional "info" stringStringListDecoder []
        |> optional "classes" (list string) []
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
        |> optional "classes" (list string) []
        |> optional "label" (maybe string) Nothing
        |> optional "width" (maybe float) Nothing
        |> optional "color" (maybe string) Nothing
        |> optional "speed" (maybe float) Nothing
        |> optional "dashColor" (maybe string) Nothing


handleInput : Model -> String -> Model
handleInput model inputString =
    case decodeString inputDecoder inputString of
        Ok input ->
            model
                |> removeNodes input.removeNodes
                |> removeEdges input.removeEdges
                |> addNodes input.addNodes False (model.nodes == [])
                |> addEdges input.addEdges False (model.edges == [])

        _ ->
            model
