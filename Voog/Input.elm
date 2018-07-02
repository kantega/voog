module Voog.Input exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Voog.Model exposing (..)
import Voog.Action exposing (..)


stringStringTuple : Decoder ( String, String )
stringStringTuple =
    map2 (,) (index 0 string) (index 1 string)


intIntTuple : Decoder ( Int, Int )
intIntTuple =
    map2 (,) (index 0 int) (index 1 int)


floatFloatTuple : Decoder ( Float, Float )
floatFloatTuple =
    map2 (,) (index 0 float) (index 1 float)


input : Decoder Input
input =
    decode Input
        |> optional "name" string ""
        |> optional "size" (maybe intIntTuple) Nothing
        |> optional "position" (maybe intIntTuple) Nothing
        |> optional "layout" (maybe string) Nothing
        |> optional "nodeDistance" (maybe float) Nothing
        |> optional "setNodes" (list node) []
        |> optional "setEdges" (list edge) []
        |> optional "removeNodes" (list int) []
        |> optional "removeEdges" (list (intIntTuple)) []


node : Decoder InputNode
node =
    decode InputNode
        |> required "id" int
        |> optional "info" (list stringStringTuple) []
        |> optional "classes" (list string) []
        |> optional "name" (maybe string) Nothing
        |> optional "shape" (maybe string) Nothing
        |> optional "image" (maybe string) Nothing
        |> optional "size" (maybe float) Nothing
        |> optional "x" (maybe float) Nothing
        |> optional "y" (maybe float) Nothing


edge : Decoder InputEdge
edge =
    decode InputEdge
        |> required "from" int
        |> required "to" int
        |> optional "info" (list stringStringTuple) []
        |> optional "classes" (list string) []
        |> optional "label" (maybe string) Nothing
        |> optional "width" (maybe float) Nothing
        |> optional "speed" (maybe float) Nothing


handleInput : Model -> String -> Model
handleInput model inputString =
    case decodeString input inputString of
        Ok input ->
            model
                |> (\m ->
                        { m
                            | name = input.name
                            , layout = input.layout
                            , nodeDistance = input.nodeDistance
                        }
                   )
                |> handleSize input.size
                |> handlePosition input.position
                |> removeNodes input.removeNodes
                |> removeEdges input.removeEdges
                |> setNodes input.setNodes False (model.nodes == [])
                |> setEdges input.setEdges False (model.edges == [])
                |> setNodesWithEdges input.setEdges

        _ ->
            model


handleSize : Maybe ( Int, Int ) -> Model -> Model
handleSize size model =
    case size of
        Just ( x, y ) ->
            updateWindow { width = x, height = y } model

        _ ->
            model


handlePosition : Maybe ( Int, Int ) -> Model -> Model
handlePosition position model =
    case position of
        Just ( x, y ) ->
            { model | elementPosition = ( toFloat x, toFloat y ) }

        _ ->
            model
