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
        |> optional "clear" bool False
        |> optional "size" (maybe intIntTuple) Nothing
        |> optional "position" (maybe intIntTuple) Nothing
        |> optional "layout" (maybe string) Nothing
        |> optional "nodeDistance" (maybe float) Nothing
        |> optional "attraction" (maybe float) Nothing
        |> optional "repulsion" (maybe float) Nothing
        |> optional "center" (maybe bool) Nothing
        |> optional "addMovement" (list movement) []
        |> optional "setNodes" (list node) []
        |> optional "setEdges" (list edge) []
        |> optional "removeNodes" (list int) []
        |> optional "removeEdges" (list (intIntTuple)) []


movement : Decoder InputMovement
movement =
    decode InputMovement
        |> required "from" int
        |> required "to" int
        |> required "duration" float
        |> required "icon" string
        |> optional "classes" (list string) []


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
                        if input.clear then
                            { m | nodes = [], edges = [] }
                        else
                            m
                   )
                |> (\m ->
                        { m
                            | name = input.name
                            , layout = input.layout
                            , nodeDistance = input.nodeDistance
                            , attraction = input.attraction
                            , repulsion = input.repulsion
                            , center = Maybe.withDefault False input.center
                        }
                   )
                |> handleSize input.size
                |> handlePosition input.position
                |> addMovement input.addMovement
                |> removeNodes input.removeNodes
                |> removeEdges input.removeEdges
                |> setNodes input.setNodes False
                |> setEdges input.setEdges False
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
