module Voog.Input exposing (edge, floatFloatTuple, handleInput, handlePosition, handleSize, input, intIntTuple, movement, node, stringStringTuple)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Voog.Action exposing (..)
import Voog.Model exposing (..)


stringStringTuple : Decoder ( String, String )
stringStringTuple =
    map2 (\a b -> ( a, b )) (index 0 string) (index 1 string)


intIntTuple : Decoder ( Int, Int )
intIntTuple =
    map2 (\a b -> ( a, b )) (index 0 int) (index 1 int)


floatFloatTuple : Decoder ( Float, Float )
floatFloatTuple =
    map2 (\a b -> ( a, b )) (index 0 float) (index 1 float)


input : Decoder Input
input =
    Decode.succeed Input
        |> optional "name" string ""
        |> optional "clear" bool False
        |> optional "size" (maybe intIntTuple) Nothing
        |> optional "position" (maybe intIntTuple) Nothing
        |> optional "layout" (maybe string) Nothing
        |> optional "nodeDistance" (maybe float) Nothing
        |> optional "attraction" float 0.1
        |> optional "repulsion" float 300000
        |> optional "forceDampFactor" float 0.99
        |> optional "center" (maybe bool) Nothing
        |> optional "addMovement" (list movement) []
        |> optional "setNodes" (list node) []
        |> optional "setEdges" (list edge) []
        |> optional "removeNodes" (list int) []
        |> optional "removeEdges" (list intIntTuple) []


movement : Decoder InputMovement
movement =
    Decode.succeed InputMovement
        |> required "from" int
        |> required "to" int
        |> required "duration" float
        |> required "icon" string
        |> optional "classes" (list string) []


node : Decoder InputNode
node =
    Decode.succeed InputNode
        |> required "id" int
        |> optional "info" (list stringStringTuple) []
        |> optional "classes" (list string) []
        |> optional "name" (maybe string) Nothing
        |> optional "href" (maybe string) Nothing
        |> optional "shape" (maybe string) Nothing
        |> optional "image" (maybe string) Nothing
        |> optional "width" (maybe float) Nothing
        |> optional "height" (maybe float) Nothing
        |> optional "x" (maybe float) Nothing
        |> optional "y" (maybe float) Nothing


edge : Decoder InputEdge
edge =
    Decode.succeed InputEdge
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
        Ok inputMsg ->
            model
                |> (\m ->
                        if inputMsg.clear then
                            { m | nodes = [], edges = [] }

                        else
                            m
                   )
                |> (\m ->
                        { m
                            | name = inputMsg.name
                            , layout = inputMsg.layout
                            , nodeDistance = inputMsg.nodeDistance
                            , attraction = inputMsg.attraction
                            , repulsion = inputMsg.repulsion
                            , forceDampFactor = inputMsg.forceDampFactor
                            , center = Maybe.withDefault False inputMsg.center
                        }
                   )
                |> handleSize inputMsg.size
                |> handlePosition inputMsg.position
                |> addMovement inputMsg.addMovement
                |> removeNodes inputMsg.removeNodes
                |> removeEdges inputMsg.removeEdges
                |> setNodes inputMsg.setNodes False
                |> setEdges inputMsg.setEdges False
                |> setNodesWithEdges inputMsg.setEdges

        _ ->
            { model | invalidInput = True }


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
