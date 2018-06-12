module Action exposing (..)

import Model exposing (..)
import Depth exposing (..)

addNodes : Model -> List ( Int, String ) -> Model
addNodes model nodes =
    case nodes of
        ( id, name ) :: rest ->
            if not (List.any (\n -> n.id == id) model.nodes) then
                addNodes { model | nodes = { id = id, name = name } :: model.nodes } rest
            else
                addNodes model rest
        _ ->
            { model | depthNodes = calculateDepth model.edges model.nodes }


addEdges : Model -> List ( Int, Int ) -> Model
addEdges model edges =
    case edges of
        ( from, to ) :: rest ->
            addEdges { model | edges = { from = from, to = to } :: model.edges } rest

        _ ->
            { model | depthNodes = calculateDepth model.edges model.nodes }
