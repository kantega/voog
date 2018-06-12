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


removeNodes : Model -> List Int -> Model
removeNodes model nodes =
    let
        newNodes =
            List.filter (\n -> not (List.member n.id nodes)) model.nodes

        newEdges =
            List.filter (\e -> (not (List.member e.from nodes)) && (not (List.member e.to nodes))) model.edges
    in
        { model | nodes = newNodes, edges = newEdges, depthNodes = calculateDepth newEdges newNodes }


addEdges : Model -> List ( Int, Int ) -> Model
addEdges model edges =
    case edges of
        ( from, to ) :: rest ->
            let
                edge =
                    { from = from, to = to }
            in
                if not (List.member edge model.edges) then
                    addEdges { model | edges = { from = from, to = to } :: model.edges } rest
                else
                    addEdges model rest

        _ ->
            { model | depthNodes = calculateDepth model.edges model.nodes }


removeEdges : Model -> List ( Int, Int ) -> Model
removeEdges model edges =
    let
        newEdges = List.filter (\e -> not (List.member (e.from, e.to) edges)) model.edges
    in
        { model | edges = newEdges, depthNodes = calculateDepth newEdges model.nodes }