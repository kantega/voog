module Action exposing (..)

import Model exposing (..)
import Depth exposing (..)
import Place exposing (..)


addNodes : Model -> List ( Int, String ) -> Model
addNodes model nodes =
    case nodes of
        ( id, name ) :: rest ->
            if not (List.any (\n -> n.id == id) model.nodes) then
                addNodes { model | nodes = { id = id, name = name, selected = False, info = [] } :: model.nodes } rest
            else
                addNodes model rest

        _ ->
            place { model | depthNodes = calculateDepth model.edges model.nodes }


removeNodes : Model -> List Int -> Model
removeNodes model nodes =
    let
        newNodes =
            List.filter (\n -> not (List.member n.id nodes)) model.nodes

        newEdges =
            List.filter (\e -> (not (List.member e.from nodes)) && (not (List.member e.to nodes))) model.edges
    in
        place { model | nodes = newNodes, edges = newEdges, depthNodes = calculateDepth newEdges newNodes }


addEdges : Model -> List ( Int, Int ) -> Model
addEdges model edges =
    case edges of
        ( from, to ) :: rest ->
            let
                edge =
                    { id = ( from, to ), from = from, to = to, selected = False }
            in
                if not (List.any (\e -> e.id == ( from, to )) model.edges) then
                    addEdges { model | edges = edge :: model.edges } rest
                else
                    addEdges model rest

        _ ->
            place { model | depthNodes = calculateDepth model.edges model.nodes }


removeEdges : Model -> List ( Int, Int ) -> Model
removeEdges model edges =
    let
        newEdges =
            List.filter (\e -> not (List.member e.id edges)) model.edges
    in
        place { model | edges = newEdges, depthNodes = calculateDepth newEdges model.nodes }


toggleNode : Model -> Int -> Model
toggleNode model id =
    { model
        | nodes =
            List.map
                (\n ->
                    if n.id == id then
                        { n | selected = not n.selected }
                    else
                        n
                )
                model.nodes
    }


toggleEdge : Model -> ( Int, Int ) -> Model
toggleEdge model id =
    { model
        | edges =
            List.map
                (\e ->
                    if e.id == id then
                        { e | selected = not e.selected }
                    else
                        e
                )
                model.edges
    }
