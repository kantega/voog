module Action exposing (..)

import Dict
import Model exposing (..)
import Depth exposing (..)
import Place exposing (..)


addNodes : Model -> Bool -> List ( String, Dict.Dict String String ) -> Model
addNodes model update nodes =
    case nodes of
        ( id, info ) :: rest ->
            case String.toInt id of
                Ok id ->
                    if not update || not (List.any (\n -> n.id == id) model.nodes) then
                        let
                            node =
                                { id = id
                                , name = Dict.get "name" info
                                , typ = Dict.get "type" info
                                , info = info
                                , selected = False
                                }
                        in
                            if not (List.any (\n -> n.id == id) model.nodes) then
                                addNodes { model | nodes = node :: model.nodes } update rest
                            else
                                addNodes model update rest
                    else
                        addNodes
                            { model
                                | nodes =
                                    List.map
                                        (\n ->
                                            if n.id == id then
                                                let
                                                    union =
                                                        Dict.union info n.info
                                                in
                                                    { n
                                                        | name = Dict.get "name" union
                                                        , typ = Dict.get "type" union
                                                        , info = union
                                                    }
                                            else
                                                n
                                        )
                                        model.nodes
                            }
                            update
                            rest

                _ ->
                    addNodes model update rest

        _ ->
            place
                { model
                    | depthNodes = calculateDepth model.edges model.nodes
                    , nodes = List.sortWith (\a b -> compare a.id b.id) model.nodes
                }


removeNodes : Model -> List Int -> Model
removeNodes model nodes =
    let
        newNodes =
            List.filter (\n -> not (List.member n.id nodes)) model.nodes

        newEdges =
            List.filter (\e -> (not (List.member e.from nodes)) && (not (List.member e.to nodes))) model.edges
    in
        place { model | nodes = newNodes, edges = newEdges, depthNodes = calculateDepth newEdges newNodes }


addEdges : Model -> Bool -> List ( ( Int, Int ), Dict.Dict String String ) -> Model
addEdges model update edges =
    case edges of
        ( ( from, to ), info ) :: rest ->
            if not update || not (List.any (\e -> e.id == ( from, to )) model.edges) then
                let
                    edge =
                        { id = ( from, to ), from = from, to = to, selected = False, info = info }
                in
                    if not (List.any (\e -> e.id == ( from, to )) model.edges) then
                        addEdges { model | edges = edge :: model.edges } update rest
                    else
                        addEdges model update rest
            else
                addEdges
                    { model
                        | edges =
                            List.map
                                (\e ->
                                    if e.id == ( from, to ) then
                                        { e | info = Dict.union info e.info }
                                    else
                                        e
                                )
                                model.edges
                    }
                    update
                    rest

        _ ->
            place
                { model
                    | depthNodes = calculateDepth model.edges model.nodes
                    , edges = List.sortWith (\a b -> compare a.id b.id) model.edges
                }


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
                        { n | selected = False }
                )
                model.nodes
        , edges = List.map (\e -> { e | selected = False }) model.edges
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
                        { e | selected = False }
                )
                model.edges
        , nodes = List.map (\n -> { n | selected = False }) model.nodes
    }
