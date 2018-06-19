module Action exposing (..)

import Dict
import Model exposing (..)
import Sugiyama.Sugiyama exposing (sugiyama)
import Sugiyama.Model
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
                                , info = info
                                , selected = False
                                , position = Nothing
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
                                                    { n | info = union }
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
            let
                ( newNodes, newEdges ) =
                    calculateDepth model.edges model.nodes
            in
                place
                    { model | nodes = newNodes, edges = newEdges }


removeNodes : Model -> List Int -> Model
removeNodes model nodes =
    let
        newNodes =
            List.filter (\n -> not (List.member n.id nodes)) model.nodes

        newEdges =
            List.filter (\e -> (not (List.member e.from nodes)) && (not (List.member e.to nodes))) model.edges

        ( new2Nodes, new2Edges ) =
            calculateDepth newEdges newNodes
    in
        place { model | nodes = new2Nodes, edges = new2Edges }


addEdges : Model -> Bool -> List ( ( Int, Int ), Dict.Dict String String ) -> Model
addEdges model update edges =
    case edges of
        ( ( from, to ), info ) :: rest ->
            if not update || not (List.any (\e -> e.id == ( from, to )) model.edges) then
                let
                    edge =
                        { id = ( from, to ), from = from, to = to, selected = False, info = info, position = Nothing }
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
            let
                ( newNodes, newEdges ) =
                    calculateDepth model.edges model.nodes
            in
                place
                    { model | nodes = newNodes, edges = newEdges }


removeEdges : Model -> List ( Int, Int ) -> Model
removeEdges model edges =
    let
        newEdges =
            List.filter (\e -> not (List.member e.id edges)) model.edges

        ( newNodes, new2Edges ) =
            calculateDepth newEdges model.nodes
    in
        place { model | edges = new2Edges, nodes = newNodes }


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


calculateDepth : Edges -> Nodes -> ( Nodes, Edges )
calculateDepth edges nodes =
    let
        basicEdges =
            List.map (\e -> ( e.from, e.to )) edges

        basicNodes =
            List.map (\n -> n.id) nodes

        graph =
            sugiyama { nodes = basicNodes, edges = basicEdges }

        sortedSugiyama =
            List.sortWith (\a b -> compare a.id b.id) graph.nodes

        sortedNodes =
            List.sortWith (\a b -> compare a.id b.id) nodes

        mergedNodes =
            List.map2
                (\{ x, y } n ->
                    let
                        position =
                            Just { x = Maybe.withDefault 0 x, y = Maybe.withDefault 0 y }
                    in
                        { n | position = position }
                )
                sortedSugiyama
                sortedNodes

        mergedEdges =
            List.map (mergeEdge graph) edges
    in
        ( mergedNodes, mergedEdges )


mergeEdge : Sugiyama.Model.Graph -> Edge -> Edge
mergeEdge { nodes, edges } edge =
    let
        parts =
            edges
                |> List.filter (\e -> e.id == edge.id)
                |> List.filterMap
                    (\e ->
                        case e.num of
                            Just num ->
                                Just { e | num = num }

                            _ ->
                                Nothing
                    )
                |> List.sortWith (\a b -> compare a.num b.num)
    in
        if List.length parts < 2 then
            edge
        else
            let
                endNodeIds =
                    List.map (\e -> e.to) parts

                nodeIds =
                    parts
                        |> List.map (\e -> e.from)
                        |> List.head
                        |> List.singleton
                        |> List.filterMap identity
                        |> (\a -> List.append a endNodeIds)

                points =
                    List.map (getNodePosition nodes) nodeIds

                reversed =
                    parts
                        |> List.map (\n -> {reversed = n.reversed})
                        |> List.head
                        |> Maybe.withDefault {reversed = False}
                        |> (\e -> e.reversed)

                correctPoints =
                    if reversed then
                        List.reverse points
                    else
                        points
            in
                { edge | position = Just (Multi correctPoints) }


getNodePosition : Sugiyama.Model.Nodes -> Int -> Point
getNodePosition nodes id =
    let
        node =
            nodes
                |> List.filter (\n -> n.id == id)
                |> List.head
    in
        case node of
            Just node ->
                case ( node.x, node.y ) of
                    ( Just x, Just y ) ->
                        { x = x, y = y }

                    _ ->
                        { x = -1, y = -1 }

            Nothing ->
                { x = -1, y = -1 }
