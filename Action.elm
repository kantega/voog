module Action exposing (..)

import Dict
import Model exposing (..)
import Sugiyama.Sugiyama exposing (sugiyama)
import Sugiyama.Model
import Place exposing (..)


updateVal : Maybe a -> Maybe a -> Maybe a
updateVal old new =
    if new == Nothing then
        old
    else
        new


updateInfo : Info -> Info -> Info
updateInfo old new =
    let
        newItems =
            Dict.fromList new

        oldKeys =
            List.map Tuple.first old

        updated =
            List.map
                (\( k, v ) ->
                    case Dict.get k newItems of
                        Just newVal ->
                            ( k, newVal )

                        Nothing ->
                            ( k, v )
                )
                old

        appended =
            List.filter (\( k, v ) -> not (List.member k oldKeys)) new
    in
        List.append updated appended


addNodes : Model -> List InputNode -> Bool -> Model
addNodes model nodes recalculate =
    case nodes of
        node :: rest ->
            if not (List.any (\n -> n.id == node.id) model.nodes) then
                let
                    newNodes =
                        { selected = False
                        , position = Nothing
                        , id = node.id
                        , info = node.info
                        , name = node.name
                        , shape = node.shape
                        , image = node.image
                        , category = node.category
                        , color = node.color
                        , size = node.size
                        }
                            :: model.nodes
                in
                    addNodes { model | nodes = newNodes } rest True
            else
                let
                    ( oldNode, oldNodes ) =
                        List.partition (\n -> n.id == node.id) model.nodes

                    newNodes =
                        case List.head oldNode of
                            Just oldNode ->
                                { oldNode
                                    | info = updateInfo oldNode.info node.info
                                    , name = updateVal oldNode.name node.name
                                    , shape = updateVal oldNode.shape node.shape
                                    , image = updateVal oldNode.image node.image
                                    , category = updateVal oldNode.category node.category
                                    , color = updateVal oldNode.color node.color
                                    , size = updateVal oldNode.size node.size
                                }
                                    :: oldNodes

                            _ ->
                                model.nodes
                in
                    addNodes { model | nodes = newNodes } rest recalculate

        _ ->
            if recalculate then
                let
                    ( newNodes, newEdges ) =
                        calculateDepth model.edges model.nodes
                in
                    place
                        { model | nodes = newNodes, edges = newEdges }
            else
                model


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


addEdges : Model -> List InputEdge -> Bool -> Model
addEdges model edges recalculate =
    case edges of
        edge :: rest ->
            if not (List.any (\e -> e.id == ( edge.from, edge.to )) model.edges) then
                let
                    newEdges =
                        { id = ( edge.from, edge.to )
                        , selected = False
                        , position = Nothing
                        , from = edge.from
                        , to = edge.to
                        , info = edge.info
                        , label = edge.label
                        , width = edge.width
                        , color = edge.color
                        }
                            :: model.edges
                in
                    addEdges { model | edges = newEdges } rest True
            else
                let
                    ( oldEdge, oldEdges ) =
                        List.partition (\e -> e.id == ( edge.from, edge.to )) model.edges

                    newEdges =
                        case List.head oldEdge of
                            Just oldNode ->
                                { oldNode
                                    | info = updateInfo oldNode.info edge.info
                                    , label = updateVal oldNode.label edge.label
                                    , width = updateVal oldNode.width edge.width
                                    , color = updateVal oldNode.color edge.color
                                }
                                    :: oldEdges

                            _ ->
                                model.edges
                in
                    addEdges { model | edges = newEdges } rest recalculate

        _ ->
            if recalculate then
                let
                    ( newNodes, newEdges ) =
                        calculateDepth model.edges model.nodes
                in
                    place
                        { model | nodes = newNodes, edges = newEdges }
            else
                model


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
            { edge | position = Nothing }
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
                    edges
                        |> List.filter (\e -> e.id == edge.id)
                        |> List.map (\e -> { reversed = e.reversed })
                        |> List.head
                        |> Maybe.withDefault { reversed = False }
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
