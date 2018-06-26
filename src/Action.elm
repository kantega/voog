module Action exposing (..)

import Dict
import Model exposing (..)
import Place exposing (..)
import Sugiyama.Sugiyama exposing (sugiyama)
import Sugiyama.Model


updateVal : Maybe a -> Maybe a -> Maybe a
updateVal old new =
    new


categoryColor =
    [ "#f44336"
    , "#2196f3"
    , "#4caf50"
    , "#ffeb3b"
    , "#607d8b"
    ]
        |> List.indexedMap (\i c -> ( i, c ))
        |> Dict.fromList


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


addNodes : List InputNode -> Bool -> Model -> Model
addNodes nodes recalculate model =
    case nodes of
        node :: rest ->
            if not (List.any (\n -> n.id == node.id) model.nodes) then
                let
                    newNodes =
                        { selected = False
                        , position = Nothing
                        , categoryColor = Nothing
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
                    addNodes rest True { model | nodes = newNodes }
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
                    addNodes rest recalculate { model | nodes = newNodes }

        _ ->
            let
                coloredNodes =
                    setNodeColors model.nodes
            in
                if recalculate then
                    let
                        ( newNodes, newEdges ) =
                            calculateDepth model.edges coloredNodes
                    in
                        place
                            { model | nodes = newNodes, edges = newEdges }
                else
                    { model | nodes = coloredNodes }


removeNodes : List Int -> Model -> Model
removeNodes nodes model =
    let
        newNodes =
            List.filter (\n -> not (List.member n.id nodes)) model.nodes

        newEdges =
            List.filter (\e -> (not (List.member e.from nodes)) && (not (List.member e.to nodes))) model.edges

        ( new2Nodes, new2Edges ) =
            calculateDepth newEdges newNodes
    in
        place { model | nodes = new2Nodes, edges = new2Edges }


addEdges : List InputEdge -> Bool -> Model -> Model
addEdges edges recalculate model =
    case edges of
        edge :: rest ->
            if not (List.any (\e -> e.id == ( edge.from, edge.to )) model.edges) then
                let
                    newEdges =
                        { id = ( edge.from, edge.to )
                        , selected = False
                        , position = Nothing
                        , labelPosition = Nothing
                        , dashOffset = 0
                        , from = edge.from
                        , to = edge.to
                        , info = edge.info
                        , label = edge.label
                        , width = edge.width
                        , color = edge.color
                        , speed = edge.speed
                        , dashColor = edge.dashColor
                        }
                            :: model.edges
                in
                    addEdges rest True { model | edges = newEdges }
            else
                let
                    ( oldEdge, oldEdges ) =
                        List.partition (\e -> e.id == ( edge.from, edge.to )) model.edges

                    newEdges =
                        case List.head oldEdge of
                            Just oldEdge ->
                                { oldEdge
                                    | info = updateInfo oldEdge.info edge.info
                                    , label = updateVal oldEdge.label edge.label
                                    , width = updateVal oldEdge.width edge.width
                                    , color = updateVal oldEdge.color edge.color
                                    , speed = updateVal oldEdge.speed edge.speed
                                    , dashColor = updateVal oldEdge.dashColor edge.dashColor
                                }
                                    :: oldEdges

                            _ ->
                                model.edges
                in
                    addEdges rest recalculate { model | edges = newEdges }

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


removeEdges : List ( Int, Int ) -> Model -> Model
removeEdges edges model =
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


setNodeColors : Nodes -> Nodes
setNodeColors nodes =
    let
        categories =
            nodes
                |> List.filterMap
                    (\n ->
                        case n.category of
                            Just category ->
                                Just ( n.id, category )

                            _ ->
                                Nothing
                    )
                |> List.sortWith (\a b -> compare (Tuple.second a) (Tuple.second b))
                |> (setColors "" -1)
                |> Dict.fromList
    in
        List.map
            (\n ->
                case Dict.get n.id categories of
                    Just colorId ->
                        { n | categoryColor = Dict.get colorId categoryColor }

                    Nothing ->
                        n
            )
            nodes


setColors : String -> Int -> List ( Int, String ) -> List ( Int, Int )
setColors prevCategory colorId categories =
    case categories of
        ( id, category ) :: rest ->
            if category == prevCategory then
                ( id, colorId ) :: (setColors category colorId rest)
            else
                ( id, colorId + 1 ) :: (setColors category (colorId + 1) rest)

        _ ->
            []


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


getParts : Edge -> ( Int, Int ) -> Sugiyama.Model.Edges -> List { from : Int, to : Int, id : ( Int, Int ), num : Int }
getParts edge id edges =
    edges
        |> List.filter (\e -> e.id == id)
        |> List.filterMap
            (\e ->
                case e.num of
                    Just num ->
                        Just { from = e.from, to = e.to, id = e.id, num = num }

                    _ ->
                        Nothing
            )
        |> List.sortWith (\a b -> compare a.num b.num)


mergeEdge : Sugiyama.Model.Graph -> Edge -> Edge
mergeEdge { nodes, edges } edge =
    let
        partsA =
            getParts edge edge.id edges

        partsB =
            getParts edge (reverseId edge.id) edges
                |> List.reverse
                |> List.map (\e -> { e | from = e.to, to = e.from })

        parts =
            List.append partsA partsB
    in
        if List.length parts < 2 then
            { edge | position = Nothing }
        else
            let
                endNodeIds =
                    List.map .to parts

                nodeIds =
                    parts
                        |> List.map .from
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
                        |> .reversed

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
