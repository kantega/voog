module Voog.Action exposing (..)

import Dict
import Window
import Voog.Model exposing (..)
import Voog.Place exposing (..)
import Voog.Layouts.Manual exposing (manualLayout)
import Voog.Layouts.Sugiyama exposing (sugiyamaLayout)


updateWindow : Window.Size -> Model -> Model
updateWindow { width, height } model =
    let
        newPosition =
            case model.windowSize of
                Just ( oldWidth, oldHeight ) ->
                    let
                        dw =
                            toFloat (width - oldWidth) / 2

                        dh =
                            toFloat (height - oldHeight) / 2
                    in
                        { x = model.position.x + dw, y = model.position.y + dh }

                Nothing ->
                    model.position
    in
        { model | position = newPosition, windowSize = Just ( width, height ) }


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


setNodes : List InputNode -> Bool -> Bool -> Model -> Model
setNodes nodes recalculate center model =
    case nodes of
        node :: rest ->
            if not (List.any (\n -> n.id == node.id) model.nodes) then
                let
                    newNodes =
                        { selected = False
                        , position = Nothing
                        , id = node.id
                        , info = node.info
                        , classes = node.classes
                        , name = node.name
                        , shape = node.shape
                        , image = node.image
                        , size = node.size
                        , x = node.x
                        , y = node.y
                        }
                            :: model.nodes
                in
                    setNodes rest True center { model | nodes = newNodes }
            else
                let
                    ( oldNode, oldNodes ) =
                        List.partition (\n -> n.id == node.id) model.nodes

                    newNodes =
                        case List.head oldNode of
                            Just oldNode ->
                                { oldNode
                                    | info = updateInfo oldNode.info node.info
                                    , classes = node.classes
                                    , name = node.name
                                    , shape = node.shape
                                    , image = node.image
                                    , size = node.size
                                    , x = node.x
                                    , y = node.y
                                }
                                    :: oldNodes

                            _ ->
                                model.nodes
                in
                    setNodes rest recalculate center { model | nodes = newNodes }

        _ ->
            if recalculate then
                let
                    recalculated =
                        layout model

                    centered =
                        if center then
                            centerGraph recalculated
                        else
                            recalculated
                in
                    place centered
            else
                model


removeNodes : List Int -> Model -> Model
removeNodes nodes model =
    let
        newNodes =
            List.filter (\n -> not (List.member n.id nodes)) model.nodes

        newEdges =
            List.filter (\e -> (not (List.member e.from nodes)) && (not (List.member e.to nodes))) model.edges

        newModel =
            layout { model | nodes = newNodes, edges = newEdges }
    in
        place newModel


setEdges : List InputEdge -> Bool -> Bool -> Model -> Model
setEdges edges recalculate center model =
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
                        , classes = edge.classes
                        , label = edge.label
                        , width = edge.width
                        , speed = edge.speed
                        }
                            :: model.edges
                in
                    setEdges rest True center { model | edges = newEdges }
            else
                let
                    ( oldEdge, oldEdges ) =
                        List.partition (\e -> e.id == ( edge.from, edge.to )) model.edges

                    newEdges =
                        case List.head oldEdge of
                            Just oldEdge ->
                                { oldEdge
                                    | info = updateInfo oldEdge.info edge.info
                                    , classes = edge.classes
                                    , label = edge.label
                                    , width = edge.width
                                    , speed = edge.speed
                                }
                                    :: oldEdges

                            _ ->
                                model.edges
                in
                    setEdges rest recalculate center { model | edges = newEdges }

        _ ->
            if recalculate then
                let
                    recalculated =
                        layout model

                    centered =
                        if center then
                            centerGraph recalculated
                        else
                            recalculated
                in
                    place centered
            else
                model


setNodesWithEdges : List InputEdge -> Model -> Model
setNodesWithEdges edges model =
    let
        nodeIds =
            List.map .id model.nodes

        nodes =
            edges
                |> List.concatMap (\e -> [ e.from, e.to ])
                |> List.filter (\n -> not <| List.member n nodeIds)
                |> List.map
                    (\id ->
                        { id = id
                        , info = []
                        , classes = []
                        , name = Nothing
                        , shape = Nothing
                        , image = Nothing
                        , size = Nothing
                        , x = Nothing
                        , y = Nothing
                        }
                    )
    in
        setNodes nodes False (model.nodes == []) model


centerGraph : Model -> Model
centerGraph ({ nodes } as model) =
    case model.windowSize of
        Just ( windowWidth, windowHeight ) ->
            let
                distance =
                    Maybe.withDefault defaultDistance model.nodeDistance

                width =
                    nodes
                        |> List.map .position
                        |> List.filterMap identity
                        |> List.map .x
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> (+) 3

                height =
                    nodes
                        |> List.map .position
                        |> List.filterMap identity
                        |> List.map .y
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> (+) 3

                widthZoom =
                    toFloat windowWidth / (width * distance)

                heightZoom =
                    toFloat windowHeight / (height * distance)

                zoom =
                    min widthZoom heightZoom

                finalZoom =
                    if zoom > 3 then
                        3
                    else if zoom < 0.1 then
                        0.1
                    else
                        zoom

                extraWidth =
                    (toFloat windowWidth - (finalZoom * width * distance)) / 2

                extraHeight =
                    (toFloat windowHeight - (finalZoom * height * distance)) / 2
            in
                { model
                    | zoom = finalZoom
                    , position =
                        { x = distance * finalZoom + extraWidth
                        , y = distance * finalZoom + extraHeight
                        }
                }

        _ ->
            model


removeEdges : List ( Int, Int ) -> Model -> Model
removeEdges edges model =
    let
        newEdges =
            List.filter (\e -> not (List.member e.id edges)) model.edges

        newModel =
            layout { model | edges = newEdges }
    in
        place newModel


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


closeInfo : Model -> Model
closeInfo model =
    { model
        | edges = List.map (\e -> { e | selected = False }) model.edges
        , nodes = List.map (\n -> { n | selected = False }) model.nodes
    }


layout : Model -> Model
layout model =
    case String.split "." <| Maybe.withDefault "" model.layout of
        "sugiyama" :: rest ->
            sugiyamaLayout rest model

        "manual" :: rest ->
            manualLayout rest model

        _ ->
            sugiyamaLayout [] model
