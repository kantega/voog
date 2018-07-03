module Voog.Layouts.Sugiyama exposing (..)

import Sugiyama.CycleRemoval exposing (removeCycles)
import Sugiyama.CycleRemovalSimple exposing (removeCyclesSimple)
import Sugiyama.Sugiyama exposing (sugiyama, sugiyamaCustom)
import Sugiyama.Model
import Voog.Model exposing (..)
import Voog.Helpers exposing (reverseId)
import Voog.Place exposing (..)


sugiyamaLayout : List String -> Model -> Model
sugiyamaLayout layout ({ nodes, edges } as model) =
    let
        basicEdges =
            List.map (\e -> ( e.from, e.to )) edges

        basicNodes =
            List.map (\n -> n.id) nodes

        graph =
            let
                cycleRemoval =
                    if List.member "simpleCycles" layout then
                        removeCyclesSimple
                    else
                        removeCycles
            in
                sugiyamaCustom { nodes = basicNodes, edges = basicEdges } cycleRemoval

        sortedSugiyama =
            List.sortWith (\a b -> compare a.id b.id) graph.nodes

        sortedNodes =
            List.sortWith (\a b -> compare a.id b.id) nodes

        mergedNodes =
            List.map2
                (\{ x, y } n ->
                    let
                        position =
                            Just { x = toFloat <| Maybe.withDefault 0 x, y = toFloat <| Maybe.withDefault 0 y }
                    in
                        { n | position = position }
                )
                sortedSugiyama
                sortedNodes

        mergedEdges =
            List.map (mergeEdge graph) edges

        distance =
            4 * (Maybe.withDefault nodeRadius model.nodeDistance)

        placedNodes =
            List.map (placeNode distance) mergedNodes
    in
        { model | nodes = placedNodes, edges = mergedEdges }


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
                        { x = toFloat x, y = toFloat y }

                    _ ->
                        { x = -1, y = -1 }

            Nothing ->
                { x = -1, y = -1 }


placeNode : Float -> Node -> Node
placeNode distance ({ position } as node) =
    let
        p =
            case position of
                Just position ->
                    Just
                        { x = position.x * distance
                        , y = position.y * distance
                        }

                Nothing ->
                    position
    in
        { node
            | position = p
        }
