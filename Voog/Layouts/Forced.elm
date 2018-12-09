module Voog.Layouts.Forced exposing (attraction, forceTick, moveEdges, moveNodes, placeEdge, repulsion)

import Dict exposing (Dict)
import Voog.Model exposing (..)
import Voog.Place exposing (placeSingleLineEdge)
import Voog.View exposing (defaultDistance, nodeRadius)


forceTick : Model -> Model
forceTick ({ nodes, edges } as model) =
    if model.force > 0 then
        let
            nodePositions =
                nodes
                    |> List.map (\n -> ( n.id, n.position ))
                    |> Dict.fromList

            newNodes =
                List.map (moveNodes model nodePositions) nodes

            newNodePositions =
                newNodes
                    |> List.map (\n -> ( n.id, n.position ))
                    |> Dict.fromList

            newEdges =
                moveEdges model

            movement =
                List.map2
                    (\( _, a ) ( _, b ) ->
                        case ( a, b ) of
                            ( Just aInner, Just bInner ) ->
                                abs (bInner.x - aInner.x) + abs (bInner.y - aInner.y)

                            _ ->
                                0
                    )
                    (Dict.toList newNodePositions)
                    (Dict.toList nodePositions)
                    |> List.sum

            force =
                if movement > toFloat (List.length nodes) then
                    model.force * model.forceDampFactor

                else
                    0
        in
        { model | nodes = newNodes, edges = newEdges, force = force }

    else
        model


moveNodes : Model -> Dict Int (Maybe Point) -> Node -> Node
moveNodes ({ nodes, edges } as model) nodePositions node =
    case node.position of
        Just pos ->
            let
                nodeDistance =
                    Maybe.withDefault defaultDistance model.nodeDistance

                connections =
                    List.filter (\e -> e.from == node.id || e.to == node.id) edges

                attractions =
                    List.map (attraction model.attraction nodeDistance nodePositions node) connections

                repulsions =
                    List.map (repulsion model.repulsion (nodeDistance ^ 2) nodePositions node) nodes

                forces =
                    List.append attractions repulsions

                dx =
                    forces
                        |> List.map Tuple.first
                        |> List.sum
                        |> (*) model.force

                dy =
                    forces
                        |> List.map Tuple.second
                        |> List.sum
                        |> (*) model.force

                newPos =
                    Just { x = pos.x + dx, y = pos.y + dy }
            in
            { node | viewNode = Nothing, position = newPos }

        _ ->
            node


moveEdges : Model -> Edges
moveEdges model =
    let
        nodes =
            model.nodes
                |> List.map (\n -> ( n.id, n ))
                |> Dict.fromList

        edges =
            model.edges
                |> List.map (\e -> ( e.id, e ))
                |> Dict.fromList
    in
    List.map (placeEdge nodes edges) model.edges


placeEdge : Dict Int Node -> Dict ( Int, Int ) Edge -> Edge -> Edge
placeEdge nodes edges edge =
    let
        ( position, labelPosition ) =
            placeSingleLineEdge nodes edges edge
    in
    { edge | position = position, labelPosition = labelPosition }


attraction : Float -> Float -> Dict Int (Maybe Point) -> Node -> Edge -> ( Float, Float )
attraction coefficient nodeDistance nodePositions node edge =
    let
        nodeBId =
            if edge.from /= node.id then
                edge.from

            else
                edge.to

        posA =
            Dict.get node.id nodePositions

        posB =
            Dict.get nodeBId nodePositions
    in
    case ( posA, posB ) of
        ( Just (Just a), Just (Just b) ) ->
            let
                angle =
                    atan2 (a.y - b.y) (a.x - b.x)

                distance =
                    sqrt <| (b.y - a.y) ^ 2 + (b.x - a.x) ^ 2

                force =
                    coefficient * (nodeDistance - distance)
            in
            ( force * cos angle, force * sin angle )

        _ ->
            ( 0, 0 )


repulsion : Float -> Float -> Dict Int (Maybe Point) -> Node -> Node -> ( Float, Float )
repulsion coefficient nodeDistanceSquared nodePositions nodeA nodeB =
    let
        posA =
            Dict.get nodeA.id nodePositions

        posB =
            Dict.get nodeB.id nodePositions
    in
    case ( posA, posB ) of
        ( Just (Just a), Just (Just b) ) ->
            let
                angle =
                    atan2 (a.y - b.y) (a.x - b.x)

                distanceSquared =
                    (b.y - a.y) ^ 2 + (b.x - a.x) ^ 2
            in
            if distanceSquared > 0 && distanceSquared < 3 * nodeDistanceSquared then
                let
                    force =
                        coefficient / distanceSquared
                in
                ( force * cos angle, force * sin angle )

            else
                ( 0, 0 )

        _ ->
            ( 0, 0 )
