module Sugiyama.CrossReduction exposing (..)

import Dict exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


reduceCrossing : Direction -> Graph -> Graph
reduceCrossing direction graph =
    let
        layerPos =
            getLayerPos graph
        startLayer =
            case direction of
                Down ->
                    1
                Up ->
                    0
    in
        reduceCrossingAtLayer graph direction layerPos startLayer


reduceCrossingAtLayer : Graph -> Direction -> Dict Int (Maybe Int) -> Int -> Graph
reduceCrossingAtLayer ({ edges } as graph) direction layerPos layer =
    let
        (edgeLayer, otherLayer) =
            case direction of
                Down ->
                    (layer - 1, layer - 1)
                Up ->
                    (layer, layer + 1)
        layerEdges =
            List.filter (\e -> Dict.get e.from layerPos == Just (Just (edgeLayer))) edges
    in
        if List.length layerEdges == 0 then
            graph
        else
            let
                newGraph =
                    case direction of
                        Down ->
                            graph
                        Up ->
                            reduceCrossingAtLayer graph direction layerPos (layer + 1)

                xPosOther =
                    getXPos newGraph otherLayer

                xPos =
                    getXPos newGraph layer

                maxX =
                    newGraph.nodes
                        |> List.filter (\n -> n.y == Just layer)
                        |> List.filterMap (\n -> n.x)
                        |> List.maximum
                        |> Maybe.withDefault 0

                newXPos =
                    tryFlipLayer layerEdges xPos xPosOther 0 maxX 10

                newNodes =
                    List.map
                        (\n -> { n | x = Maybe.withDefault n.x (Dict.get n.id newXPos) })
                        newGraph.nodes
            in
                case direction of
                    Down ->
                        reduceCrossingAtLayer { graph | nodes = newNodes } direction layerPos (layer + 1)
                    Up ->
                        { graph | nodes = newNodes }


tryFlipLayer : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int -> Int -> Int -> Dict Int (Maybe Int)
tryFlipLayer edges xPos xPosOther pos maxX iteration =
    if iteration == 0 then
        xPos
    else if pos < maxX then
        let
            newXPos =
                tryFlip edges xPos xPosOther pos (pos + 1)
        in
            tryFlipLayer edges newXPos xPosOther (pos + 1) maxX iteration
    else
        tryFlipLayer edges xPos xPosOther 0 maxX (iteration - 1)


tryFlip : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int -> Int -> Dict Int (Maybe Int)
tryFlip edges xPos xPosOther x1 x2 =
    let
        newXPos =
            xPos
                |> Dict.toList
                |> List.map
                    (\( id, x ) ->
                        if x == Just x1 then
                            ( id, Just x2 )
                        else if x == Just x2 then
                            ( id, Just x1 )
                        else
                            ( id, x )
                    )
                |> Dict.fromList
    in
        if crossings edges xPos xPosOther > crossings edges newXPos xPosOther then
            newXPos
        else
            xPos


crossings : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int
crossings edges xPos xPosOther =
    edges
        |> List.map (edgeCrossings xPos xPosOther edges)
        |> List.sum


edgeCrossings : Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Edges -> Edge -> Int
edgeCrossings xPos xPosOther edges edge =
    edges
        |> List.filter (edgeCrossing xPos xPosOther edge)
        |> List.length


edgeCrossing : Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Edge -> Edge -> Bool
edgeCrossing xPos xPosOther a b =
    let
        union =
            Dict.union xPos xPosOther

        a1 =
            Dict.get a.from union

        a2 =
            Dict.get a.to union

        b1 =
            Dict.get b.from union

        b2 =
            Dict.get b.to union
    in
        case ( a1, a2, b1, b2 ) of
            ( Just (Just a1), Just (Just a2), Just (Just b1), Just (Just b2) ) ->
                (a1 < b1 && a2 > b2) || (a1 > b1 && a2 < b2)

            _ ->
                False
