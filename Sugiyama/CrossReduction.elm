module Sugiyama.CrossReduction exposing (..)

import Dict exposing (..)
import Sugiyama.Model exposing (..)


reduceCrossing : Graph -> Graph
reduceCrossing graph =
    let
        layerDict =
            getLayerDict graph
    in
        reduceCrossingAtLayer graph layerDict 0


reduceCrossingAtLayer : Graph -> Dict Int (Maybe Int) -> Int -> Graph
reduceCrossingAtLayer ({ edges } as graph) layerDict layer =
    let
        layerEdges =
            List.filter (\e -> Dict.get e.from layerDict == Just (Just (layer))) edges
    in
        if List.length layerEdges == 0 then
            graph
        else
            let
                newGraph =
                    reduceCrossingAtLayer graph layerDict (layer + 1)

                xDictBelow =
                    getXDict newGraph (layer + 1)

                xDict =
                    getXDict newGraph layer

                maxX =
                    newGraph.nodes
                        |> List.filter (\n -> n.y == Just layer)
                        |> List.filterMap (\n -> n.x)
                        |> List.maximum
                        |> Maybe.withDefault 0

                newXDict =
                    tryFlipLayer layerEdges xDict xDictBelow 0 maxX 10

                newNodes =
                    List.map
                        (\n -> { n | x = Maybe.withDefault n.x (Dict.get n.id newXDict) })
                        newGraph.nodes
            in
                { graph | nodes = newNodes }


tryFlipLayer : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int -> Int -> Int -> Dict Int (Maybe Int)
tryFlipLayer edges xDict xDictBelow pos maxX iteration =
    if iteration == 0 then
        xDict
    else if pos < maxX then
        let
            newXDict =
                tryFlip edges xDict xDictBelow pos (pos + 1)
        in
            tryFlipLayer edges newXDict xDictBelow (pos + 1) maxX iteration
    else
        tryFlipLayer edges xDict xDictBelow 0 maxX (iteration - 1)


tryFlip : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int -> Int -> Dict Int (Maybe Int)
tryFlip edges xDict xDictBelow x1 x2 =
    let
        newXDict =
            xDict
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
        if crossings edges xDict xDictBelow > crossings edges newXDict xDictBelow then
            newXDict
        else
            xDict


crossings : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int
crossings edges xDict xDictBelow =
    edges
        |> List.map (edgeCrossings xDict xDictBelow edges)
        |> List.sum


edgeCrossings : Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Edges -> Edge -> Int
edgeCrossings xDict xDictBelow edges edge =
    edges
        |> List.filter (edgeCrossing xDict xDictBelow edge)
        |> List.length


edgeCrossing : Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Edge -> Edge -> Bool
edgeCrossing xDict xDictBelow a b =
    let
        a1 =
            Dict.get a.from xDict

        a2 =
            Dict.get a.to xDictBelow

        b1 =
            Dict.get b.from xDict

        b2 =
            Dict.get b.to xDictBelow
    in
        case ( a1, a2, b1, b2 ) of
            ( Just (Just a1), Just (Just a2), Just (Just b1), Just (Just b2) ) ->
                (a1 < b1 && a2 > b2) || (a1 > b1 && a2 < b2)

            _ ->
                False


getXDict : Graph -> Int -> Dict Int (Maybe Int)
getXDict { nodes } layer =
    nodes
        |> List.filter (\n -> n.y == Just layer)
        |> List.map (\n -> ( n.id, n.x ))
        |> Dict.fromList


getLayerDict : Graph -> Dict Int (Maybe Int)
getLayerDict { nodes } =
    nodes
        |> List.map (\n -> ( n.id, n.y ))
        |> Dict.fromList
