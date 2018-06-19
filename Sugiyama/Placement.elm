module Sugiyama.Placement exposing (..)

import Set
import Dict exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


setPosition : Graph -> Graph
setPosition ({ nodes } as graph) =
    let
        wideLayer =
            nodes
                |> List.sortWith (\a b -> compare (Maybe.withDefault -1 a.x) (Maybe.withDefault -1 b.x))
                |> List.map (\n -> n.y)
                |> List.reverse
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault 0

        layerDict =
            getLayerDict graph
    in
        graph
            |> setLayerPosition layerDict (wideLayer - 1) Up
            |> setLayerPosition layerDict (wideLayer + 1) Down


setLayerPosition : Dict Int (Maybe Int) -> Int -> Direction -> Graph -> Graph
setLayerPosition layerDict layer direction ({ nodes, edges } as graph) =
    let
        xDict =
            getXDict graph layer
    in
        if Dict.size xDict == 0 then
            graph
        else
            let
                ( edgeLayer, otherLayer, nextLayer ) =
                    case direction of
                        Down ->
                            ( layer - 1, layer - 1, layer + 1 )

                        Up ->
                            ( layer, layer + 1, layer - 1 )

                xDictOther =
                    getXDict graph otherLayer

                layerEdges =
                    List.filter (\e -> Dict.get e.from layerDict == Just (Just (edgeLayer))) edges

                newXDict =
                    setSubLayerPosition xDict xDictOther layerEdges layer 0

                newNodes =
                    List.map
                        (\n -> { n | x = Maybe.withDefault n.x (Dict.get n.id newXDict) })
                        nodes
            in
                setLayerPosition layerDict nextLayer direction { graph | nodes = newNodes }


setSubLayerPosition : Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Edges -> Int -> Int -> Dict Int (Maybe Int)
setSubLayerPosition xDict xDictOther edges layer pos =
    let
        rightIdsA =
            xDict
                |> Dict.toList
                |> List.filter (\( id, x ) -> Maybe.withDefault -1 x >= pos)
                |> List.map (\( id, x ) -> id)
        rightIdsB =
            xDict
                |> Dict.toList
                |> List.filter (\( id, x ) -> Maybe.withDefault -1 x >= pos + 1)
                |> List.map (\( id, x ) -> id)
    in
        if List.length rightIdsA == 0 then
            xDict
        else
            let
                testXDictA =
                    xDict
                        |> Dict.toList
                        |> List.map
                            (\( id, x ) ->
                                if List.member id rightIdsA then
                                    ( id, Just ((Maybe.withDefault -1 x) + 1) )
                                else
                                    ( id, x )
                            )
                        |> Dict.fromList

                testXDictB =
                    xDict
                        |> Dict.toList
                        |> List.map
                            (\( id, x ) ->
                                if List.member id rightIdsB then
                                    ( id, Just ((Maybe.withDefault -1 x) + 1) )
                                else
                                    ( id, x )
                            )
                        |> Dict.fromList

                newXDict =
                    if totalOffset edges xDict xDictOther > totalOffset edges testXDictA xDictOther then
                        if totalOffset edges testXDictA xDictOther > totalOffset edges testXDictB xDictOther then
                            testXDictB
                        else
                            testXDictA
                    else
                        if totalOffset edges xDict xDictOther > totalOffset edges testXDictB xDictOther then
                            testXDictB
                        else
                            xDict
            in
                setSubLayerPosition newXDict xDictOther edges layer (pos + 1)


totalOffset : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int
totalOffset edges xDict xDictOther =
    let
        union =
            Dict.union xDict xDictOther
    in
        edges
            |> List.map (offset union)
            |> List.sum


offset : Dict Int (Maybe Int) -> Edge -> Int
offset xDict edge =
    let
        x =
            Dict.get edge.from xDict

        y =
            Dict.get edge.to xDict
    in
        case ( x, y ) of
            ( Just (Just x), Just (Just y) ) ->
                abs (x - y)

            _ ->
                0
