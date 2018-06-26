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
                |> List.map .y
                |> List.reverse
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault 0

        layerPos =
            getLayerPos graph
    in
        graph
            |> setLayerPosition layerPos (wideLayer - 1) Up
            |> setLayerPosition layerPos (wideLayer + 1) Down
            |> makePositive


makePositive : Graph -> Graph
makePositive ({ nodes } as graph) =
    let
        minVal =
            nodes
                |> List.sortWith (\a b -> compare (Maybe.withDefault -1 a.x) (Maybe.withDefault -1 b.x))
                |> List.map .x
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault 0

        newNodes =
            List.map
                (\n -> { n | x = Just (Maybe.withDefault -1 n.x - minVal) })
                nodes
    in
        { graph | nodes = newNodes }


setLayerPosition : Dict Int (Maybe Int) -> Int -> Direction -> Graph -> Graph
setLayerPosition layerPos layer direction ({ nodes, edges } as graph) =
    let
        xPos =
            getXPos graph layer
    in
        if Dict.size xPos == 0 then
            graph
        else
            let
                ( edgeLayer, otherLayer, nextLayer ) =
                    case direction of
                        Down ->
                            ( layer - 1, layer - 1, layer + 1 )

                        Up ->
                            ( layer, layer + 1, layer - 1 )

                xPosOther =
                    getXPos graph otherLayer

                layerEdges =
                    List.filter (\e -> Dict.get e.from layerPos == Just (Just (edgeLayer))) edges

                moveAmount =
                    round (toFloat (Dict.size xPos) / 2) + 1

                movedXPos =
                    xPos
                        |> Dict.toList
                        |> List.map (\( id, x ) -> ( id, Just (Maybe.withDefault -1 x - moveAmount) ))
                        |> Dict.fromList

                newXPos =
                    setSubLayerPosition movedXPos xPosOther layerEdges layer (-moveAmount)

                newNodes =
                    List.map
                        (\n -> { n | x = Maybe.withDefault n.x (Dict.get n.id newXPos) })
                        nodes
            in
                setLayerPosition layerPos nextLayer direction { graph | nodes = newNodes }


setSubLayerPosition : Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Edges -> Int -> Int -> Dict Int (Maybe Int)
setSubLayerPosition xPos xPosOther edges layer pos =
    let
        ( offset, newPos, newXPos ) =
            bestCut edges xPos xPosOther pos
    in
        if offset == getTotalOffset edges xPos xPosOther then
            xPos
        else
            setSubLayerPosition newXPos xPosOther edges layer (pos + 1)


bestCut : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int -> ( Int, Int, Dict Int (Maybe Int) )
bestCut edges xPos xPosOther pos =
    let
        testXPos =
            xPos
                |> Dict.toList
                |> List.map
                    (\( id, x ) ->
                        if Maybe.withDefault -1 x >= pos then
                            ( id, Just ((Maybe.withDefault -1 x) + 1) )
                        else
                            ( id, x )
                    )
                |> Dict.fromList

        offset =
            getTotalOffset edges testXPos xPosOther
    in
        if testXPos == xPos then
            ( offset, pos, testXPos )
        else
            let
                ( nextOffset, nextPos, nextXPos ) =
                    bestCut edges xPos xPosOther (pos + 1)
            in
                if offset > nextOffset then
                    ( nextOffset, nextPos, nextXPos )
                else
                    ( offset, pos, testXPos )


getTotalOffset : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int
getTotalOffset edges xPos xPosOther =
    let
        union =
            Dict.union xPos xPosOther
    in
        edges
            |> List.map (getOffset union)
            |> List.sum


getOffset : Dict Int (Maybe Int) -> Edge -> Int
getOffset xPos edge =
    let
        x =
            Dict.get edge.from xPos

        y =
            Dict.get edge.to xPos
    in
        case ( x, y ) of
            ( Just (Just x), Just (Just y) ) ->
                (x - y) ^ 2

            _ ->
                0
