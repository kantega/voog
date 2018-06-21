module Sugiyama.CrossReduction exposing (..)

import Dict exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


reduceCrossing : Graph -> Graph
reduceCrossing ({ nodes } as graph) =
    let
        ( oldEdges, crossEdges ) =
            getCrossEdges graph

        newCrossEdges =
            reduceCrossingsWithEdgesUpDown crossEdges

        newPos =
            newCrossEdges
                |> Dict.toList
                |> List.filter (\( ( x, y, d ), connectionDict ) -> d == dirInt Down)
                |> List.map
                    (\( ( x, y, d ), connectionDict ) ->
                        List.filterMap
                            (\( x2, id ) ->
                                case Dict.get id oldEdges of
                                    Just oldEdge ->
                                        Just [ ( oldEdge.from, x ), ( oldEdge.to, x2 ) ]

                                    _ ->
                                        Nothing
                            )
                            (Dict.toList connectionDict)
                    )
                |> List.concat
                |> List.concat
                |> Dict.fromList

        newNodes =
            List.map
                (\n ->
                    { n
                        | x =
                            newPos
                                |> Dict.get n.id
                                |> Maybe.withDefault (Maybe.withDefault -1 n.x)
                                |> Just
                    }
                )
                nodes
    in
        { graph | nodes = newNodes }


reduceCrossingsWithEdgesUpDown : CrossEdges -> CrossEdges
reduceCrossingsWithEdgesUpDown crossEdges =
    let
        newCrossEdges =
            crossEdges
                |> reduceCrossingsWithEdges Up
                |> reduceCrossingsWithEdges Down
    in
        if newCrossEdges == crossEdges then
            crossEdges
        else
            reduceCrossingsWithEdgesUpDown newCrossEdges


reduceCrossingsWithEdges : Direction -> CrossEdges -> CrossEdges
reduceCrossingsWithEdges direction crossEdges =
    let
        startLayer =
            case direction of
                Down ->
                    1

                Up ->
                    crossEdges
                        |> Dict.toList
                        |> List.map (\( ( _, y, _ ), _ ) -> y)
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> (+) -1
    in
        reduceCrossingAtLayer direction startLayer crossEdges


reduceCrossingAtLayer : Direction -> Int -> CrossEdges -> CrossEdges
reduceCrossingAtLayer direction layer crossEdges =
    let
        key =
            ( 0, layer, -(dirInt direction) )

        size =
            crossEdges
                |> Dict.get key
                |> Maybe.withDefault Dict.empty
                |> Dict.size
    in
        if size > 0 then
            reduceCrossingAtLayer direction (layer + dirInt direction) (tryFlipLayer crossEdges layer direction)
        else
            crossEdges


tryFlipLayer : CrossEdges -> Int -> Direction -> CrossEdges
tryFlipLayer crossEdges y direction =
    let
        newCrossEdges =
            tryFlip crossEdges 0 y direction
    in
        if newCrossEdges == crossEdges then
            crossEdges
        else
            tryFlipLayer newCrossEdges y direction


tryFlip : CrossEdges -> Int -> Int -> Direction -> CrossEdges
tryFlip crossEdges x y direction =
    let
        keyDownA =
            ( x, y, -1 * (dirInt direction) )

        keyDownB =
            ( x + 1, y, -1 * (dirInt direction) )

        keyUpA =
            ( x, y, (dirInt direction) )

        keyUpB =
            ( x + 1, y, (dirInt direction) )

        downFromA =
            crossEdges
                |> Dict.get keyDownA
                |> Maybe.withDefault Dict.empty

        downFromB =
            crossEdges
                |> Dict.get keyDownB
                |> Maybe.withDefault Dict.empty

        upFromA =
            crossEdges
                |> Dict.get keyUpA
                |> Maybe.withDefault Dict.empty

        upFromB =
            crossEdges
                |> Dict.get keyUpB
                |> Maybe.withDefault Dict.empty
    in
        if (Dict.size upFromA + Dict.size downFromA > 0 && Dict.size upFromB + Dict.size downFromB > 0) then
            let
                childrenA =
                    downFromA
                        |> Dict.toList
                        |> List.map (\( x, id ) -> x)

                childrenB =
                    downFromB
                        |> Dict.toList
                        |> List.map (\( x, id ) -> x)

                diff =
                    childrenB
                        |> List.map
                            (\xB ->
                                ((childrenA
                                    |> List.filter (\xA -> xB > xA)
                                    |> List.length
                                 )
                                    - (childrenA
                                        |> List.filter (\xA -> xB < xA)
                                        |> List.length
                                      )
                                )
                            )
                        |> List.sum
            in
                if diff < 0 then
                    tryFlip
                        (crossEdges
                            |> Dict.insert keyDownA downFromB
                            |> Dict.insert keyDownB downFromA
                            |> Dict.insert keyUpA upFromB
                            |> Dict.insert keyUpB upFromA
                            |> reverseUpdate keyDownB -1 downFromB
                            |> reverseUpdate keyDownA 1 downFromA
                            |> reverseUpdate keyUpB -1 upFromB
                            |> reverseUpdate keyUpA 1 upFromA
                        )
                        (x + 1)
                        y
                        direction
                else
                    tryFlip crossEdges (x + 1) y direction
        else
            crossEdges


reverseUpdate : ( Int, Int, Int ) -> Int -> Dict Int Int -> CrossEdges -> CrossEdges
reverseUpdate ( x, y, dir ) move updateDict crossEdges =
    let
        newDicts =
            updateDict
                |> Dict.toList
                |> List.map
                    (\( x2, id ) ->
                        ( ( x2, y + dir, -1 * dir )
                        , (Dict.get ( x2, y + dir, -1 * dir ) crossEdges
                            |> Maybe.withDefault Dict.empty
                            |> Dict.remove x
                            |> Dict.insert (x + move) id
                          )
                        )
                    )
    in
        foldUpdate crossEdges newDicts


foldUpdate : CrossEdges -> List ( ( Int, Int, Int ), Dict Int Int ) -> CrossEdges
foldUpdate crossEdges updates =
    case updates of
        ( k, v ) :: rest ->
            foldUpdate (Dict.insert k v crossEdges) rest

        _ ->
            crossEdges


getCrossEdges : Graph -> ( Dict Int Edge, CrossEdges )
getCrossEdges ({ nodes, edges } as graph) =
    let
        xPos =
            nodes
                |> List.map (\n -> ( n.id, n.x ))
                |> Dict.fromList

        yPos =
            nodes
                |> List.map (\n -> ( n.id, n.y ))
                |> Dict.fromList

        oldEdges =
            edges
                |> List.indexedMap (\i e -> ( i, e ))

        crossEdges =
            createCrossEdgeDict Dict.empty xPos yPos oldEdges
    in
        ( Dict.fromList oldEdges, crossEdges )


createCrossEdgeDict : CrossEdges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> List ( Int, Edge ) -> CrossEdges
createCrossEdgeDict crossEdges xPos yPos oldEdges =
    case oldEdges of
        ( id, e ) :: rest ->
            let
                x1 =
                    Dict.get e.from xPos

                x2 =
                    Dict.get e.to xPos

                y =
                    Dict.get e.from yPos
            in
                case ( x1, x2, y ) of
                    ( Just (Just x1), Just (Just x2), Just (Just y) ) ->
                        let
                            keyDown =
                                ( x1, y, dirInt Down )

                            keyUp =
                                ( x2, y + 1, dirInt Up )

                            currentDictDown =
                                Maybe.withDefault Dict.empty (Dict.get keyDown crossEdges)

                            currentDictUp =
                                Maybe.withDefault Dict.empty (Dict.get keyUp crossEdges)

                            updatedInnerDown =
                                Dict.insert x2 id currentDictDown

                            updatedInnerUp =
                                Dict.insert x1 id currentDictUp

                            newDict =
                                crossEdges
                                    |> Dict.insert keyDown updatedInnerDown
                                    |> Dict.insert keyUp updatedInnerUp
                        in
                            createCrossEdgeDict newDict xPos yPos rest

                    _ ->
                        createCrossEdgeDict crossEdges xPos yPos rest

        _ ->
            crossEdges


dirInt : Direction -> Int
dirInt direction =
    case direction of
        Down ->
            1

        Up ->
            -1
