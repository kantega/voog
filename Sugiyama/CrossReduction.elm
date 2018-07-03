module Sugiyama.CrossReduction exposing (..)

{- | Reduce crossings in each layer
   The data structure used in this algorithm is Dict.Dict ( Int, Int, Int ) (Dict.Dict Int Int),
   where the key is (x, y, direction) and the value is a dictionary with key x and value id

   With this approach it is possible to search for all edges connected to a point (x, y) either
   going up to the layer above or down to the layer below, specified by the direction. The returned
   value consists of the ids of edges connected to the point and their opposite x position in the
   layer above or below.
-}

import Dict exposing (..)
import Sugiyama.Model exposing (..)


{-| Create new data structure
Reduce crossings
Extract new x position of each node
Apply new positions to graph
-}
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


{-| Alternate between up and down reduction until no further improvements
-}
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


{-| Run one iteration of either up or down cross reduction
-}
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


{-| Iterate down or upwards reducing crossings in each layer
-}
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
            reduceCrossingAtLayer direction (layer + dirInt direction) (tryFlipAllInLayer crossEdges layer direction)
        else
            crossEdges


{-| Repeatedly try to flip all pairs in layer until no further improvements
-}
tryFlipAllInLayer : CrossEdges -> Int -> Direction -> CrossEdges
tryFlipAllInLayer crossEdges y direction =
    let
        newCrossEdges =
            tryFlip crossEdges 0 y direction
    in
        if newCrossEdges == crossEdges then
            crossEdges
        else
            tryFlipAllInLayer newCrossEdges y direction


{-| Try to flip a single pair of nodes at x and x+1 in layer y
Recursively call x+1 and x+2 to iterate over whole length of layer
-}
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
                            |> reverseUpdate
                                (getUpdates keyDownB -1 downFromB []
                                    |> getUpdates keyDownA 1 downFromA
                                    |> getUpdates keyUpB -1 upFromB
                                    |> getUpdates keyUpA 1 upFromA
                                )
                        )
                        (x + 1)
                        y
                        direction
                else
                    tryFlip crossEdges (x + 1) y direction
        else
            crossEdges


{-| Collect all the required reverse-updates required when swapping to points
-}
getUpdates : ( Int, Int, Int ) -> Int -> Dict Int Int -> List ( ( Int, Int, Int ), Int, Int, Int ) -> List ( ( Int, Int, Int ), Int, Int, Int )
getUpdates ( x, y, dir ) move updateDict otherUpdates =
    List.append
        otherUpdates
        (updateDict
            |> Dict.toList
            |> List.map
                (\( x2, id ) -> ( ( x2, y + dir, -1 * dir ), x, (x + move), id ))
        )


{-| Update all the points this point is pointing at
First remove all old values then insert all the new in order to avoid collisions
-}
reverseUpdate : List ( ( Int, Int, Int ), Int, Int, Int ) -> CrossEdges -> CrossEdges
reverseUpdate updates crossEdges =
    crossEdges
        |> foldUpdate updates True
        |> foldUpdate updates False


foldUpdate : List ( ( Int, Int, Int ), Int, Int, Int ) -> Bool -> CrossEdges -> CrossEdges
foldUpdate updates remove crossEdges =
    case updates of
        ( k, x, x2, id ) :: rest ->
            let
                current =
                    crossEdges
                        |> Dict.get k
                        |> Maybe.withDefault Dict.empty

                new =
                    if remove then
                        Dict.remove x current
                    else
                        Dict.insert x2 id current
            in
                foldUpdate rest remove (Dict.insert k new crossEdges)

        _ ->
            crossEdges


{-| Convert the graph to desired data structure
-}
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


{-| Iterate over all edges and insert them into CrossEdges
-}
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


{-| Elm does not support Bool as key in dictionary, simply convert to Int for comparability
-}
dirInt : Direction -> Int
dirInt direction =
    case direction of
        Down ->
            1

        Up ->
            -1
