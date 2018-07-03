module Sugiyama.Placement exposing (..)

{-| Space nodes out visually aesthetic horizontally
This algorithm starts with all nodes on the left, with position x = - width/2 to x = width/2 of layer

Best cut: Given a position A it is the the position B that is to the right of or at A, where
moving all nodes right of, or at, B one step to the right results in the lowers horizontal edge offsets

For each layer with initial position 0:
Locate the best cut given position
Move all right of or at best cut to the right
Repeat for same layer and position + 1

Discussion:

1.  The best cut will always be a reduction in offset because the final cut where none is moved is a candidate.

2.  Lets compare two cuts A < B. They both move all nodes right of or at B one step. If moving all the nodes
    between them and the one at A will reduce the total offset then A is the better cut. If A is not a better
    cut, that proves the nodes between them should stay.

3.  The best cut is the leftmost cut A where moving everything to the right of or at A is a reduction in offset.
    By then repeating for each position from 0 to W we find an optimal solution.

4.  Although this is optimal for this layer it might not be for the graph as a whole.

-}

import Dict exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.Helpers exposing (..)


{-| Locate widest layer
Iterate up and down from widest layer then do the widest layer
Finally move whole graph to position
-}
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

        height =
            nodes
                |> List.map (\n -> Maybe.withDefault -1 n.y)
                |> List.maximum
                |> Maybe.withDefault 0

        layerPos =
            getLayerPos graph
    in
        graph
            |> setLayerPosition layerPos (wideLayer - 1) Up True True
            |> setLayerPosition layerPos (wideLayer + 1) Down True True
            |> setLayerPosition layerPos wideLayer Down False True
            |> makePositive


{-| Move the graph so the leftmost node has x=0
Find smallest x value and subtract that from each node
-}
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


{-| Move whole layer to the left to make space space for worst case scenario where each node is connected
to first node of the layer above
Then iteratively move a subsection of it to the right
-}
setLayerPosition : Dict Int (Maybe Int) -> Int -> Direction -> Bool -> Bool -> Graph -> Graph
setLayerPosition layerPos layer direction repeat resetX ({ nodes, edges } as graph) =
    let
        xPos =
            getXPos graph layer
    in
        if Dict.isEmpty xPos then
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
                    if resetX then
                        Dict.map (\id x -> Just (Maybe.withDefault -1 x - moveAmount)) xPos
                    else
                        xPos

                newXPos =
                    setSubLayerPosition movedXPos xPosOther layerEdges layer (-moveAmount)

                newNodes =
                    List.map
                        (\n -> { n | x = Maybe.withDefault n.x (Dict.get n.id newXPos) })
                        nodes
            in
                if repeat then
                    setLayerPosition layerPos nextLayer direction repeat resetX { graph | nodes = newNodes }
                else
                    { graph | nodes = newNodes }


{-| Find the best cut
If it is better then apply the right shift
Recursively call next on x+1
-}
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


{-| Find the optimal position to move all nodes right of it left one position
The optimal is the one with lowest total offset
-}
bestCut : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int -> ( Int, Int, Dict Int (Maybe Int) )
bestCut edges xPos xPosOther pos =
    let
        testXPos =
            Dict.map
                (\id x ->
                    if Maybe.withDefault -1 x >= pos then
                        Just ((Maybe.withDefault -1 x) + 1)
                    else
                        x
                )
                xPos

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


{-| Find offset of all edges in layer
-}
getTotalOffset : Edges -> Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Int
getTotalOffset edges xPos xPosOther =
    edges
        |> List.map (getOffset xPos xPosOther)
        |> List.sum


{-| Find horizontal (squared) offset between start and end of an edge
-}
getOffset : Dict Int (Maybe Int) -> Dict Int (Maybe Int) -> Edge -> Int
getOffset xPos xPosOther edge =
    let
        tryFindX =
            Dict.get edge.from xPos

        x =
            if tryFindX == Nothing then
                Dict.get edge.from xPosOther
            else
                tryFindX

        tryFindY =
            Dict.get edge.to xPos

        y =
            if tryFindY == Nothing then
                Dict.get edge.to xPosOther
            else
                tryFindY
    in
        case ( x, y ) of
            ( Just (Just x), Just (Just y) ) ->
                (x - y) ^ 2

            _ ->
                0
