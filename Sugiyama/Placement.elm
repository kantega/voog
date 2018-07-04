module Sugiyama.Placement exposing (..)

{-| Space nodes out visually aesthetic horizontally
This algorithm starts with all nodes on the left, with position x = -width/2 to x = width/2 of layer

For each layer with initial position A = -width/2:

1.  Evaluate three options inclusive move, exclusive move or no move
    Exclusive: move all nodes to the right of position A one step right
    Inclusive: move all nodes to the right of A and the one at position A
    No move: keep all positions
2.  If inclusive is the best then do the inclusive move
3.  Move to next position

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
            |> setLayerPosition layerPos (wideLayer - 1) Up True
            |> setLayerPosition layerPos (wideLayer + 1) Down True
            |> setLayerPosition layerPos wideLayer Down False
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
setLayerPosition : IdPos -> Int -> Direction -> Bool -> Graph -> Graph
setLayerPosition layerPos layer direction repeat ({ nodes, edges } as graph) =
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

                xPosNext =
                    getXPos graph nextLayer

                layerEdges =
                    List.filter (\e -> Dict.get e.from layerPos == Just (Just (edgeLayer))) edges

                moveAmount =
                    round (toFloat (Dict.size xPos) / 2) + 1

                movedXPos =
                    Dict.map (\id x -> Just (Maybe.withDefault -1 x - moveAmount)) xPos

                -- Dont move
                newXPos =
                    setSubLayerPosition xPos xPosOther xPosNext layerEdges layer 0

                newNodes =
                    List.map
                        (\n -> { n | x = Maybe.withDefault n.x (Dict.get n.id newXPos) })
                        nodes
            in
                if repeat then
                    setLayerPosition layerPos nextLayer direction repeat { graph | nodes = newNodes }
                else
                    { graph | nodes = newNodes }


{-| By calculating offset at the beginning and passing it along in every operation we avoid doing the same work twice
-}
setSubLayerPosition : IdPos -> IdPos -> IdPos -> Edges -> Int -> Int -> IdPos
setSubLayerPosition xPos xPosOther xPosNext edges layer pos =
    let
        offset =
            getTotalOffset edges xPos xPosOther
    in
        setSubLayerPositionInner xPos xPosOther xPosNext offset edges layer pos


{-| Select bet option; inclusive move, exclusive move or no move
Recursively call next on position
-}
setSubLayerPositionInner : IdPos -> IdPos -> IdPos -> Int -> Edges -> Int -> Int -> IdPos
setSubLayerPositionInner xPos xPosOther xPosNext offset edges layer pos =
    let
        xPosInclusive =
            Dict.map
                (\id x ->
                    if (Maybe.withDefault -1 x) >= pos then
                        Just ((Maybe.withDefault -1 x) + 1)
                    else
                        x
                )
                xPos

        xPosExclusive =
            Dict.map
                (\id x ->
                    if (Maybe.withDefault -1 x) > pos then
                        Just ((Maybe.withDefault -1 x) + 1)
                    else
                        x
                )
                xPos

        xValues =
            xPos
                |> Dict.toList
                |> List.map (\( id, x ) -> Maybe.withDefault -1 x)

        maxX =
            xValues
                |> List.maximum
                |> Maybe.withDefault -1

        minX =
            xValues
                |> List.minimum
                |> Maybe.withDefault -1

        offsetInclusive =
            getTotalOffset edges xPosInclusive xPosOther

        offsetExclusive =
            getTotalOffset edges xPosExclusive xPosOther
    in
        if pos > maxX || pos < minX then
            xPos
        else if offset >= offsetExclusive || offset >= offsetInclusive then
            if offsetInclusive < offsetExclusive then
                setSubLayerPositionInner xPosInclusive xPosOther xPosNext offsetInclusive edges layer (pos + 1)
            else if offsetInclusive == offsetExclusive then
                let
                    offsetInclusiveOther =
                        getTotalOffset edges xPosInclusive xPosNext

                    offsetExclusiveOther =
                        getTotalOffset edges xPosExclusive xPosNext
                in
                    if offsetInclusiveOther < offsetExclusiveOther then
                        setSubLayerPositionInner xPosInclusive xPosOther xPosNext offsetInclusive edges layer (pos + 1)
                    else
                        setSubLayerPositionInner xPos xPosOther xPosNext offset edges layer (pos + 1)
            else
                setSubLayerPositionInner xPos xPosOther xPosNext offset edges layer (pos + 1)
        else
            setSubLayerPositionInner xPos xPosOther xPosNext offset edges layer (pos + 1)


{-| Find offset of all edges in layer
-}
getTotalOffset : Edges -> IdPos -> IdPos -> Int
getTotalOffset edges xPos xPosOther =
    edges
        |> List.map (getOffset xPos xPosOther)
        |> List.sum


{-| Find horizontal (squared) offset between start and end of an edge
-}
getOffset : IdPos -> IdPos -> Edge -> Int
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
