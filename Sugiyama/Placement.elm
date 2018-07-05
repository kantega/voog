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
            |> setLayerPosition layerPos (wideLayer - 1) Up False
            |> setLayerPosition layerPos (wideLayer + 1) Down False
            |> setLayerPosition layerPos wideLayer Up True
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
setLayerPosition layerPos layer direction isMiddle ({ nodes, edges } as graph) =
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

                xPosOpposite =
                    getXPos graph nextLayer

                layerEdges =
                    if not isMiddle then
                        List.filter (\e -> Dict.get e.from layerPos == Just edgeLayer) edges
                    else
                        List.filter
                            (\e ->
                                (Dict.get e.from layerPos == Just layer)
                                    || (Dict.get e.to layerPos == Just layer)
                            )
                            edges

                moveAmount =
                    round (toFloat (Dict.size xPos) / 2) + 1

                movedXPos =
                    Dict.map (\id x -> x - moveAmount) xPos

                newXPos =
                    setSubLayerPosition movedXPos xPosOther xPosOpposite isMiddle layerEdges layer (-moveAmount)

                newNodes =
                    List.map
                        (\n ->
                            { n
                                | x =
                                    case Dict.get n.id newXPos of
                                        Just x ->
                                            Just x

                                        Nothing ->
                                            n.x
                            }
                        )
                        nodes
            in
                if not isMiddle then
                    setLayerPosition layerPos nextLayer direction isMiddle { graph | nodes = newNodes }
                else
                    { graph | nodes = newNodes }


{-| By calculating offset at the beginning and passing it along in every operation we avoid doing the same work twice
-}
setSubLayerPosition : IdPos -> IdPos -> IdPos -> Bool -> Edges -> Int -> Int -> IdPos
setSubLayerPosition xPos xPosOther xPosOpposite isMiddle edges layer pos =
    let
        offset =
            getTotalOffset edges xPos xPosOther
                + (if isMiddle then
                    getTotalOffset edges xPos xPosOpposite
                   else
                    0
                  )
    in
        setSubLayerPositionInner xPos xPosOther xPosOpposite isMiddle offset edges layer pos


{-| Select bet option; inclusive move, exclusive move or no move
Recursively call next on position
-}
setSubLayerPositionInner : IdPos -> IdPos -> IdPos -> Bool -> Int -> Edges -> Int -> Int -> IdPos
setSubLayerPositionInner xPos xPosOther xPosOpposite isMiddle offset edges layer pos =
    let
        xPosInclusive =
            xPos
                |> Dict.map
                    (\id x ->
                        if x >= pos then
                            x + 1
                        else
                            x
                    )

        xPosExclusive =
            xPos
                |> Dict.map
                    (\id x ->
                        if x > pos then
                            x + 1
                        else
                            x
                    )

        newEdges =
            List.filter
                (\e ->
                    (Maybe.withDefault -100000 (Dict.get e.from xPos) >= pos)
                        || (Maybe.withDefault -100000 (Dict.get e.to xPos) >= pos)
                )
                edges

        offsetInclusive =
            getTotalOffset newEdges xPosInclusive xPosOther
                + (if isMiddle then
                    getTotalOffset edges xPosInclusive xPosOpposite
                   else
                    0
                  )

        offsetExclusive =
            getTotalOffset newEdges xPosExclusive xPosOther
                + (if isMiddle then
                    getTotalOffset edges xPosExclusive xPosOpposite
                   else
                    0
                  )
    in
        if List.isEmpty newEdges then
            xPos
        else if offset >= offsetExclusive && offsetInclusive < offsetExclusive then
            setSubLayerPositionInner xPosInclusive xPosOther xPosOpposite isMiddle offsetInclusive newEdges layer (pos + 1)
        else
            setSubLayerPositionInner xPos xPosOther xPosOpposite isMiddle offset newEdges layer (pos + 1)


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
            ( Just x, Just y ) ->
                (x - y) ^ 2

            _ ->
                0
