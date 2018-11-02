module Sugiyama.Placement exposing (..)

{-| Space nodes out visually aesthetic horizontally
This algorithm starts by centering the nodes in each layer along the middle of the widest layer

For each layer, starting on the left or rightmost node:

1.  Evaluate three options inclusive move, exclusive move or no move
    Exclusive: move all nodes to the right/left of position A one step right/left
    Inclusive: move all nodes to the right/left of A and the one at position A
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
        maxWidth =
            nodes
                |> List.map .x
                |> List.filterMap identity
                |> List.maximum
                |> Maybe.withDefault 0

        wideLayer =
            nodes
                |> List.filter (\n -> n.x == Just maxWidth)
                |> List.map .y
                |> List.filterMap identity
                |> List.head
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
            |> setLayerPosition layerPos (wideLayer - 1) maxWidth Up False
            |> setLayerPosition layerPos (wideLayer + 1) maxWidth Down False
            |> setLayerPosition layerPos wideLayer maxWidth Up True
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


{-| Move whole layer to center of widest layer
Then iteratively move a subsection of it to the left then repeat to the right
-}
setLayerPosition : IdPos -> Int -> Int -> Direction -> Bool -> Graph -> Graph
setLayerPosition layerPos layer maxWidth direction isMiddle ({ nodes, edges } as graph) =
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

                layerMaxWidth =
                    nodes
                        |> List.filter (\n -> n.y == Just layer)
                        |> List.map .x
                        |> List.filterMap identity
                        |> List.maximum
                        |> Maybe.withDefault 0

                newXPos =
                    xPos
                        |> Dict.map (\id x -> x + floor (toFloat (maxWidth - layerMaxWidth) / 2))
                        |> setSubLayerPositionRight xPosOther xPosOpposite isMiddle layerEdges layer
                        |> setSubLayerPositionLeft xPosOther xPosOpposite isMiddle layerEdges layer

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
                    setLayerPosition layerPos nextLayer maxWidth direction isMiddle { graph | nodes = newNodes }
                else
                    { graph | nodes = newNodes }


{-| By calculating offset at the beginning and passing it along in every operation we avoid doing the same work twice
-}
setSubLayerPositionRight : IdPos -> IdPos -> Bool -> Edges -> Int -> IdPos -> IdPos
setSubLayerPositionRight xPosOther xPosOpposite isMiddle edges layer xPos =
    let
        minX =
            xPos
                |> Dict.toList
                |> List.map (\( id, x ) -> x)
                |> List.minimum
                |> Maybe.withDefault -1
    in
        setSubLayerPositionInner xPos xPosOther xPosOpposite isMiddle edges layer minX 1


setSubLayerPositionLeft : IdPos -> IdPos -> Bool -> Edges -> Int -> IdPos -> IdPos
setSubLayerPositionLeft xPosOther xPosOpposite isMiddle edges layer xPos =
    let
        maxX =
            xPos
                |> Dict.toList
                |> List.map (\( id, x ) -> x)
                |> List.maximum
                |> Maybe.withDefault -1
    in
        setSubLayerPositionInner xPos xPosOther xPosOpposite isMiddle edges layer maxX -1


{-| Select bet option; inclusive move, exclusive move or no move
Recursively call next on position
-}
setSubLayerPositionInner : IdPos -> IdPos -> IdPos -> Bool -> Edges -> Int -> Int -> Int -> IdPos
setSubLayerPositionInner xPos xPosOther xPosOpposite isMiddle edges layer pos dir =
    let
        getNewXPos condition =
            xPos
                |> Dict.map
                    (\id x ->
                        if condition x pos then
                            x + dir
                        else
                            x
                    )

        getNewEdges default condition =
            List.filter
                (\e ->
                    (condition (Maybe.withDefault default (Dict.get e.from xPos)) pos)
                        || (condition (Maybe.withDefault default (Dict.get e.to xPos)) pos)
                )
                edges

        xPosInclusive =
            if dir > 0 then
                getNewXPos (>=)
            else
                getNewXPos (<=)

        xPosExclusive =
            if dir > 0 then
                getNewXPos (>)
            else
                getNewXPos (<)

        newEdges =
            if dir > 0 then
                getNewEdges -10000 (>=)
            else
                getNewEdges 10000 (<=)

        offset =
            getTotalOffset newEdges xPos xPosOther
                + (if isMiddle then
                    getTotalOffset newEdges xPos xPosOpposite
                   else
                    0
                  )

        offsetInclusive =
            getTotalOffset newEdges xPosInclusive xPosOther
                + (if isMiddle then
                    getTotalOffset newEdges xPosInclusive xPosOpposite
                   else
                    0
                  )

        offsetExclusive =
            getTotalOffset newEdges xPosExclusive xPosOther
                + (if isMiddle then
                    getTotalOffset newEdges xPosExclusive xPosOpposite
                   else
                    0
                  )
    in
        if List.isEmpty newEdges then
            xPos
        else if offset >= offsetExclusive && offsetInclusive < offsetExclusive then
            setSubLayerPositionInner xPosInclusive xPosOther xPosOpposite isMiddle newEdges layer (pos + dir) dir
        else
            setSubLayerPositionInner xPos xPosOther xPosOpposite isMiddle newEdges layer (pos + dir) dir


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

{-| Flip x and y
-}
flipAxis : Graph -> Graph
flipAxis ({ nodes } as graph) =
    let
        newNodes =
            List.map
                (\n -> { n | x = n.y, y = n.x })
                nodes
    in
        { graph | nodes = newNodes }
