module Place exposing (..)

import Model exposing (..)
import Depth exposing (..)


place : Model -> Model
place model =
    let
        placedNodes =
            placeNodes model.edges model.nodes model.depthNodes
                |> List.sortWith (\a b -> compare a.id b.id)
    in
        { model | placedNodes = placedNodes, placedEdges = placeEdges model.edges placedNodes }


placeNodes : Edges -> Nodes -> DepthNodes -> PlacedNodes
placeNodes edges nodes depthNodes =
    let
        sortedNodes =
            List.sortWith (\a b -> compare a.id b.id) nodes

        sortedDepthNodes =
            List.sortWith (\a b -> compare a.id b.id) depthNodes
    in
        List.map2
            (\n d ->
                { id = n.id
                , x = 0
                , y = d.depth * 120
                }
            )
            sortedNodes
            sortedDepthNodes
            |> List.sortWith (\a b -> compare (outEdges edges a.id) (outEdges edges b.id))
            |> List.reverse
            |> List.indexedMap setX


setX : Int -> PlacedNode -> PlacedNode
setX index node =
    { node | x = 120 * index }


placeEdges : Edges -> PlacedNodes -> PlacedEdges
placeEdges edges placedNodes =
    List.filterMap (placeEdge placedNodes edges) edges


placeEdge : PlacedNodes -> Edges -> Edge -> Maybe PlacedEdge
placeEdge placedNodes edges edge =
    let
        from =
            List.head (List.filter (\node -> node.id == edge.from) placedNodes)

        to =
            List.head (List.filter (\node -> node.id == edge.to) placedNodes)
    in
        case ( from, to ) of
            ( Just from, Just to ) ->
                if from /= to then
                    let
                        angle =
                            atan2 (toFloat (to.y - from.y)) (toFloat (to.x - from.x))

                        distance =
                            sqrt ((toFloat (to.y - from.y)) ^ 2 + (toFloat (to.x - from.x)) ^ 2)
                    in
                        if List.any (\e -> e.id == ( to.id, from.id )) edges then
                           Just  { id = edge.id
                            , x1 = from.x + 50 + round (50 * cos (angle)) + round (8 * cos (angle + 3.1415 / 2))
                            , y1 = from.y + 50 + round (50 * sin (angle)) + round (8 * sin (angle + 3.1415 / 2))
                            , x2 = to.x + 50 - round (60 * cos (angle)) + round (8 * cos (angle + 3.1415 / 2))
                            , y2 = to.y + 50 - round (60 * sin (angle)) + round (8 * sin (angle + 3.1415 / 2))
                            , cx = from.x + 50 + round (distance / 2 * cos (angle + 3.1415 / 32)) + round (8 * cos (angle + 3.1415 / 2))
                            , cy = from.y + 50 + round (distance / 2 * sin (angle + 3.1415 / 32)) + round (8 * sin (angle + 3.1415 / 2))
                            }
                        else
                            Just { id = edge.id
                            , x1 = from.x + 50
                            , y1 = from.y + 50
                            , x2 = to.x + 50 - round (60 * cos (angle))
                            , y2 = to.y + 50 - round (60 * sin (angle))
                            , cx = from.x + 50
                            , cy = from.y + 50
                            }
                else
                    Just { id = edge.id
                    , x1 = from.x - 12
                    , y1 = from.y + 50
                    , x2 = from.x - 10
                    , y2 = from.y + 50
                    , cx = from.x - 12
                    , cy = from.y + 50
                    }

            _ ->
                Nothing
