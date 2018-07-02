module Voog.Layouts.Manual exposing (..)

import Voog.Model exposing (..)


manualLayout : List String -> Model -> Model
manualLayout layout ({ nodes, edges } as model) =
    let
        minX =
            nodes
                |> List.map .x
                |> List.filterMap identity
                |> List.minimum
                |> Maybe.withDefault 0

        minY =
            nodes
                |> List.map .y
                |> List.filterMap identity
                |> List.map (\y -> -y)
                |> List.minimum
                |> Maybe.withDefault 0

        positions =
            List.filterMap
                (\n ->
                    case ( n.x, n.y ) of
                        ( Just x, Just y ) ->
                            Just ( x, y )

                        _ ->
                            Nothing
                )
                nodes

        minDistnace =
            positions
                |> List.concatMap
                    (\( x1, y1 ) ->
                        List.filterMap
                            (\( x2, y2 ) ->
                                if x1 == x2 && y1 == y2 then
                                    Nothing
                                else
                                    Just <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2
                            )
                            positions
                    )
                |> List.minimum
                |> Maybe.withDefault 1
                |> (\n -> n ^ 0.5)

        newNodes =
            List.map
                (\n ->
                    { n
                        | position =
                            case ( n.x, n.y ) of
                                ( Just x, Just y ) ->
                                    Just { x = (x - minX) / minDistnace, y = (-y - minY) / minDistnace }

                                _ ->
                                    Nothing
                    }
                )
                nodes
    in
        { model | nodes = newNodes }
