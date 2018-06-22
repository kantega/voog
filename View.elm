module View exposing (..)

import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Place exposing (..)
import Action exposing (..)
import Attributes exposing (..)


view : Model -> Html Msg
view model =
    let
        windowWidth =
            model.windowSize
                |> Maybe.withDefault ( 0, 0 )
                |> Tuple.first

        windowHeight =
            model.windowSize
                |> Maybe.withDefault ( 0, 0 )
                |> Tuple.second

        xx =
            round model.position.x

        yy =
            round model.position.y
    in
        div [ Svg.Attributes.style "overflow: hidden;" ]
            ((popup model)
                :: [ svg
                        [ width (toString windowWidth)
                        , height (toString windowHeight)
                        , viewBox ("0 0 " ++ (toString (toFloat windowWidth / model.zoom)) ++ " " ++ (toString (toFloat windowHeight / model.zoom)))
                        ]
                        ((defs model)
                            :: (List.append
                                    (List.foldr List.append [] (List.filterMap (viewNode ( xx, yy )) model.nodes))
                                    (List.foldr List.append [] (List.filterMap (viewEdge ( xx, yy )) model.edges))
                               )
                        )
                   ]
            )


infoList : List ( String, String ) -> Html Msg
infoList info =
    div
        [ Svg.Attributes.style
            ("position: absolute; left: -1px; top: -1px; display: grid; grid-template-columns: minmax(150px, 1fr) minmax(150px, 1fr); grid-gap: 10px; background-color: #ffffff; border: 1px solid #ccc; padding: 20px 30px; font-family: sans-serif;")
        ]
        (List.concat
            (List.append
                [ [ p [ Svg.Attributes.style "margin-top: 5px; font-size: 24px;" ] [ Html.text "Info" ], p [] [] ] ]
                (List.map
                    (\( k, v ) ->
                        [ p [ Svg.Attributes.style "margin: 5px 0; font-size: 20px;" ] [ Html.text k ]
                        , p [ Svg.Attributes.style "margin: 5px 0; font-size: 20px;" ] [ Html.text v ]
                        ]
                    )
                    info
                )
            )
        )


popup : Model -> Html Msg
popup model =
    case List.head (List.filter (\n -> n.selected) model.nodes) of
        Just node ->
            infoList node.info

        _ ->
            case List.head (List.filter (\e -> e.selected) model.edges) of
                Just edge ->
                    infoList edge.info

                _ ->
                    div [] []


defs : Model -> Html Msg
defs model =
    let
        imageNodes =
            List.filter (\n -> n.image /= Nothing) model.nodes

        imageIds =
            List.map (\n -> n.id) imageNodes

        placedImageNodes =
            List.filter (\n -> List.member n.id imageIds) model.nodes
    in
        Svg.defs []
            (List.append
                ([ Svg.marker
                    [ id "arrow"
                    , orient "auto"
                    , markerWidth "3"
                    , markerHeight "6"
                    , refX "0.1"
                    , refY "3"
                    ]
                    [ Svg.path [ d "M0,0 V6 L3,3 Z", fill "#b0b0b0" ] [] ]
                 , Svg.marker
                    [ id "selectedArrow"
                    , orient "auto"
                    , markerWidth "3"
                    , markerHeight "6"
                    , refX "0.1"
                    , refY "3"
                    ]
                    [ Svg.path [ d "M0,0 V6 L3,3 Z", fill "#808080" ] [] ]
                 ]
                )
                (List.filterMap
                    (\node ->
                        case node.position of
                            Just { x, y } ->
                                Just
                                    (Svg.pattern
                                        [ id ("img" ++ (toString node.id))
                                        , height "100%"
                                        , width "100%"
                                        , patternContentUnits "objectBoundingBox"
                                        ]
                                        [ Svg.image
                                            [ xlinkHref (Maybe.withDefault "" node.image)
                                            , preserveAspectRatio "xMidYMid slice"
                                            , width "1"
                                            , height "1"
                                            ]
                                            []
                                        ]
                                    )

                            Nothing ->
                                Nothing
                    )
                    imageNodes
                )
            )


getStrokeWidth : Node -> String
getStrokeWidth node =
    (if getAttribute "error" node == Just "True" then
        "3"
     else
        "0"
    )


viewNode : ( Int, Int ) -> Node -> Maybe (List (Svg Msg))
viewNode ( xx, yy ) node =
    case node.position of
        Just { x, y } ->
            Just
                [ (case getAttribute "type" node of
                    Just "rect" ->
                        rect
                            [ onClick (ClickNode node.id)
                            , Svg.Attributes.x (toString (xx + x))
                            , Svg.Attributes.y (toString (yy + y))
                            , width "100"
                            , height "100"
                            , rx "15"
                            , ry "15"
                            , fill "#f0f0f0"
                            , stroke "#f44336"
                            , strokeWidth (getStrokeWidth node)
                            ]

                    _ ->
                        circle
                            [ onClick (ClickNode node.id)
                            , cx (toString (xx + x + nodeRadius))
                            , cy (toString (yy + y + nodeRadius))
                            , r (toString (Maybe.withDefault nodeRadius node.size))
                            , fill (Maybe.withDefault "#f0f0f0" node.color)
                            , stroke "#f44336"
                            , strokeWidth (getStrokeWidth node)
                            ]
                  )
                    []
                , circle
                    [ cx (toString (xx + x + nodeRadius))
                    , cy (toString (yy + y + 30))
                    , r "25"
                    , fill ("url(#img" ++ (toString node.id) ++ ")")
                    ]
                    []
                , Svg.text_
                    [ Svg.Attributes.x (toString (xx + x + nodeRadius))
                    , Svg.Attributes.y (toString (yy + y + round (nodeRadius * 1.4)))
                    , fill "#b0b0b0"
                    , fontFamily "sans-serif"
                    , textAnchor "middle"
                    , alignmentBaseline "hanging"
                    ]
                    [ Svg.text (Maybe.withDefault "" node.name) ]
                ]

        _ ->
            Nothing


getStrokeColor : Bool -> ( String, String )
getStrokeColor selected =
    if selected then
        ( "url(#selectedArrow)", "#808080" )
    else
        ( "url(#arrow)", "#b0b0b0" )


path : Line -> ( Int, Int ) -> String
path position ( xx, yy ) =
    case position of
        Straight line ->
            "M"
                ++ (toString (xx + line.from.x))
                ++ " "
                ++ (toString (yy + line.from.y))
                ++ " "
                ++ (toString (xx + line.to.x))
                ++ " "
                ++ (toString (yy + line.to.y))

        Curved line ->
            "M"
                ++ (toString (xx + line.from.x))
                ++ " "
                ++ (toString (yy + line.from.y))
                ++ " Q "
                ++ (toString (xx + line.via.x))
                ++ " "
                ++ (toString (yy + line.via.y))
                ++ " "
                ++ (toString (xx + line.to.x))
                ++ " "
                ++ (toString (yy + line.to.y))

        Multi line ->
            lineToString line ( xx, yy ) True


lineToString : List Point -> ( Int, Int ) -> Bool -> String
lineToString line ( xx, yy ) first =
    let
        char =
            if first then
                "M"
            else
                "L"
    in
        case line of
            head :: rest ->
                char
                    ++ " "
                    ++ (toString (xx + head.x))
                    ++ " "
                    ++ (toString (yy + head.y))
                    ++ " "
                    ++ lineToString rest ( xx, yy ) False

            _ ->
                ""


viewEdge : ( Int, Int ) -> Edge -> Maybe (List (Html Msg))
viewEdge ( xx, yy ) edge =
    case edge.position of
        Just position ->
            let
                ( marker, strokeColor ) =
                    getStrokeColor edge.selected
            in
                Just
                    ([ Svg.path
                        [ onClick (ClickEdge ( edge.from, edge.to ))
                        , id ((toString (Tuple.first edge.id)) ++ "_" ++ (toString (Tuple.second edge.id)))
                        , markerEnd marker
                        , fill "none"
                        , strokeWidth (toString (Maybe.withDefault 1 edge.width))
                        , stroke strokeColor
                        , d (path position ( xx, yy ))
                        ]
                        []
                     , Svg.text_
                        [ fill "#808080"
                        , fontSize "20"
                        , fontFamily "sans-serif"
                        , textAnchor "middle"
                        , dy "-5"
                        ]
                        [ Svg.textPath
                            [ xlinkHref ("#" ++ (toString (Tuple.first edge.id)) ++ "_" ++ (toString (Tuple.second edge.id)))
                            , startOffset "50%"
                            , orient "up"
                            ]
                            [ Svg.text (Maybe.withDefault "" edge.label)
                            ]
                        ]
                     ]
                    )

        _ ->
            Nothing
