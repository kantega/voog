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
    div []
        ((popup model)
            :: [ svg [ width "10000", height "10000", viewBox "0 0 10000 10000" ]
                    ((defs model)
                        :: (List.append
                                (List.foldr List.append [] (List.filterMap viewNode model.nodes))
                                (List.foldr List.append [] (List.filterMap viewEdge model.edges))
                           )
                    )
               ]
        )


popup : Model -> Html Msg
popup model =
    case List.head (List.filter (\n -> n.selected) model.nodes) of
        Just node ->
            case node.position of
                Just { x, y } ->
                    div
                        [ Svg.Attributes.style
                            ("position: absolute; left: "
                                ++ (toString (x + 110))
                                ++ "px; top: "
                                ++ (toString (y + 10))
                                ++ "px; background-color: #808080; color: #fff; padding: 10px 15px; font-family: sans-serif; border-radius: 5px;"
                            )
                        ]
                        ((p [ Svg.Attributes.style "margin-top: 5px;" ] [ Html.text "Info" ])
                            :: (List.map
                                    (\( k, v ) -> p [ Svg.Attributes.style "margin: 5px 0;" ] [ Html.text (k ++ ": " ++ v) ])
                                    node.info
                               )
                        )

                _ ->
                    div [] []

        _ ->
            case List.head (List.filter (\e -> e.selected) model.edges) of
                Just edge ->
                    case edge.position of
                        Just (Straight line) ->
                            div
                                [ Svg.Attributes.style
                                    ("position: absolute; left: "
                                        ++ (toString ((toFloat (line.from.x + line.to.x)) / 2))
                                        ++ "px; top: "
                                        ++ (toString ((toFloat (line.from.y + line.to.y)) / 2))
                                        ++ "px; background-color: #808080; color: #fff; padding: 5px 10px; font-family: sans-serif; border-radius: 5px;"
                                    )
                                ]
                                ((p [ Svg.Attributes.style "margin-top: 0;" ] [ Html.text "Info" ])
                                    :: (List.map
                                            (\( k, v ) -> p [ Svg.Attributes.style "margin: 5px 0;" ] [ Html.text (k ++ ": " ++ v) ])
                                            edge.info
                                       )
                                )

                        _ ->
                            div [] []

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


viewNode : Node -> Maybe (List (Svg Msg))
viewNode node =
    case node.position of
        Just { x, y } ->
            Just
                [ (case getAttribute "type" node of
                    Just "rect" ->
                        rect
                            [ onClick (ClickNode node.id)
                            , Svg.Attributes.x (toString x)
                            , Svg.Attributes.y (toString y)
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
                            , cx (toString (x + nodeRadius))
                            , cy (toString (y + nodeRadius))
                            , r (toString nodeRadius)
                            , fill (Maybe.withDefault "#f0f0f0" node.color)
                            , stroke "#f44336"
                            , strokeWidth (getStrokeWidth node)
                            ]
                  )
                    []
                , circle
                    [ cx (toString (x + nodeRadius))
                    , cy (toString (y + 30))
                    , r "25"
                    , fill ("url(#img" ++ (toString node.id) ++ ")")
                    ]
                    []
                , Svg.text_
                    [ Svg.Attributes.x (toString (x + nodeRadius))
                    , Svg.Attributes.y (toString (y + round (nodeRadius * 1.4)))
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


path : Line -> String
path position =
    case position of
        Straight line ->
            "M"
                ++ (toString line.from.x)
                ++ " "
                ++ (toString line.from.y)
                ++ " "
                ++ (toString line.to.x)
                ++ " "
                ++ (toString line.to.y)

        Curved line ->
            "M"
                ++ (toString line.from.x)
                ++ " "
                ++ (toString line.from.y)
                ++ " Q "
                ++ (toString line.via.x)
                ++ " "
                ++ (toString line.via.y)
                ++ " "
                ++ (toString line.to.x)
                ++ " "
                ++ (toString line.to.y)

        Multi line ->
            lineToString line True


lineToString : List Point -> Bool -> String
lineToString line first =
    let
        char =
            if first then
                "M"
            else
                "L"
    in
        case line of
            head :: rest ->
                char ++ " " ++ (toString (head.x)) ++ " " ++ (toString (head.y)) ++ " " ++ lineToString rest False

            _ ->
                ""


viewEdge : Edge -> Maybe (List (Html Msg))
viewEdge edge =
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
                        , d (path position)
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
                            ]
                            [ Svg.text (Maybe.withDefault "" edge.label)
                            ]
                        ]
                     ]
                    )

        _ ->
            Nothing
