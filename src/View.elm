module View exposing (..)

import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Messages exposing (..)
import Place exposing (..)
import Action exposing (..)
import Attributes exposing (..)


view : Model -> Html Msg
view model =
    let
        ( windowWidth, windowHeight ) =
            Maybe.withDefault ( 0, 0 ) model.windowSize
    in
        div []
            [ viewTooltip model
            , svg
                [ Messages.onMouseWheel MouseWheel
                , Messages.onMouseMove MouseMove
                , Messages.onMouseUp MouseUp
                , Messages.onMouseDown MouseDown
                , width (toString windowWidth)
                , height (toString windowHeight)
                , viewBox
                    ((toString <| -model.position.x / model.zoom)
                        ++ " "
                        ++ (toString <| -model.position.y / model.zoom)
                        ++ " "
                        ++ (toString <| toFloat windowWidth / model.zoom)
                        ++ " "
                        ++ (toString <| toFloat windowHeight / model.zoom)
                    )
                ]
                (List.concat
                    [ [ defs model ]
                    , (List.foldr List.append [] (List.filterMap viewEdge model.edges))
                    , (List.foldr List.append [] (List.filterMap viewNode model.nodes))
                    , (List.foldr List.append [] (List.filterMap viewLabel model.edges))
                    ]
                )
            ]


viewTooltip : Model -> Html Msg
viewTooltip model =
    case List.head (List.filter (\n -> n.selected) model.nodes) of
        Just node ->
            viewInfoList model node.info node.position node.name
            ( toFloat <| nodeRadius + (Maybe.withDefault nodeRadius node.size), toFloat <| nodeRadius - (Maybe.withDefault nodeRadius node.size) )

        _ ->
            case List.head (List.filter (\e -> e.selected) model.edges) of
                Just edge ->
                    viewInfoList model edge.info edge.labelPosition edge.label ( labelWidth / 2, -labelHeight / 2 )

                _ ->
                    Html.text ""


viewInfoList : Model -> List ( String, String ) -> Maybe Point -> Maybe String -> ( Float, Float ) -> Html Msg
viewInfoList model info maybeElementPos name ( offsetX, offsetY ) =
    let
        elementPos =
            Maybe.withDefault { x = 0, y = 0 } maybeElementPos

        pos =
            { x = model.position.x + (offsetX + toFloat elementPos.x + 10) * model.zoom
            , y = model.position.y + (offsetY + toFloat elementPos.y) * model.zoom
            }
    in
        div
            [ class "info-popup"
            , Svg.Attributes.style <| "left: " ++ (toString pos.x) ++ "; top: " ++ (toString pos.y) ++ ";"
            ]
            [ div [class "info-top"]
                [ h3 [class "info-header"] [ Html.text <| Maybe.withDefault "Info" name ]
                , h3
                    [ Messages.onMouseDown CloseInfo
                    , class "info-close"
                    ]
                    [ Html.text "✖" ]
                ]
            , div [ class "info-list" ]
                (List.concatMap
                    (\( k, v ) ->
                        [ p [] [ Html.text k ]
                        , p [] [ Html.text v ]
                        ]
                    )
                    info
                )
            ]


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
            (List.filterMap
                (\node ->
                    case node.position of
                        Just { x, y } ->
                            Just
                                (Svg.pattern
                                    [ id ("img" ++ (toString node.id))
                                    , class "image-pattern"
                                    , height "100%"
                                    , width "100%"
                                    , patternContentUnits "objectBoundingBox"
                                    ]
                                    [ Svg.image
                                        [ class "image-pattern-image"
                                        , xlinkHref (Maybe.withDefault "" node.image)
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


nodeColor : Node -> String
nodeColor node =
    case node.color of
        Just color ->
            color

        Nothing ->
            Maybe.withDefault "#808080" node.categoryColor


viewNode : Node -> Maybe (List (Svg Msg))
viewNode node =
    case node.position of
        Just { x, y } ->
            Just
                [ (case getAttribute "type" node of
                    Just "rect" ->
                        rect
                            [ onClick (ClickNode node.id)
                            , class "node-rect"
                            , Svg.Attributes.x (toString x)
                            , Svg.Attributes.y (toString y)
                            , width <| toString <| 2 * nodeRadius
                            , height <| toString <| 2 * nodeRadius
                            , stroke (nodeColor node)
                            , strokeWidth "5"
                            ]

                    _ ->
                        circle
                            [ onClick (ClickNode node.id)
                            , class "node-circle"
                            , cx (toString (x + nodeRadius))
                            , cy (toString (y + nodeRadius))
                            , r (toString (Maybe.withDefault nodeRadius node.size))
                            , stroke (nodeColor node)
                            , strokeWidth "5"
                            ]
                  )
                    []
                , circle
                    [ onClick (ClickNode node.id)
                    , class "node-image"
                    , cx (toString (x + nodeRadius))
                    , cy (toString (y + 30))
                    , fill ("url(#img" ++ (toString node.id) ++ ")")
                    ]
                    []
                , Svg.text_
                    [ onClick (ClickNode node.id)
                    , class "node-text"
                    , Svg.Attributes.x (toString (x + nodeRadius))
                    , Svg.Attributes.y (toString (y + round (nodeRadius * 1.2)))
                    ]
                    [ Svg.text (Maybe.withDefault "" node.name) ]
                ]

        _ ->
            Nothing


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
                char
                    ++ " "
                    ++ (toString head.x)
                    ++ " "
                    ++ (toString head.y)
                    ++ " "
                    ++ lineToString rest False

            _ ->
                ""


viewEdge : Edge -> Maybe (List (Html Msg))
viewEdge edge =
    case edge.position of
        Just position ->
            Just
                [ Svg.path
                    [ onClick (ClickEdge edge.id)
                    , class "edge"
                    , id ((toString (Tuple.first edge.id)) ++ "_" ++ (toString (Tuple.second edge.id)))
                    , strokeWidth (toString (Maybe.withDefault 8 edge.width))
                    , stroke (Maybe.withDefault "#fff" edge.color)
                    , d (path position)
                    ]
                    []
                , Svg.path
                    [ onClick (ClickEdge edge.id)
                    , class "edge-dash"
                    , id ((toString (Tuple.first edge.id)) ++ "_" ++ (toString (Tuple.second edge.id)))
                    , strokeWidth (toString (0.75 * (Maybe.withDefault 8 edge.width)))
                    , stroke (Maybe.withDefault "#b0b0b0" edge.dashColor)
                    , strokeDasharray (toString (2 * (Maybe.withDefault 8 edge.width)))
                    , strokeDashoffset (toString edge.dashOffset)
                    , d (path position)
                    ]
                    []
                ]

        _ ->
            Nothing


viewLabel : Edge -> Maybe (List (Html Msg))
viewLabel edge =
    case edge.position of
        Just position ->
            Just
                (case ( edge.labelPosition, edge.label ) of
                    ( Just position, Just label ) ->
                        [ rect
                            [ onClick (ClickEdge edge.id)
                            , class "label"
                            , x (toString (position.x - round (toFloat labelWidth / 2)))
                            , y (toString (position.y - round (toFloat labelHeight / 2)))
                            , width (toString labelWidth)
                            , height (toString labelHeight)
                            , stroke (Maybe.withDefault "#808080" edge.color)
                            ]
                            []
                        , Svg.text_
                            [ onClick (ClickEdge edge.id)
                            , class "label-text"
                            , fill (Maybe.withDefault "#b0b0b0" edge.color)
                            , x (toString position.x)
                            , y (toString (position.y + 2))
                            ]
                            [ Svg.text label
                            ]
                        ]

                    _ ->
                        []
                )

        _ ->
            Nothing
