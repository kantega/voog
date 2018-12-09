module Voog.View exposing (defaultDistance, defs, getViewNode, invalidInput, labelHeight, labelWidth, lineToString, nodeRadius, path, textPosition, view, viewEdge, viewInfoList, viewLabel, viewMovement, viewNode, viewNodeWithHref, viewTooltip)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Voog.Messages exposing (..)
import Voog.Model exposing (..)


nodeRadius : Float
nodeRadius =
    45


defaultDistance : Float
defaultDistance =
    4 * nodeRadius


labelWidth : Float
labelWidth =
    60


labelHeight : Float
labelHeight =
    30


view : Model -> Html Msg
view model =
    let
        ( windowWidth, windowHeight ) =
            Maybe.withDefault ( 0, 0 ) model.windowSize

        edgeDict =
            model.edges
                |> List.map (\e -> ( e.id, e ))
                |> Dict.fromList
    in
    div [ class "voog" ]
        [ viewTooltip model
        , svg
            [ Voog.Messages.onMouseWheel MouseWheel
            , Voog.Messages.onMouseMove MouseMove
            , Voog.Messages.onMouseUp MouseUp
            , Voog.Messages.onMouseDown MouseDown
            , width (String.fromInt windowWidth)
            , height (String.fromInt windowHeight)
            , viewBox
                ((String.fromFloat <| -model.position.x / model.zoom)
                    ++ " "
                    ++ (String.fromFloat <| -model.position.y / model.zoom)
                    ++ " "
                    ++ (String.fromFloat <| toFloat windowWidth / model.zoom)
                    ++ " "
                    ++ (String.fromFloat <| toFloat windowHeight / model.zoom)
                )
            ]
            (List.concat
                [ [ defs model ]
                , List.foldr List.append [] (List.filterMap viewEdge model.edges)
                , List.foldr List.append [] (List.filterMap viewLabel model.edges)
                , List.map (viewMovement edgeDict) model.movements
                , List.foldr List.append [] (List.filterMap (getViewNode model.name) model.nodes)
                , invalidInput model
                ]
            )
        ]


invalidInput : Model -> List (Svg Msg)
invalidInput model =
    if model.invalidInput then
        let
            ( windowWidth, windowHeight ) =
                Maybe.withDefault ( 0, 0 ) model.windowSize
        in
        [ rect
            [ onClick AcceptInvalidInput
            , fill "rgba(255, 0, 0, 0.5)"
            , x <| String.fromFloat <| -model.position.x / model.zoom
            , y <| String.fromFloat <| -model.position.y / model.zoom
            , width <| String.fromFloat <| toFloat windowWidth / model.zoom
            , height <| String.fromFloat <| toFloat windowHeight / model.zoom
            ]
            []
        , Svg.text_
            [ x <| String.fromFloat <| -model.position.x / model.zoom + toFloat windowWidth / model.zoom / 2
            , y <| String.fromFloat <| -model.position.y / model.zoom + toFloat windowHeight / model.zoom / 2
            , fill "rgba(255, 0, 0, 0.4)"
            , textAnchor "middle"
            , alignmentBaseline "middle"
            , fontWeight "700"
            , fontSize <| String.fromFloat <| 50 / model.zoom
            ]
            [ Svg.text "Invalid input" ]
        ]

    else
        []


viewTooltip : Model -> Html Msg
viewTooltip model =
    case List.head (List.filter (\n -> n.selected) model.nodes) of
        Just node ->
            viewInfoList model
                node.info
                node.position
                node.name
                ( Maybe.withDefault nodeRadius node.width, nodeRadius - Maybe.withDefault nodeRadius node.height )

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
            { x = model.position.x + (offsetX + elementPos.x + 10) * model.zoom
            , y = model.position.y + (offsetY + elementPos.y) * model.zoom
            }
    in
    div
        [ class "info-popup"
        , Svg.Attributes.style <| "left: " ++ String.fromFloat pos.x ++ "; top: " ++ String.fromFloat pos.y ++ ";"
        ]
        [ div [ class "info-top" ]
            [ h3 [ class "info-header" ] [ Html.text <| Maybe.withDefault "Info" name ]
            , h3
                [ Voog.Messages.onMouseDown CloseInfo
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
                                [ id (model.name ++ "_img" ++ String.fromInt node.id)
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


viewMovement : Dict ( Int, Int ) Edge -> Movement InputMovement -> Svg Msg
viewMovement edges movement =
    let
        pathString =
            case Dict.get ( movement.from, movement.to ) edges of
                Just edge ->
                    case edge.position of
                        Just line ->
                            path line

                        Nothing ->
                            ""

                Nothing ->
                    ""

        distance =
            String.fromFloat <| movement.runningTime / movement.duration * 100
    in
    g
        [ class <| String.join " " <| "movement" :: movement.classes
        , Svg.Attributes.style <| "offset-path: path(\"" ++ pathString ++ "\"); offset-distance: " ++ distance ++ "%;"
        ]
        [ use [ xlinkHref <| "#" ++ movement.icon ] []
        ]


getViewNode : String -> Node -> Maybe (List (Svg Msg))
getViewNode modelName node =
    case node.viewNode of
        Just vn ->
            Just vn

        Nothing ->
            viewNodeWithHref modelName node


viewNodeWithHref : String -> Node -> Maybe (List (Svg Msg))
viewNodeWithHref modelName node =
    case node.href of
        Just href ->
            case viewNode modelName node of
                Nothing ->
                    Nothing

                Just renderedNode ->
                    Just
                        [ Svg.a
                            [ Svg.Attributes.xlinkHref href, Svg.Attributes.target "_blank" ]
                            renderedNode
                        ]

        _ ->
            viewNode modelName node


viewNode : String -> Node -> Maybe (List (Svg Msg))
viewNode modelName node =
    case node.position of
        Just { x, y } ->
            let
                nodeWidth =
                    Maybe.withDefault nodeRadius node.width

                nodeHeight =
                    Maybe.withDefault nodeRadius node.height
            in
            Just
                [ (case node.shape of
                    Just "rect" ->
                        rect
                            [ --onClick (ClickNode node.id)
                              class <| String.join " " <| "node rect" :: node.classes
                            , Svg.Attributes.x (String.fromFloat <| x - nodeWidth)
                            , Svg.Attributes.y (String.fromFloat <| y - nodeHeight)
                            , width <| String.fromFloat <| 2 * nodeWidth
                            , height <| String.fromFloat <| 2 * nodeHeight
                            ]

                    _ ->
                        ellipse
                            [ --onClick (ClickNode node.id)
                              class <| String.join " " <| "node circle" :: node.classes
                            , cx (String.fromFloat x)
                            , cy (String.fromFloat y)
                            , rx (String.fromFloat nodeWidth)
                            , ry (String.fromFloat nodeHeight)
                            ]
                  )
                    []
                , circle
                    [ --onClick (ClickNode node.id)
                      class <| String.join " " <| "node-image" :: node.classes
                    , cx (String.fromFloat x)
                    , cy (String.fromFloat (y - nodeRadius + 30))
                    , fill ("url(#" ++ modelName ++ "_img" ++ String.fromInt node.id ++ ")")
                    ]
                    []
                , Svg.foreignObject
                    [ --onClick (ClickNode node.id)
                      class <| String.join " " <| "node-text-wrapper" :: node.classes
                    , Svg.Attributes.x (String.fromFloat (x - nodeWidth * 0.8))
                    , Svg.Attributes.y (String.fromFloat (y - nodeHeight * 0.8))
                    , Svg.Attributes.width (String.fromFloat (1.6 * nodeWidth))
                    , Svg.Attributes.height (String.fromFloat (1.6 * nodeHeight))
                    ]
                    [ Html.span
                        [ class "node-text" ]
                        [ Html.text (Maybe.withDefault "" node.name) ]
                    ]
                ]

        _ ->
            Nothing


textPosition : Float -> Maybe String -> String
textPosition y image =
    case image of
        Just _ ->
            String.fromFloat (y + nodeRadius * 1 / 2)

        Nothing ->
            String.fromFloat y


path : Line -> String
path position =
    case position of
        Straight line ->
            "M"
                ++ String.fromFloat line.from.x
                ++ " "
                ++ String.fromFloat line.from.y
                ++ " "
                ++ String.fromFloat line.to.x
                ++ " "
                ++ String.fromFloat line.to.y

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
                ++ String.fromFloat head.x
                ++ " "
                ++ String.fromFloat head.y
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
                    , class <| String.join " " <| "edge" :: edge.classes
                    , id (String.fromInt (Tuple.first edge.id) ++ "_" ++ String.fromInt (Tuple.second edge.id))
                    , strokeWidth (String.fromFloat (Maybe.withDefault 8 edge.width))
                    , d (path position)
                    ]
                    []
                , Svg.path
                    [ onClick (ClickEdge edge.id)
                    , class <| String.join " " <| "edge-dash" :: edge.classes
                    , id (String.fromInt (Tuple.first edge.id) ++ "_" ++ String.fromInt (Tuple.second edge.id))
                    , strokeWidth (String.fromFloat (0.75 * Maybe.withDefault 8 edge.width))
                    , strokeDasharray (String.fromFloat (2 * Maybe.withDefault 8 edge.width))
                    , strokeDashoffset (String.fromFloat edge.dashOffset)
                    , d (path position)
                    ]
                    []
                ]

        _ ->
            Nothing


viewLabel : Edge -> Maybe (List (Html Msg))
viewLabel edge =
    case edge.position of
        Just _ ->
            Just
                (case ( edge.labelPosition, edge.label ) of
                    ( Just position, Just label ) ->
                        [ rect
                            [ onClick (ClickEdge edge.id)
                            , class <| String.join " " <| "label" :: edge.classes
                            , x (String.fromFloat (position.x - labelWidth / 2))
                            , y (String.fromFloat (position.y - labelHeight / 2))
                            , width (String.fromFloat labelWidth)
                            , height (String.fromFloat labelHeight)
                            ]
                            []
                        , Svg.text_
                            [ onClick (ClickEdge edge.id)
                            , class <| String.join " " <| "label-text" :: edge.classes
                            , x (String.fromFloat position.x)
                            , y (String.fromFloat (position.y + 2))
                            ]
                            [ Svg.text label
                            ]
                        ]

                    _ ->
                        []
                )

        _ ->
            Nothing
