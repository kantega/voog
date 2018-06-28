module Update exposing (..)

import Task
import Time
import Model exposing (..)
import Messages exposing (..)
import Action exposing (..)
import Input


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickNode id ->
            ( toggleNode model id, Cmd.none )

        ClickEdge id ->
            ( toggleEdge model id, Cmd.none )

        CloseInfo _ ->
            ( closeInfo model, Cmd.none )

        InputMsg msg ->
            ( Input.handleInput model msg, Cmd.none )

        Tick diff ->
            let
                newEdges =
                    List.map
                        (\e ->
                            case e.speed of
                                Just speed ->
                                    { e | dashOffset = e.dashOffset - speed * (Time.inSeconds diff) }

                                Nothing ->
                                    e
                        )
                        model.edges
            in
                ( { model | edges = newEdges }, Cmd.none )

        WindowSize size ->
            ( updateWindow size model, Cmd.none )

        MouseMove ( x, y ) ->
            let
                pos =
                    model.position
            in
                case ( model.drag, model.mouse ) of
                    ( True, Just mouse ) ->
                        let
                            dx =
                                toFloat (x - mouse.x)

                            dy =
                                toFloat (y - mouse.y)

                            newPos =
                                { pos | x = pos.x + dx, y = pos.y + dy }
                        in
                            ( { model | position = newPos, mouse = Just { x = x, y = y } }, Cmd.none )

                    _ ->
                        ( { model | mouse = Just { x = x, y = y } }, Cmd.none )

        MouseUp ( btn, x, y ) ->
            ( { model | drag = False }, Cmd.none )

        MouseDown ( btn, x, y ) ->
            let
                drag =
                    if btn == 1 || btn == 2 then
                        True
                    else
                        False
            in
                ( { model | drag = drag }, Cmd.none )

        MouseWheel wheelDelta ->
            let
                zoomFactor =
                    1 - (toFloat wheelDelta) / 3000

                newZoom =
                    model.zoom * zoomFactor

                clampedZoom =
                    if newZoom > 10 then
                        10
                    else if newZoom < 0.1 then
                        0.1
                    else
                        newZoom

                scaleChange =
                    (clampedZoom - model.zoom) / model.zoom

                ({ x, y } as pos) =
                    model.position

                ( elementX, elementY ) =
                    model.elementPosition

                newPosition =
                    case ( model.windowSize, model.mouse ) of
                        ( Just ( width, height ), Just mouse ) ->
                            { pos | x = x - (toFloat (mouse.x - elementX) - x) * scaleChange, y = y - (toFloat (mouse.y - elementY) - y) * scaleChange }

                        _ ->
                            pos
            in
                ( { model | zoom = clampedZoom, position = newPosition }, Cmd.none )
