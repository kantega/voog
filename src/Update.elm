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

        InputMsg msg ->
            ( Input.handleInput model msg, Cmd.none )

        Tick time ->
            case model.time of
                Just prevTime ->
                    let
                        deltaTime =
                            (Time.inSeconds (time - prevTime))

                        newEdges =
                            List.map
                                (\e ->
                                    case e.speed of
                                        Just speed ->
                                            { e | dashOffset = e.dashOffset - speed * deltaTime }

                                        Nothing ->
                                            e
                                )
                                model.edges
                    in
                        ( { model | time = Just time, edges = newEdges }, Cmd.none )

                Nothing ->
                    ( { model | time = Just time }, Cmd.none )

        WindowSize { width, height } ->
            let
                newPosition =
                    case model.windowSize of
                        Just ( oldWidth, oldHeight ) ->
                            let
                                dw =
                                    toFloat (width - oldWidth) / 2 / model.zoom

                                dh =
                                    toFloat (height - oldHeight) / 2 / model.zoom
                            in
                                { x = model.position.x + dw, y = model.position.y + dh }

                        Nothing ->
                            model.position

                newModel =
                    { model | position = newPosition, windowSize = Just ( width, height ) }
            in
                ( newModel, Cmd.none )

        MouseMove ({ x, y } as point) ->
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
                            ( { model | position = newPos, mouse = Just point }, Cmd.none )

                    _ ->
                        ( { model | mouse = Just point }, Cmd.none )

        MouseUp { x, y } ->
            ( { model | drag = False }, Cmd.none )

        MouseDown point ->
            ( { model | drag = True }, Cmd.none )

        MouseWheel wheelDelta ->
            let
                zoomFactor =
                    1 - wheelDelta / 3000

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

                newPosition =
                    case ( model.windowSize, model.mouse ) of
                        ( Just ( width, height ), Just mouse ) ->
                            { pos | x = x - (toFloat mouse.x - x) * scaleChange, y = y - (toFloat mouse.y - y) * scaleChange }

                        _ ->
                            pos
            in
                ( { model | zoom = clampedZoom, position = newPosition }, Cmd.none )
