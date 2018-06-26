module Update exposing (..)

import Random
import Char exposing (..)
import Model exposing (..)
import Input exposing (..)
import Action exposing (..)
import Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickNode id ->
            ( toggleNode model id, Cmd.none )

        ClickEdge id ->
            ( toggleEdge model id, Cmd.none )

        SocketMsg msg ->
            ( handleInput model msg, Cmd.none )

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
                case model.drag of
                    Just drag ->
                        let
                            dx =
                                toFloat (x - drag.x) / model.zoom

                            dy =
                                toFloat (y - drag.y) / model.zoom

                            newPos =
                                { pos | x = pos.x + dx, y = pos.y + dy }
                        in
                            ( { model | position = newPos, drag = Just point }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

        MouseUp { x, y } ->
            ( { model | drag = Nothing }, Cmd.none )

        MouseDown point ->
            ( { model | drag = Just point }, Cmd.none )
