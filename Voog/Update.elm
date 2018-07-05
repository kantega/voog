module Voog.Update exposing (..)

import Time
import Voog.Model exposing (..)
import Voog.Messages exposing (..)
import Voog.Action exposing (..)
import Voog.Input exposing (..)
import Voog.Layouts.Forced exposing (forceTick)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickNode id ->
            toggleNode model id

        ClickEdge id ->
            toggleEdge model id

        CloseInfo _ ->
            closeInfo model

        InputMsg msg ->
            handleInput model msg

        Tick diff ->
            model
                |> center
                |> moveEdges diff
                |> force
                |> movement diff

        WindowSize size ->
            updateWindow size model

        MouseMove ( xx, yy ) ->
            let
                ( x, y ) =
                    ( toFloat xx, toFloat yy )

                pos =
                    model.position
            in
                case ( model.drag, model.mouse ) of
                    ( True, Just mouse ) ->
                        let
                            dx =
                                x - mouse.x

                            dy =
                                y - mouse.y

                            newPos =
                                { pos | x = pos.x + dx, y = pos.y + dy }
                        in
                            { model | position = newPos, mouse = Just { x = x, y = y } }

                    _ ->
                        { model | mouse = Just { x = x, y = y } }

        MouseUp ( btn, x, y ) ->
            { model | drag = False }

        MouseDown ( btn, x, y ) ->
            let
                drag =
                    if btn == 1 || btn == 2 then
                        True
                    else
                        False
            in
                { model | drag = drag }

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
                            { pos | x = x - ((mouse.x - elementX) - x) * scaleChange, y = y - ((mouse.y - elementY) - y) * scaleChange }

                        _ ->
                            pos
            in
                { model | zoom = clampedZoom, position = newPosition }


center : Model -> Model
center model =
    if
        not model.initiallyCentered
            && (not <| (List.isEmpty model.edges && List.isEmpty model.nodes))
            && (model.windowSize /= Nothing)
    then
        centerGraph model
    else
        model


moveEdges : Time.Time -> Model -> Model
moveEdges diff model =
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
        { model | edges = newEdges }


force : Model -> Model
force model =
    if List.member "forced" (String.split "." <| Maybe.withDefault "" model.layout) then
        forceTick model
    else
        model


movement : Time.Time -> Model -> Model
movement diff ({ movements } as model) =
    let
        newMovements =
            List.filterMap
                (\m ->
                    let
                        newRunning =
                            m.runningTime + (Time.inSeconds diff)
                    in
                        if newRunning > m.duration then
                            Nothing
                        else
                            Just { m | runningTime = newRunning }
                )
                movements
    in
        { model | movements = newMovements }
