module Voog.Update exposing (center, force, moveEdges, movement, update)

import Time
import Voog.Action exposing (..)
import Voog.Input exposing (..)
import Voog.Layouts.Forced exposing (forceTick)
import Voog.Messages exposing (..)
import Voog.Model exposing (..)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickNode id ->
            toggleNode model id

        ClickEdge id ->
            toggleEdge model id

        CloseInfo _ ->
            closeInfo model

        InputMsg msgInner ->
            handleInput model msgInner

        AcceptInvalidInput ->
            { model | invalidInput = False }

        Tick diff ->
            model
                |> center
                |> moveEdges diff
                |> force
                |> movement diff

        UpdateWindowSize size ->
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
                    1 - toFloat wheelDelta / 3000

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


moveEdges : Float -> Model -> Model
moveEdges diff model =
    let
        newEdges =
            List.map
                (\e ->
                    case e.speed of
                        Just speed ->
                            { e | dashOffset = e.dashOffset - speed * diff / 1000.0 }

                        Nothing ->
                            e
                )
                model.edges
    in
    { model | edges = newEdges }


force : Model -> Model
force model =
    if List.member "forced" (String.split "." <| Maybe.withDefault "" model.layout) then
        if model.center && model.force > 0 then
            centerGraph <| forceTick model

        else
            forceTick model

    else
        model


movement : Float -> Model -> Model
movement diff ({ movements } as model) =
    let
        newMovements =
            List.filterMap
                (\m ->
                    let
                        newRunning =
                            m.runningTime + diff / 1000.0
                    in
                    if newRunning > m.duration then
                        Nothing

                    else
                        Just { m | runningTime = newRunning }
                )
                movements
    in
    { model | movements = newMovements }
