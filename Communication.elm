module Communication exposing (..)

import Json.Decode exposing (..)
import Model exposing (..)
import Action exposing (..)


handleCommunication : Model -> String -> ( Model, Cmd Msg )
handleCommunication model msg =
    case decodeString (maybe (field "command" string)) msg of
        Ok (Just command) ->
            case decodeString (maybe (field "nodes" (keyValuePairs string))) msg of
                Ok (Just rawNodes) ->
                    let
                        nodes =
                            rawNodes
                                |> List.filterMap
                                    (\( k, v ) ->
                                        case ( String.toInt k ) of
                                            Ok id ->
                                                Just (id, v)

                                            _ ->
                                                Nothing
                                    )
                    in
                        ( addNodes model nodes, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
