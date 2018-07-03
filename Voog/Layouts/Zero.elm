module Voog.Layouts.Zero exposing (..)

import Voog.Model exposing (..)


zeroLayout : Model -> Model
zeroLayout ({ nodes } as model) =
    let
        zero =
            { x = 0, y = 0 }

        newNodes =
            List.map (\n -> { n | position = Just (Maybe.withDefault zero n.position) }) nodes
    in
        { model | nodes = newNodes }
