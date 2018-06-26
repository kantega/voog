module Attributes exposing (..)

import Dict
import Model exposing (..)


getAttribute : String -> InfoElement e -> Maybe String
getAttribute attribute element =
    element.info
        |> Dict.fromList
        |> Dict.get attribute


getAttributeWithDefault : String -> String -> InfoElement e -> String
getAttributeWithDefault attribute default element =
    Maybe.withDefault default (getAttribute attribute element)
