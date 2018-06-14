module Attributes exposing (..)

import Dict
import Model exposing (..)


getAttribute : String -> InfoElement e -> Maybe String
getAttribute attribute element =
    Dict.get attribute element.info
