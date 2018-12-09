port module Ports exposing (input)


port input : (String -> msg) -> Sub msg
