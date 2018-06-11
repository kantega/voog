module Creators exposing (..)

import Model exposing (..)
import Char


empty : Model
empty =
    { nodes = [], edges = [] }


createNode : Int -> Node
createNode n =
    { id = n
    , name = toString (Char.fromCode (n + 65))
    }


addNode : Model -> Model
addNode model =
    { model | nodes = (createNode (List.length model.nodes)) :: model.nodes }


addEdge : Model -> Int -> Int -> Model
addEdge model from to =
    { model | edges = { from = from, to = to } :: model.edges }
