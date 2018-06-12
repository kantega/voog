module Creators exposing (..)

import Model exposing (..)
import Depth exposing (..)
import Char


empty : Model
empty =
    { nodes = [], edges = [], depthNodes = [] }


createNode : Int -> Node
createNode n =
    { id = n
    , name = toString (Char.fromCode (n + 65))
    }


addNode : Model -> Model
addNode model =
    let
        nodes = (createNode (List.length model.nodes)) :: model.nodes
    in
        { model | nodes = nodes, depthNodes = calculateDepth model.edges nodes }


addEdge : Model -> Int -> Int -> Model
addEdge model from to =
    let
        edges = { from = from, to = to } :: model.edges
    in
        { model | edges = edges, depthNodes = calculateDepth edges model.nodes }
