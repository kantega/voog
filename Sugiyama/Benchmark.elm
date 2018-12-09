module Sugiyama.Benchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (..)
import Sugiyama.CrossReduction exposing (..)
import Sugiyama.CycleRemoval exposing (..)
import Sugiyama.DummyNodes exposing (..)
import Sugiyama.InitialPlacement exposing (..)
import Sugiyama.Layering exposing (..)
import Sugiyama.Model exposing (..)
import Sugiyama.Placement exposing (..)
import Sugiyama.Sugiyama exposing (..)


suite : Benchmark
suite =
    let
        graph =
            { nodes = [ 0, 1, 2, 3, 4, 5, 6 ]
            , edges = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 5 ), ( 5, 6 ), ( 6, 0 ), ( 0, 2 ), ( 3, 1 ), ( 1, 4 ) ]
            }

        initilized =
            initialize graph

        cyclesRemoved =
            removeCycles initilized

        layered =
            layer cyclesRemoved

        dummiesAdded =
            addDummies layered

        initialPositioned =
            setInitialPosition dummiesAdded

        reducedCrossing =
            reduceCrossing initialPositioned

        positioned =
            setPosition reducedCrossing
    in
    Benchmark.describe "Sugiyama"
        [ Benchmark.benchmark "initialize" (\_ -> initialize graph)
        , Benchmark.benchmark "removeCycles" (\_ -> removeCycles initilized)
        , Benchmark.benchmark "layer" (\_ -> layer cyclesRemoved)
        , Benchmark.benchmark "addDummies" (\_ -> addDummies layered)
        , Benchmark.benchmark "setInitialPosition" (\_ -> setInitialPosition dummiesAdded)
        , Benchmark.benchmark "reduceCrossing" (\_ -> reduceCrossing initialPositioned)
        , Benchmark.benchmark "setPosition" (\_ -> setPosition reducedCrossing)
        ]


main : BenchmarkProgram
main =
    program suite
