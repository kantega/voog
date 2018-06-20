module Sugiyama.Benchmark exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (..)
import Sugiyama.Sugiyama exposing (..)
import Sugiyama.CrossReduction exposing (..)
import Sugiyama.CycleRemoval exposing (..)
import Sugiyama.DummyNodes exposing (..)
import Sugiyama.InitialPlacement exposing (..)
import Sugiyama.Layering exposing (..)
import Sugiyama.Placement exposing (..)
import Sugiyama.Model exposing (..)


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

        reducedCrossingUp =
            reduceCrossing Up initialPositioned

        reducedCrossingDown =
            reduceCrossing Down reducedCrossingUp

        positioned =
            setPosition reducedCrossingDown
    in
        Benchmark.describe "Sugiyama"
            [ Benchmark.benchmark "initialize" (\_ -> initialize graph)
            , Benchmark.benchmark "removeCycles" (\_ -> removeCycles initilized)
            , Benchmark.benchmark "layer" (\_ -> layer cyclesRemoved)
            , Benchmark.benchmark "addDummies" (\_ -> addDummies layered)
            , Benchmark.benchmark "setInitialPosition" (\_ -> setInitialPosition dummiesAdded)
            , Benchmark.benchmark "reduceCrossing Up" (\_ -> reduceCrossing Up initialPositioned)
            , Benchmark.benchmark "reducedCrossing Down" (\_ -> reduceCrossing Down reducedCrossingUp)
            , Benchmark.benchmark "setPosition" (\_ -> setPosition reducedCrossingDown)
            ]


main : BenchmarkProgram
main =
    program suite
