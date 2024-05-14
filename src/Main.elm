module Main exposing (..)

import Html exposing (Html,text, div, pre)
import Kripke.KripkeModel exposing (KripkeModel, valuationFunction)
import Graph.GraphKripke exposing (kripkeToGraph, generateDot)

view : Html msg
view = 
    let
        model = KrM [0,1,2] valuationFunction [(0,1),(1,2),(2,0)]
        graph = kripkeToGraph model
        dotOutput = generateDot graph
    in
    div [] [pre [] [text dotOutput]]
    

main =
    Html.program
        { init = ( (), Cmd.none )
        , view = (\_ -> view)
        , update = (\_ _ -> ( (), Cmd.none ))
        , subscriptions = (\_ -> Sub.none)
        }
