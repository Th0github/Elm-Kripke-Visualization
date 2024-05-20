module Main exposing (..)

import Html exposing (Html,text, div, pre)
import Pages.KripkeModel exposing (KripkeModel)
import Graph.GraphKripke exposing (kripkeToGraph, generateDot)
import Html exposing (a)
import Url.Parser exposing (oneOf)
import Svg exposing (Svg)

type Route
    = Blog Int
    | Kripke String
