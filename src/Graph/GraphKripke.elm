module Graph.GraphKripke exposing (..)

import Graph exposing (Graph)
import Pages.KripkeModel exposing (..)
import Debug exposing (todo)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- convert a Kripke model to a Graph
kripkeToGraph : KripkeModel -> Graph World ()
kripkeToGraph =
    todo "implement model to graph conversion"

-- Generate GraphViz DOT format from a graph
generateDot : Graph a b -> String
generateDot =
    todo "Implement GraphViz DOT generation from a graph"

-- Generate the SVG representation of a graph
generateSVG : KripkeModel -> List (Svg msg)
generateSVG model =
    todo "Implement SVG generation"