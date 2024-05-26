module Graph.GraphKripke exposing (kripkeToGraph)

-- -- import Pages.KripkeModel exposing (..)

import Debug exposing (todo)
import Graph exposing (Graph)
import Model exposing (Model, World)
import Svg exposing (..)



-- import Svg.Attributes exposing (..)
-- convert a Kripke model to a Graph


kripkeToGraph : Model -> Graph World ()
kripkeToGraph =
    todo "implement model to graph conversion"



-- -- Generate GraphViz DOT format from a graph
-- generateDot : Graph a b -> String
-- generateDot =
--     todo "Implement GraphViz DOT generation from a graph"
-- -- Generate the SVG representation of a graph
-- generateSVG : KripkeModel -> List (Svg msg)
-- generateSVG model =
--     todo "Implement SVG generation"
