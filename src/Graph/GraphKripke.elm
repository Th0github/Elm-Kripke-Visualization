module Graph.GraphKripke exposing (..)

import Graph exposing (Graph)
import Kripke.KripkeModel exposing (..)

-- convert a Kripke model to a Graph
kripkeToGraph : KripkeModel -> Graph World ()
kripkeToGraph (KrM worlds _ relations) =    
    List.foldl (\(from, to) g -> Graph.Edge from to () g)
        (Graph.Node worlds)
        relations

-- Generate GraphViz DOT format from a graph
generateDot : Graph World () -> String
generateDot graph =
    let
        config = undefined
    in
    toDot config
        (\world -> String.fromInt world)
        (\_ -> "")
        graph