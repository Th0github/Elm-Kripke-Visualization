module GraphKripke exposing (getSvg)

-- -- import Pages.KripkeModel exposing (..)

import Color
import Debug exposing (todo)
import Dict
import Force
import Graph exposing (Edge, Graph, Node, fromNodesAndEdges)
import KripkeModel exposing (KripkeModel)
import Model exposing (Model, World)
import TypedSvg exposing (circle, defs, g, line, svg, text_, title)
import TypedSvg.Attributes as Attrs exposing (class, fill, fontSize, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, dx, dy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Paint(..))



-- convert a Kripke model to a Graph


muddy : KripkeModel
muddy =
    { worlds = [ 0, 1, 2, 3, 4, 5, 6, 7 ]
    , relations =
        [ { agentName = "1", worldRelations = [ [ 0, 4 ], [ 2, 6 ], [ 3, 7 ], [ 1, 5 ] ] }
        , { agentName = "2", worldRelations = [ [ 0, 2 ], [ 4, 6 ], [ 5, 7 ], [ 1, 3 ] ] }
        , { agentName = "3", worldRelations = [ [ 0, 1 ], [ 4, 5 ], [ 6, 7 ], [ 2, 3 ] ] }
        ]
    , evaluations =
        [ { world = 0, propositions = [] }
        , { world = 1, propositions = [ 3 ] }
        , { world = 2, propositions = [ 2 ] }
        , { world = 3, propositions = [ 2, 3 ] }
        , { world = 4, propositions = [ 1 ] }
        , { world = 5, propositions = [ 1, 3 ] }
        , { world = 6, propositions = [ 1, 2 ] }
        , { world = 7, propositions = [ 1, 2, 3 ] }
        ]
    }



-- [0, 1, 2, 3, 4, 5, 6, 7]
--
--     [
--     ]


fromKripkeModelToGraph : KripkeModel -> Graph String String
fromKripkeModelToGraph { worlds, relations, evaluations } =
    let
        combineWorlds : String -> List World -> List (Edge String)
        combineWorlds label xs =
            List.concatMap
                (\e1 ->
                    List.map (\e2 -> Edge e1 e2 label) xs
                )
                xs
    in
    fromNodesAndEdges
        (List.map
            (\{ propositions, world } -> Node world (String.join "," <| List.map String.fromInt propositions))
            evaluations
        )
        (List.concatMap (\{ agentName, worldRelations } -> List.concatMap (combineWorlds agentName) worldRelations) relations)


getSvg : Svg msg
getSvg =
    let
        graph =
            fromKripkeModelToGraph
                muddy

        links =
            List.map (\lnk -> ( lnk.from, lnk.to )) <| Graph.edges graph

        simulation =
            Force.simulation
                [ Force.links links
                , Force.manyBody <| List.map .id <| Graph.nodes graph
                , Force.center (100 / 2) (100 / 2)
                ]

        nodes =
            List.map (\node -> Force.entity node.id node.label) (Graph.nodes graph)
                |> Force.computeSimulation simulation

        dict =
            Dict.fromList (List.map (\ent -> ( ent.id, ent )) nodes)

        edges =
            List.filterMap (\( from, to ) -> Maybe.map2 Tuple.pair (Dict.get from dict) (Dict.get to dict))
                links
    in
    svg
        [ viewBox -10 -10 200 150
        , Attrs.width <| Percent 50
        , Attrs.height <| Percent 100
        ]
        [ edges
            |> List.map
                (\( source, target ) ->
                    line
                        [ strokeWidth 1
                        , stroke <| Paint <| Color.gray
                        , x1 source.x
                        , y1 source.y
                        , x2 target.x
                        , y2 target.y
                        ]
                        []
                )
            |> g [ class [ "links" ] ]
        , nodes
            |> List.map
                (\node ->
                    g [ class [ "node" ] ]
                        [ circle
                            [ r 8
                            , strokeWidth 1
                            , fill (Paint Color.white)
                            , stroke (Paint Color.black)
                            , cx node.x
                            , cy node.y

                            -- The coordinates are initialized and updated by `Force.simulation`
                            -- and `Force.tick`, respectively.
                            -- Add event handler for starting a drag on the node.
                            ]
                            [ title [] [ text node.value ]
                            ]
                        , text_
                            [ -- Align text label at the center of the circle.\
                              dx <| node.x
                            , dy <| node.y + 2
                            , Attrs.alignmentBaseline
                                AlignmentMiddle
                            , Attrs.textAnchor AnchorMiddle

                            -- styling
                            , fontSize <| Px 5
                            , fill (Paint Color.black)
                            ]
                            [ text node.value ]
                        ]
                )
            |> g [ class [ "nodes" ] ]
        ]



-- -- Generate GraphViz DOT format from a graph
-- generateDot : Graph a b -> String
-- generateDot =
--     todo "Implement GraphViz DOT generation from a graph"
-- -- Generate the SVG representation of a graph
-- generateSVG : KripkeModel -> List (Svg msg)
-- generateSVG model =
--     todo "Implement SVG generation"
