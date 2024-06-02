module GraphKripke exposing (getSvg)

-- -- import Pages.KripkeModel exposing (..)

import Color
import Dict
import Force
import Graph exposing (Edge, Graph, Node, fromNodesAndEdges)
import KripkeModel exposing (KripkeModel)
import Model exposing (World)
import TypedSvg exposing (circle, g, line, svg, text_, title)
import TypedSvg.Attributes as Attrs exposing (class, fill, fontSize, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, dx, dy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Paint(..))


fromKripkeModelToGraph : KripkeModel -> Graph String String
fromKripkeModelToGraph { relations, evaluations } =
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


getSvg : KripkeModel -> Svg msg
getSvg kripkeModel =
    let
        graph =
            fromKripkeModelToGraph
                kripkeModel

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
        [ viewBox -10 -10 150 150
        , Attrs.width <| Percent 100
        , Attrs.height <| Percent 50
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
