module Kripke.KripkeModel exposing (..)

type alias World = Int
type alias Proposition = Int

type alias Relation = List (World, World)
type alias Valuation = World -> List Proposition

type KripkeModel = 
    KrM (List World) Valuation Relation

valuationFunction : Valuation
valuationFunction world = 
    case world of
        0 -> [1, 2]
        1 -> [1]
        2 -> [2]
        _ -> []