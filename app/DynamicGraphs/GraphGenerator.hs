{-# LANGUAGE OverloadedStrings #-}

module DynamicGraphs.GraphGenerator
  (sampleGraph)
  where

import Data.GraphViz.Attributes as A
import Data.GraphViz.Attributes.Complete as AC
import Data.GraphViz.Types.Generalised (
  DotGraph(..),
  DotStatement(..),
  GlobalAttributes(..))
import Database.Requirement (Req(..))
import Data.Sequence as Seq
import Data.Text.Lazy (Text)


buildGraph :: [DotStatement Text] -> DotGraph Text
buildGraph statements = DotGraph {
    strictGraph = False,
    directedGraph = True,
    graphID = Nothing,
    graphStatements = Seq.fromList $ [
        GA graphAttributes,
        GA nodeAttributes,
        GA edgeAttributes
        ] ++ statements
    }


graphAttributes :: GlobalAttributes
graphAttributes = GraphAttrs [AC.RankDir AC.FromLeft]

nodeAttributes :: GlobalAttributes
nodeAttributes = NodeAttrs [A.shape A.Circle, AC.Width 1, A.style A.filled]

edgeAttributes :: GlobalAttributes
edgeAttributes = EdgeAttrs []


reqsToGraph :: [Req] -> DotGraph Text
reqsToGraph reqs =
    let stmts = concatMap reqToStmts reqs
    in
        buildGraph stmts


reqToStmts :: Req -> [DotStatement Text]
reqToStmts _ = []

sampleGraph = reqsToGraph []

-- ex3 :: G.DotGraph Text
-- ex3 = digraph (Str "ex3") $ do
--
--     graphAttrs [RankDir FromLeft]
--
--     cluster (Str "G1") $ do
--         nodeAttrs               [shape Circle, Width 1, style filled, myColor 1]
--         node "A"                [textLabel "MAT237"]
--
--     cluster (Str "G2") $ do
--         nodeAttrs               [shape Circle, Width 1, style filled, myColor 1]
--         node "B"                [textLabel "STA261"]
--
--     cluster (Str "G3") $ do
--         nodeAttrs               [shape Circle, Width 1, style filled, myColor 1]
--         node "C"                [textLabel "MAT224"]
--
--     cluster (Str "G4") $ do
--         nodeAttrs               [shape Circle, Width 1, style filled, myColor 2]
--         node "D"                [textLabel "MAT137"]
--
--     cluster (Str "G5") $ do
--         nodeAttrs               [shape Circle, Width 1, style filled, myColor 2]
--         node "E"                [textLabel "MAT157"]
--
--     cluster (Str "G6") $ do
--         nodeAttrs               [shape Circle, Width 1, style filled, myColor 3]
--         node "F"                [textLabel "STA257"]
--
--     cluster (Str "G0") $ do
--         nodeAttrs               [shape Circle, Width 1, style filled, myColor 4]
--         node "ZZ"               [textLabel "STA302"]
--
--     ("A" :: Text)             --> ("ZZ" :: Text)
--     ("B" :: Text)              --> ("ZZ" :: Text)
--     ("C" :: Text)              --> ("ZZ" :: Text)
--     ("D" :: Text)              --> ("Or" :: Text)
--     ("E" :: Text)              --> ("Or" :: Text)
--     ("Or" :: Text)             --> ("A" :: Text)
--     ("F" :: Text)              --> ("B" :: Text)
