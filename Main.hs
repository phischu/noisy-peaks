module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG 
import Diagrams.TwoD
import Diagrams.Trail
import qualified Data.ByteString.Lazy as B (readFile)
import Data.Csv (decode,HasHeader(HasHeader))
import Data.Vector (Vector)
import qualified Data.Vector as V (map,toList,slice)

main :: IO ()
main = do
  d <- B.readFile "OFDP-FUTURE_C1.csv"
  let rows = either error id (decode HasHeader d) :: Vector (String,Double,Double,Double,Double,Double,Double)
      values = V.map (\(_,x,_,_,_,_,_) -> x) (V.slice 1000 1200 rows)
  renderSVG "output.svg" (mkSizeSpec (Just 2000) Nothing) (plotValues values)


plotValues :: Vector Double -> Diagram SVG R2
plotValues values = strokeLine (lineFromVertices (map p2 (zip [0..] (V.toList values))))

diagram :: Diagram SVG R2
diagram = strokeLine (lineFromVertices [p2 (0,0), p2 (1,1), p2 (3,1)])



