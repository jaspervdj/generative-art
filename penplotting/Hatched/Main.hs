{-# LANGUAGE TupleSections #-}
module Main (main) where

import Data.List (sortOn)
import qualified Graphics.PlotFont as PF
import Text.Printf

import Draw.Plotting
import Draw
import Geometry
import qualified Geometry.Shapes as Shapes (haskellLogo)

-- DIN A4
picWidth, picHeight :: Num a => a
picWidth = 297
picHeight = 210

haskellLogo :: [Polygon]
haskellLogo = transform (translate (Vec2 15 10) <> scale (picHeight - 20) <> mirrorAlong (angledLine (Vec2 0 0.5) (deg 0) 1)) Shapes.haskellLogo

main :: IO ()
main = do
    let settings = def
            { _feedrate = 30000
            , _zTravelHeight = 1
            , _canvasBoundingBox = Just (boundingBox [zero, Vec2 picWidth picHeight])
            }
    writeGCodeFile "hatching-pen-pressure.g" $ runPlot settings penPressure
    writeGCodeFile "hatching-density.g" $ runPlot settings hatchingDensity

penPressure :: Plot ()
penPressure = do
    let hatches = fmap (\poly -> hatch poly zero 1) haskellLogo
        hatchesWithPressure = concat $ zipWith (\p hs -> fmap (, p) hs) [2, 5, 10, 10] hatches
        sortedHatches = sortOn (\(Line (Vec2 _ y) _, _) -> y) $ sortOn (\(Line (Vec2 x _) _, _) -> x) hatchesWithPressure
    for_ sortedHatches $ \(Line p q, pressure) ->
        withDrawingHeight (-pressure) $ do
            repositionTo p
            lineTo q

hatchingDensity :: Plot ()
hatchingDensity = for_ (zip [ (x, y) | y <- [4, 3..0], x <- [0..4]] [0.1, 0.2 :: Double ..]) $ \((x, y), density) -> do
    let strokes = Polyline . fmap (uncurry Vec2) <$> PF.render' PF.canvastextFont (printf "%.1f" density)
        origin = Vec2 (x * 30) (y * 40)
    for_ strokes $ plot . transform (translate origin <> translate (Vec2 0 21) <> scale 0.2)
    let box = transform (translate origin) (boundingBoxPolygon (boundingBox [zero, Vec2 20 20]))
        hatches = zigzag (hatch box (deg 0) density)
    plot hatches
  where
    zigzag = Polyline . go
      where
        go [] = []
        go [Line a b] = [a, b]
        go (Line a b : Line c d : ls) = a : b : d : c : go ls
