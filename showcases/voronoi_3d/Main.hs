module Main (main) where



import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Math.Noise               (Perlin (..), getValue, perlin)
import           Prelude                  hiding ((**))
import           System.Random.MWC

import Draw
import Geometry                     as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

scaleFactor :: Double
scaleFactor = 1

main :: IO ()
main = do
    let file = "out/voronoi_3d.svg"
        count = 500
        scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)

    gen <- initialize (V.fromList [12, 984, 498, 498, 626, 15, 165])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = 1440 * sqrt (0.75 / count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox [zero, Vec2 1440 1440]
            , _poissonRadius = adaptiveRadius
            , _poissonK      = 4
            }

    points <- poissonDisc gen samplingProps
    print (length points)
    let voronoi = toVoronoi (lloydRelaxation 4 (bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 1440 1440)) points))
        voronoiWithProps = mapWithMetadata (\p _ _ -> (randomColor p, randomHeight p)) voronoi
        origin = Vec2 (picWidth/2) (picHeight/2)
        voronoiCells = sortOn ((\(Polygon ps) -> minimum (yCoordinate <$> ps)) . fst) $
            ( \c ->
                ( G.transform
                    (  G.translate (Vec2 0 (picHeight/5))
                    <> G.scaleAround' origin 1 0.35
                    <> G.rotateAround origin (deg 45)
                    <> G.translate (Vec2 560 0 )
                    <> G.scaleAround (_voronoiSeed c) 0.9 )
                    (_voronoiRegion c)
                , _voronoiProps c) )
            <$> _voronoiCells voronoiWithProps

    render file scaledWidth scaledHeight $ do
        cairoScope (setColor (magma 0.05) >> paint)
        for_ voronoiCells $ uncurry drawCell

randomHeight :: Vec2 -> Double
randomHeight = \p -> 300 + 400 * noise2d p + 200 * exp(- 0.000005 * normSquare (p -. origin))
  where
    noise = perlin { perlinOctaves = 4, perlinFrequency = 0.001, perlinSeed = 1980166 }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)
    origin = Vec2 720 720

randomColor :: Vec2 -> Color Double
randomColor = \p -> inferno (0.6 + 0.35 * noise2d p)
  where
    noise = perlin { perlinOctaves = 5, perlinFrequency = 0.001, perlinPersistence = 0.65, perlinSeed = 1980166 }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)

drawCell :: Polygon -> (Color Double, Double) -> Render ()
drawCell (Polygon []) _ = pure ()
drawCell poly@(Polygon ps) (color, height) = cairoScope $ do
    let lineColor = blend 0.95 color white
        sideColor = blend 0.1 (color `withOpacity` 0.8) (black `withOpacity` 0.3)
        topColor = blend 0.7 (color `withOpacity` 0.8) (black `withOpacity` 0.3)

    C.setLineJoin C.LineJoinBevel

    for_ (zip normalizedPoints (drop 1 (cycle normalizedPoints))) $ \(p, q) -> cairoScope $ do
        moveToVec p
        lineToVec q
        lineToVec (G.transform (G.translate (Vec2 0 (-height))) q)
        lineToVec (G.transform (G.translate (Vec2 0 (-height))) p)
        setColor (dissolve 0.8 sideColor)
        fillPreserve
        setColor lineColor
        setLineWidth 2
        stroke

    cairoScope $ do
        C.translate 0 (-height)
        sketch poly
        setColor topColor
        fillPreserve
        setColor lineColor
        setLineWidth 2
        stroke

  where
    leftmost = minimumBy (comparing xCoordinate) ps
    normalizedPoints = rotateUntil (== leftmost) ps

rotateUntil :: (a -> Bool) -> [a] -> [a]
rotateUntil p xs = zipWith
    (flip const)
    xs
    (dropWhile (not . p) (cycle xs))

xCoordinate, yCoordinate :: Vec2 -> Double
xCoordinate (Vec2 x _) = x
yCoordinate (Vec2 _ y) = y
