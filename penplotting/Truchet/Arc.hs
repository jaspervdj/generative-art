module Arc where



import Control.Monad (guard)
import Control.Monad.State.Class
import Data.List (sortOn)
import Data.Ord (comparing)
import qualified Graphics.Rendering.Cairo as C

import Draw
import Draw.Plotting
import Geometry
import Geometry.Shapes



data Arc
    = CwArc Vec2 Vec2 Vec2
    | CcwArc Vec2 Vec2 Vec2
    | Straight Vec2 Vec2
    deriving (Eq, Ord, Show)

arcStartEnd :: Arc -> (Vec2, Vec2)
arcStartEnd = \case
    CwArc _ start end -> (start, end)
    CcwArc _ start end -> (start, end)
    Straight start end -> (start, end)

reverseArc :: Arc -> Arc
reverseArc = \case
    CwArc center start end -> CcwArc center end start
    CcwArc center start end -> CwArc center end start
    Straight start end -> Straight end start

cwArc :: Vec2 -> Double -> Angle -> Angle -> Arc
cwArc center radius startAngle endAngle = CwArc center start end
  where
    start = center +. polar startAngle radius
    end = center +. polar endAngle radius

ccwArc :: Vec2 -> Double -> Angle -> Angle -> Arc
ccwArc center radius startAngle endAngle = CcwArc center start end
  where
    start = center +. polar startAngle radius
    end = center +. polar endAngle radius

straight :: Vec2 -> Vec2 -> Arc
straight start end = Straight start end

-- Valid for translation, rotation and aspect-preserving scaling,
-- but breaks down for mirroring and not-aspect-preserving scaling.
instance Transform Arc where
    transform t (CwArc center start end) = CwArc (transform t center) (transform t start) (transform t end)
    transform t (CcwArc center start end) = CcwArc (transform t center) (transform t start) (transform t end)
    transform t (Straight start end) = Straight (transform t start) (transform t end)

instance Sketch Arc where
    sketch (CwArc center start end) = do
        let radius = norm (start -. center)
            startAngle = angleOfLine (Line center start)
            endAngle = angleOfLine (Line center end)
        arcSketchNegative center radius startAngle endAngle
    sketch (CcwArc center start end) = do
        let radius = norm (start -. center)
            startAngle = angleOfLine (Line center start)
            endAngle = angleOfLine (Line center end)
        arcSketch center radius startAngle endAngle
    sketch (Straight start end) = sketch (Line start end)

instance Plotting Arc where
    plot (CwArc center start end) = do
        pos <- gets _penXY
        case norm (pos -. start) of
            0 -> pure ()
            d | d < 0.1 -> lineTo start
            _otherwise -> repositionTo start
        clockwiseArcAroundTo center end
    plot (CcwArc center start end) = do
        pos <- gets _penXY
        case norm (pos -. start) of
            0 -> pure ()
            d | d < 0.1 -> lineTo start
            _otherwise -> repositionTo start
        counterclockwiseArcAroundTo center end
    plot (Straight start end) = do
        pos <- gets _penXY
        case norm (pos -. start) of
            d | d < 0.1 -> lineTo end
            _otherwise -> repositionTo start >> lineTo end

-- | Clip an arc with a polygon mask
--
-- The algorithm approximates the arc with a polyline, so it's probably not
-- suited for computations, but should be fine for drawing/plotting purposes.
--
-- <<docs/haddock/Arc.hs/clipping_example.svg>>
clipArc :: Polygon -> Arc -> [Arc]
clipArc mask (Straight start end) =
    [ Straight a b
    | (Line a b, LineInsidePolygon) <- clipPolygonWithLineSegment mask (Line start end)
    ]
clipArc mask arc@CwArc{} = reverse (reverseArc <$> clipArc mask (reverseArc arc))
clipArc mask (CcwArc center start end) = reconstructArcs $ sortOn (getRad . angleOf . fst) (startEndPoint ++ intersectionPoints)
  where
    tolerance = 0.001
    startAngle = angleOf start
    endAngle = angleOf end
    maskEdges = polygonEdges mask
    radius = norm (start -. center)
    approximateCircle = transform (translate center <> scale radius) regularPolygon 32
    startEndPoint = concat
        [ [(start, Entering) | start `pointInPolygon` mask]
        , [(end,   Exiting)  | end   `pointInPolygon` mask]
        ]
    intersectionPoints = do
        edge <- polygonEdges approximateCircle
        (Line a' b', LineInsidePolygon) <- clipPolygonWithLineSegment mask edge
        (approximateIntersectionPoint, intersectionClass) <- [(a', Entering), (b', Exiting)]
        guard (any (\maskEdge -> distanceFromLine approximateIntersectionPoint maskEdge <= tolerance) maskEdges)
        let refinedIntersectionPoint = newton approximateIntersectionPoint
            alpha = angleOf refinedIntersectionPoint
        guard (getRad alpha >= getRad startAngle - 2 * pi * tolerance && getRad alpha <= getRad endAngle + 2 * pi * tolerance)
        pure (refinedIntersectionPoint, intersectionClass)
    reconstructArcs = \case
        [] -> []
        (p, Entering) : (q, Exiting) : rest -> CcwArc center p q : reconstructArcs rest
        xs -> error ("Could not reconstruct arcs: " ++ unlines (show <$> xs))
    newton :: Vec2 -> Vec2
    newton p =
        let edge = minimumBy (comparing (distanceFromLine p)) maskEdges
            deviation = radius - norm (center -. p)
            angle = angleBetween edge (Line center p)
        in  if deviation < tolerance * radius
                then p
                else newton (p +. deviation *. direction edge /. cos (getRad angle))
    angleOf = \p -> normalizeAngle alpha0 (angleOfLine (Line center p))
      where alpha0 = normalizeAngle zero (angleOfLine (Line center start))

-- Positive mathematical orientation
data IntersectionPointClass = Entering | Exiting deriving (Eq, Show)

clipPolygonWithLineSegment :: Polygon -> Line -> [(Line, LineType)]
clipPolygonWithLineSegment polygon scissors@(Line start end) = reconstructSegments sortedPoints
  where
    allIntersectionPoints =
        [ p
        | edge <- polygonEdges polygon
        , IntersectionReal p <- pure (intersectionLL edge scissors)
        ]
    sortedPoints = sortOn (\p -> direction scissors `dotProduct` (p -. start)) ([start, end] ++ allIntersectionPoints)
    reconstructSegments = \case
        [] -> []
        [_] -> []
        a : b : xs ->
            let segment = Line a b
                lineType = if ((a +. b) /. 2) `pointInPolygon` polygon
                    then LineInsidePolygon
                    else LineOutsidePolygon
            in  (segment, lineType) : reconstructSegments (b : xs)

-- Test pic for clipping
-- Run in GHCI:
-- > stack ghci generative-art penplotting-truchet
-- > arcsClippingExample
arcsClippingExample :: IO ()
arcsClippingExample = do
    let arcs =
            [ arcType (center +. start) (center +. end)
            | center <- [Vec2 1 1, Vec2 (-40) (-40), Vec2 (-40) 40, Vec2 40 40, Vec2 40 (-40)]
            , (start, end) <- [(Vec2 1 20, Vec2 1 (-20)), (Vec2 (-30) 30, Vec2 30 (-30))]
            , arcType <- [CwArc center, CcwArc center, Straight]
            ]
        mask = transform (scale 60) $ regularPolygon 6
    haddockRender "Arc.hs/clipping_example.svg" 200 200 $ do
        coordinateSystem (MathStandard_ZeroCenter_XRight_YUp 200 200)
        sketch mask
        setColor (mathematica97 1)
        C.stroke
        for_ arcs $ \arc -> do
            let color = case arc of
                    CcwArc{}   -> mathematica97 2
                    CwArc{}    -> mathematica97 3
                    Straight{} -> mathematica97 4
            sketch arc
            setColor (color `withOpacity` 0.2)
            C.stroke
            for_ (clipArc mask arc) sketch
            setColor color
            C.stroke
