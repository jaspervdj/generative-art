-- | <<docs/apollonian_gasket/spaced_gasket.svg>>
module Geometry.Processes.ApollonianGasket (
    -- * The classical shape
      createGasket
    , fifthCircle
    , Circle(..)

    -- * For playing around
    , ApoCircle(..)
    , newCircle
    , toCircle
    , toApoCircle
) where



import Data.Complex
import Data.Tree

import Geometry.Core as G



-- | A circle, but in a format more suitable for Descartes’ Theorem, which the
-- gasket algorithm is based on.
--
-- It interprets the center @'Vec2' x y@ as a complex number \(x + \mathrm i\,y\),
-- and stores the curvature \(k=\frac1r\) instead of the radius \(r\).
--
-- 'Circle' and 'ApoCircle' can be freely converted between using 'toCircle' and
-- 'toApoCircle'.
data ApoCircle = ApoCircle (Complex Double) Double
    deriving (Eq, Show)

instance HasBoundingBox ApoCircle where
    boundingBox = boundingBox . toCircle

-- | Simple conversion function. Inverse of 'toApoCircle'.
toCircle :: ApoCircle -> Circle
toCircle (ApoCircle (x :+ y) k) = Circle (Vec2 x y) (1/k)

-- | Simple conversion function. Inverse of 'toCircle'.
toApoCircle :: Circle -> ApoCircle
toApoCircle (Circle (Vec2 x y) r) = ApoCircle (x :+ y) (1/r)

-- | Curvature of the new Apollonian circle.
newCurvature
    :: (Double -> Double -> Double)
    -> Double -- ^ Curvature of first circle
    -> Double -- ^ Second circle
    -> Double -- ^ Third circle
    -> Double -- ^ New circle
newCurvature (+-) k1 k2 k3 =
    -- Taken straight from Wikipedia: https://en.wikipedia.org/wiki/Descartes%27_theorem
    (k1 + k2 + k3) +- (2 * sqrt (k1*k2 + k2*k3 + k3*k1))

-- | Center of the new Apollonian circle.
newCenter
    :: (Complex Double -> Complex Double -> Complex Double) -- ^ Plus or minus to yield both circles
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
    -> Double         -- ^ Curvature of the new circle
    -> Complex Double -- ^ Center of the new circle
newCenter (+-) (ApoCircle c1 k1) (ApoCircle c2 k2) (ApoCircle c3 k3) k4 =
    -- Taken straight from Wikipedia: https://en.wikipedia.org/wiki/Descartes%27_theorem
    1/k4' * ( (c1*k1' + c2*k2' + c3*k3')
              +- (2*sqrt (k1'*k2'*c1*c2 + k2'*k3'*c2*c3 + k1'*k3'*c1*c3)))
  where
    [k1', k2', k3', k4'] = map (:+ 0) [k1, k2, k3, k4]

-- | Create a new Apollonian circle, based on three existing ones. This is the raw
-- application of Descartes’ theorem. For something with less pitfalls, use
-- 'fifthCircle'.
--
-- The choice of sign is a mystery to me. For the Apollonian Gasket, the rule is:
--
--   1. The large circle is obtained using \((-,-)\) on the three initial cycles
--   2. The bottom-left circle is obtained using \((+,-)\) on the initial top, initial bottom, and large circle
--   3. All other recursive steps use \((+,+)\)
--   4. If you can explain the \((+,-)\), please contact me, I’m dying to know
--
-- If you’re wondering, this is what you get when you don’t do the \((+,-)\) trick:
--
-- <<docs/apollonian_gasket/missing_the_minus.svg>>
newCircle
    :: (Double -> Double -> Double)
        -- ^ Plus (larger curvature, smaller circle) or minus (smaller curvature, larger circle).
    -> (Complex Double -> Complex Double -> Complex Double) -- ^ Plus or minus; see this function’s doc.
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
newCircle plusMinusCurvature plusMinusCenter circ1@(ApoCircle _ k1) circ2@(ApoCircle _ k2) circ3@(ApoCircle _ k3) =
    let k4 = newCurvature plusMinusCurvature k1 k2 k3
        c4 = newCenter plusMinusCenter circ1 circ2 circ3 k4
    in ApoCircle c4 k4

-- | Given four mutually tangent circles, find »the other one« than the first argument.
-- This resolves the »\((+,-)\)« ambiguity of 'newCircle'.
fifthCircle :: ApoCircle -> ApoCircle -> ApoCircle -> ApoCircle -> ApoCircle
fifthCircle (ApoCircle c1 k1) (ApoCircle c2 k2) (ApoCircle c3 k3) (ApoCircle c4 k4) =
    let [k1', k2', k3', k4', k5'] = map (:+ 0) [k1, k2, k3, k4, k5]
        k5 = 2*(k2+k3+k4) - k1
        c5 = (2*(k2'*c2+k3'*c3+k4'*c4) - k1'*c1) / k5'
    in ApoCircle c5 k5

-- | The simple workhorse function. Given three mutually touching, equally sized
-- circles, it calculates the classical Apollonian Gasket:
--
--
-- <<docs/apollonian_gasket/classical_gasket.svg>>
--
-- Be warned though, the number of circles grows exponentially with recursion depth
-- (controlled with the radius parameter). This can easily melt your RAM, CPU or
-- harddrive if you aren’t careful.
--
-- It’s quite fun to play with the parameters though! Here is what happens when the three initial circles
-- aren’t touching:
--
-- <<docs/apollonian_gasket/forgetting_gen0.svg>>
--
-- To get even more creative freedom, I like to copy this algorithm and see what I
-- get, e.g. by commenting out one of the recursions.
createGasket
    :: Double -- ^ Radius threshold: do not recurse to circles smaller than this. Smaller values yield more detail.
    -> Circle -- ^ Initial left circle
    -> Circle -- ^ Initial right circle
    -> Circle -- ^ Initial bottom circle
    -> Tree Circle
createGasket minRadius gen0LCirc gen0RCirc gen0BCirc =
    let gen0L = toApoCircle gen0LCirc
        gen0R = toApoCircle gen0RCirc
        gen0B = toApoCircle gen0BCirc

        large = newCircle (-) (-) gen0L gen0R gen0B
        small = newCircle (+) (+) gen0L gen0R gen0B

        recurse :: ApoCircle -> ApoCircle -> ApoCircle -> ApoCircle -> Tree Circle
        recurse cA c1 c2 c3 =
            let cB@(ApoCircle _ kB) = fifthCircle cA c1 c2 c3
            in Node (toCircle cB)
                (if abs kB < 1/minRadius
                    then
                        [ recurse c3 cB c1 c2
                        , recurse c2 c3 cB c1
                        , recurse c1 c2 c3 cB
                        ]
                    else [])

        circles :: Tree Circle
        circles = Node (toCircle large)
            [ recurse small gen0L gen0R gen0B
            , recurse large gen0L gen0R gen0B
            ]
    in circles
