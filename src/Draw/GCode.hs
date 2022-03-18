{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw.GCode (
    renderGCode
) where



import           Data.Foldable
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Formatting     hiding (center)
import           Geometry.Core



decimal :: Format r (Double -> r)
decimal = fixed 3

draw :: GCode -> GCode
draw content = GBlock
    [ G91_RelativeMovement
    , G00_LinearRapidMovement Nothing Nothing (Just (-0.5))
    , content
    , G91_RelativeMovement
    , G00_LinearRapidMovement Nothing Nothing (Just 0.5)]

data GCode
    = GComment Text
    | GBlock [GCode]
    | F_Feedrate Double

    | G00_LinearRapidMovement (Maybe Double) (Maybe Double) (Maybe Double) -- ^ X, Y, Z
    | G01_LinearMovement (Maybe Double) (Maybe Double) (Maybe Double) -- ^ X, Y, Z
    | G02_ArcClockwise Double Double Double Double -- ^ I,J ; X,Y
    | G03_ArcCounterClockwise Double Double Double Double -- ^ I,J ; X,Y
    | G90_AbsoluteMovement
    | G91_RelativeMovement

renderGCode :: GCode -> Text
renderGCode = \case
    GComment comment -> "; " <> comment
    GBlock content   -> T.unlines (fmap renderGCode content)
    F_Feedrate f     -> format ("F" % decimal) f

    G00_LinearRapidMovement Nothing Nothing Nothing -> mempty
    G00_LinearRapidMovement x y z                   -> format ("G0" % optioned (" X"%decimal) % optioned (" Y"%decimal) % optioned (" Z"%decimal)) x y z

    G01_LinearMovement Nothing Nothing Nothing -> mempty
    G01_LinearMovement x y z                   -> format ("G1" % optioned (" X"%decimal) % optioned (" Y"%decimal) % optioned (" Z"%decimal)) x y z

    G02_ArcClockwise i j x y                      -> format ("G2 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j
    G03_ArcCounterClockwise i j x y               -> format ("G3 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j

    G90_AbsoluteMovement                          -> "G90 ; abs0lute movement"
    G91_RelativeMovement                          -> "G91 ; re1ative movement"

class ToGCode a where
    toGCode :: a -> GCode

instance ToGCode Circle where
    toGCode (Circle (Vec2 x y) r) =
        let (startX, startY) = (x-r, y)
        in GBlock
            [ GComment "Circle"
            , G00_LinearRapidMovement (Just startX) (Just startY) Nothing
            , draw (GBlock
                [ G90_AbsoluteMovement
                , G02_ArcClockwise r 0 startX startY
                ])
            ]

instance {-# OVERLAPPING #-} Sequential f => ToGCode (f Vec2) where
    toGCode = go . toList
      where
        go [] = GBlock []
        go (Vec2 startX startY : points) = GBlock
            [ GComment "Polyline"
            , G90_AbsoluteMovement
            , G00_LinearRapidMovement (Just startX) (Just startY) Nothing
            , draw (GBlock [ G01_LinearMovement (Just x) (Just y) Nothing | Vec2 x y <- points])
            ]

instance {-# OVERLAPPABLE #-} (Functor f, Sequential f, ToGCode a) => ToGCode (f a) where
    toGCode x = GBlock (GComment "Sequential" : toList (fmap toGCode x))

instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b) => ToGCode (a,b) where
    toGCode (a,b) = GBlock [GComment "2-tuple", toGCode a, toGCode b]

instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b, ToGCode c) => ToGCode (a,b,c) where
    toGCode (a,b,c) = GBlock [GComment "3-tuple", toGCode a, toGCode b, toGCode c]

instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b, ToGCode c, ToGCode d) => ToGCode (a,b,c,d) where
    toGCode (a,b,c,d) = GBlock [GComment "4-tuple", toGCode a, toGCode b, toGCode c, toGCode d]

instance {-# OVERLAPPING #-} (ToGCode a, ToGCode b, ToGCode c, ToGCode d, ToGCode e) => ToGCode (a,b,c,d,e) where
    toGCode (a,b,c,d,e) = GBlock [GComment "5-tuple", toGCode a, toGCode b, toGCode c, toGCode d, toGCode e]
