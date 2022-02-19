-- | Adaptation of https://weber.itn.liu.se/~stegu/simplexnoise/SimplexNoise.java
--
-- Based on example code by Stefan Gustavson (stegu@itn.liu.se).
-- Optimisations by Peter Eastman (peastman@drizzle.stanford.edu).
-- Better rank ordering method for 4D by Stefan Gustavson in 2012.
--
-- This code was placed in the public domain by its original author,
-- Stefan Gustavson. You may use it as you see fit, but
-- attribution is appreciated.
module Geometry.Algorithms.SimplexNoise (
      noise2
    , noise3
) where



import qualified Data.Vector as V
import  Data.Vector (Vector, (!))
import Data.Bits



-- | To speed up gradient calculations
data Grad3 = Grad3 !Double !Double !Double
    deriving (Eq, Ord, Show)

-- | To speed up gradient calculations
data Grad4 = Grad4 !Double !Double !Double !Double
    deriving (Eq, Ord, Show)



--   private static Grad grad3[] = {new Grad(1,1,0),new Grad(-1,1,0),new Grad(1,-1,0),new Grad(-1,-1,0),
--                                  new Grad(1,0,1),new Grad(-1,0,1),new Grad(1,0,-1),new Grad(-1,0,-1),
--                                  new Grad(0,1,1),new Grad(0,-1,1),new Grad(0,1,-1),new Grad(0,-1,-1)};

grad3 :: Vector Grad3
grad3 = V.fromList
    [ Grad3 1    1    0
    , Grad3 (-1) 1    0
    , Grad3 1    (-1) 0
    , Grad3 (-1) (-1) 0
    , Grad3 1    0    1
    , Grad3 (-1) 0    1
    , Grad3 1    0    (-1)
    , Grad3 (-1) 0    (-1)
    , Grad3 0    1    1
    , Grad3 0    (-1) 1
    , Grad3 0    1    (-1)
    , Grad3 0    (-1) (-1)
    ]

--   private static Grad grad4[]= {new Grad(0,1,1,1),new Grad(0,1,1,-1),new Grad(0,1,-1,1),new Grad(0,1,-1,-1),
--                    new Grad(0,-1,1,1),new Grad(0,-1,1,-1),new Grad(0,-1,-1,1),new Grad(0,-1,-1,-1),
--                    new Grad(1,0,1,1),new Grad(1,0,1,-1),new Grad(1,0,-1,1),new Grad(1,0,-1,-1),
--                    new Grad(-1,0,1,1),new Grad(-1,0,1,-1),new Grad(-1,0,-1,1),new Grad(-1,0,-1,-1),
--                    new Grad(1,1,0,1),new Grad(1,1,0,-1),new Grad(1,-1,0,1),new Grad(1,-1,0,-1),
--                    new Grad(-1,1,0,1),new Grad(-1,1,0,-1),new Grad(-1,-1,0,1),new Grad(-1,-1,0,-1),
--                    new Grad(1,1,1,0),new Grad(1,1,-1,0),new Grad(1,-1,1,0),new Grad(1,-1,-1,0),
--                    new Grad(-1,1,1,0),new Grad(-1,1,-1,0),new Grad(-1,-1,1,0),new Grad(-1,-1,-1,0)};

grad4 :: Vector Grad4
grad4 = V.fromList
    [ Grad4 0    1    1    1
    , Grad4 0    1    1    (-1)
    , Grad4 0    1    (-1) 1
    , Grad4 0    1    (-1) (-1)
    , Grad4 0    (-1) 1    1
    , Grad4 0    (-1) 1    (-1)
    , Grad4 0    (-1) (-1) 1
    , Grad4 0    (-1) (-1) (-1)
    , Grad4 1    0    1    1
    , Grad4 1    0    1    (-1)
    , Grad4 1    0    (-1) 1
    , Grad4 1    0    (-1) (-1)
    , Grad4 (-1) 0    1    1
    , Grad4 (-1) 0    1    (-1)
    , Grad4 (-1) 0    (-1) 1
    , Grad4 (-1) 0    (-1) (-1)
    , Grad4 1    1    0    1
    , Grad4 1    1    0    (-1)
    , Grad4 1    (-1) 0    1
    , Grad4 1    (-1) 0    (-1)
    , Grad4 (-1) 1    0    1
    , Grad4 (-1) 1    0    (-1)
    , Grad4 (-1) (-1) 0    1
    , Grad4 (-1) (-1) 0    (-1)
    , Grad4 1    1    1    0
    , Grad4 1    1    (-1) 0
    , Grad4 1    (-1) 1    0
    , Grad4 1    (-1) (-1) 0
    , Grad4 (-1) 1    1    0
    , Grad4 (-1) 1    (-1) 0
    , Grad4 (-1) (-1) 1    0
    , Grad4 (-1) (-1) (-1) 0
    ]

--   private static short p[] = {151,160,137,91,90,15,
--   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
--   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
--   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
--   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
--   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
--   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
--   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
--   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
--   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
--   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
--   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
--   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180};
--   // To remove the need for index wrapping, double the permutation table length
--   private static short perm[] = new short[512];
--   private static short permMod12[] = new short[512];
--   static {
--     for(int i=0; i<512; i++)
--     {
--       perm[i]=p[i & 255];
--       permMod12[i] = (short)(perm[i] % 12);
--     }
--   }

perm :: Vector Int
perm = fmap (.&. 255) (p <> p)
  -- To remove the need for index wrapping, double the permutation table length
  where
    p = V.fromList
        [ 151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
        140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247,
        120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177,
        33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71,
        134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133,
        230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161,
        1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130,
        116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250,
        124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227,
        47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44,
        154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19, 98,
        108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34,
        242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14,
        239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121,
        50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243,
        141, 128, 195, 78, 66, 215, 61, 156, 180 ]

permMod12 :: Vector Int
permMod12 = fmap (`mod` 12) perm

-- Skewing and unskewing factors for 2, 3, and 4 dimensions
f4, g4 :: Double
f4 = (sqrt 5-1)/4
g4 = (5-sqrt 5)/20


dot2 :: Grad3 -> Double -> Double -> Double
dot2 (Grad3 gx gy _) x y = gx*x + gy*y

dot3 :: Grad3 -> Double -> Double -> Double -> Double
dot3 (Grad3 gx gy gz) x y z = gx*x + gy*y + gz*z

dot4 :: Grad4 -> Double -> Double -> Double -> Double -> Double
dot4 (Grad4 gx gy gz gw) x y z w = gx*x + gy*y + gz*z + gw*w

-- | 2D simplex noise
noise2 :: Double -> Double -> Double
noise2 xin yin =
    let
        -- Skewing and unskewing factors for 2 dimensions
        f2, g2 :: Double
        f2 = 0.5*(sqrt 3-1)
        g2 = (3-sqrt 3)/6

        -- Skew the input space to determine which simplex cell we’re in
        i, j :: Int
        (i,j) = (floor (xin+s), floor (yin+s))
          where
            s = (xin+yin)*f2  -- Hairy factor for 2D

        -- The x,y distances from the cell origin
        x0, y0 :: Double
        (x0, y0) = (xin-xx0, yin-yy0)
          where
            t = (fromIntegral (i+j))*g2
            xx0 = fromIntegral i-t -- Unskew the cell origin back to (x,y) space
            yy0 = fromIntegral j-t

        -- For the 2D case, the simplex shape is an equilateral triangle.
        -- Determine which simplex we are in.
        i1, j1 :: Int -- Offsets for second (middle) corner of simplex in (i,j) coords
        (i1, j1)
            | x0>y0 = (1,0) -- lower triangle, XY order: (0,0)->(1,0)->(1,1)
            | otherwise = (0,1) -- upper triangle, YX order: (0,0)->(0,1)->(1,1)

        -- A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
        -- a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where c = (3-sqrt 3)/6

        -- Offsets for middle corner in (x,y) unskewed coords
        x1, y1 :: Double
        (x1, y1) =
            ( x0 - fromIntegral i1 + g2
            , y0 - fromIntegral j1 + g2
            )

        -- Offsets for last corner in (x,y) unskewed coords
        x2, y2 :: Double
        (x2, y2) =
            ( x0 - 1 + 2 * g2
            , y0 - 1 + 2 * g2
            )

        -- Work out the hashed gradient indices of the three simplex corners
        gi0, gi1, gi2 :: Int
        (gi0, gi1, gi2) =
            ( permMod12 ! (ii+   (perm ! jj     ))
            , permMod12 ! (ii+i1+(perm ! (jj+j1)))
            , permMod12 ! (ii+1+ (perm ! (jj+1) )))
          where
            ii = i .&. 255
            jj = j .&. 255

        cornerContribution x y gi =
            let t1 = 0.5 - x*x - y*y;
                t2 = t1^2
            in if t1 < 0
                then 0
                else t2 * t2 * dot2 (grad3!gi) x y

        -- Noise contributions from the three corners
        n0, n1, n2 :: Double
        [n0, n1, n2] = [cornerContribution x y gi | (x,y,gi) <- [(x0,y0,gi0), (x1,y1,gi1), (x2,y2,gi2)]]

    in
        -- Add contributions from each corner to get the final noise value.
        -- The result is scaled to return values in the interval [-1,1].
        70 * (n0 + n1 + n2)


noise3 :: Double -> Double -> Double -> Double
noise3 xin yin zin =
    let
        -- Skewing and unskewing factors for 2 dimensions
        f3, g3 :: Double
        f3 = 1/3 -- Very nice and simple skew factor for 3D
        g3 = 1/6

        i, j, k :: Int
        (i,j,k) = (floor (xin+s), floor (yin+s), floor (zin+s))
          where
            s = (xin+yin+zin)*f3

        -- The x,y,z distances from the cell origin
        x0, y0, z0 :: Double
        (x0, y0, z0) = (xin-xx0, yin-yy0, zin-zz0) -- The x,y,z distances from the cell origin
          where
            t = (fromIntegral (i+j+k))*g3
            xx0 = fromIntegral i-t -- Unskew the cell origin back to (x,y,z) space
            yy0 = fromIntegral j-t
            zz0 = fromIntegral k-t


        -- For the 3D case, the simplex shape is a slightly irregular tetrahedron.
        -- Determine which simplex we are in.
        i1, j1, k1 :: Int -- Offsets for second corner of simplex in (i,j,k) coords
        i2, j2, k2 :: Int -- Offsets for third corner of simplex in (i,j,k) coords
        (i1, j1, k1, i2, j2, k2)
            | x0 >= y0 = if
                | y0 >= z0  -> (1, 0, 0, 1, 1, 0) -- X Y Z order
                | x0 >= z0  -> (1, 0, 0, 1, 0, 1) -- X Z Y order
                | otherwise -> (0, 0, 1, 1, 0, 1) -- Z X Y order
            | otherwise = if
                | y0 < z0   -> (0, 0, 1, 0, 1, 1) -- Z Y X order
                | x0 < z0   -> (0, 1, 0, 0, 1, 1) -- Y Z X order
                | otherwise -> (0, 1, 0, 1, 1, 0) -- Y X Z order

        -- A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
        -- a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
        -- a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
        -- c = 1/6.

        -- Offsets for second corner in (x,y,z) coords
        x1, y1, z1 :: Double
        (x1, y1, z1) =
            ( x0 - fromIntegral i1 + g3
            , y0 - fromIntegral j1 + g3
            , z0 - fromIntegral k1 + g3
            )

        -- Offsets for third corner in (x,y,z) coords
        x2, y2, z2 :: Double
        (x2, y2, z2) =
            ( x0 - fromIntegral i2 + 2*g3
            , y0 - fromIntegral j2 + 2*g3
            , z0 - fromIntegral k2 + 2*g3
            )

        -- Offsets for last corner in (x,y,z) coords
        x3, y3, z3 :: Double
        (x3, y3, z3) =
            ( x0 - 1 + 3*g3
            , y0 - 1 + 3*g3
            , z0 - 1 + 3*g3
            )

        -- Work out the hashed gradient indices of the four simplex corners
        gi0, gi1, gi2, gi3 :: Int
        (gi0, gi1, gi2, gi3) =
            ( permMod12 ! (ii+   (perm ! (jj+   (perm ! (kk)))))
            , permMod12 ! (ii+i1+(perm ! (jj+j1+(perm ! (kk+k1)))))
            , permMod12 ! (ii+i2+(perm ! (jj+j2+(perm ! (kk+k2)))))
            , permMod12 ! (ii+1+ (perm ! (jj+1+ (perm ! (kk+1)))))
            )
          where
            ii = i .&. 255
            jj = j .&. 255
            kk = k .&. 255

        cornerContribution x y z gi =
            let t1 = 0.6 - x*x - y*y - z*z
                t2 = t1^2
            in if t1 < 0
                then 0
                else t2 * t2 * dot3 (grad3!gi) x y z

        [n0, n1, n2, n3] = [cornerContribution x y z gi | (x,y,z,gi) <- [(x0,y0,z0,gi0), (x1,y1,z1,gi1), (x2,y2,z2,gi2), (x3,y3,z3,gi3)]]

    in
        -- Add contributions from each corner to get the final noise value.
        -- The result is scaled to stay just inside [-1,1]
        32 * (n0 + n1 + n2 + n3);



--
--   // 4D simplex noise, better simplex rank ordering method 2012-03-09
--   public static double noise(double x, double y, double z, double w) {
--
--     double n0, n1, n2, n3, n4; // Noise contributions from the five corners
--     -- Skew the (x,y,z,w) space to determine which cell of 24 simplices we’re in
--     double s = (x + y + z + w) * F4; // Factor for 4D skewing
--     int i = fastfloor(x + s);
--     int j = fastfloor(y + s);
--     int k = fastfloor(z + s);
--     int l = fastfloor(w + s);
--     double t = (i + j + k + l) * G4; // Factor for 4D unskewing
--     double X0 = i - t; // Unskew the cell origin back to (x,y,z,w) space
--     double Y0 = j - t;
--     double Z0 = k - t;
--     double W0 = l - t;
--     double x0 = x - X0;  // The x,y,z,w distances from the cell origin
--     double y0 = y - Y0;
--     double z0 = z - Z0;
--     double w0 = w - W0;
--     -- For the 4D case, the simplex is a 4D shape I won't even try to describe.
--     -- To find out which of the 24 possible simplices we’re in, we need to
--     -- determine the magnitude ordering of x0, y0, z0 and w0.
--     -- Six pair-wise comparisons are performed between each possible pair
--     -- of the four coordinates, and the results are used to rank the numbers.
--     int rankx = 0;
--     int ranky = 0;
--     int rankz = 0;
--     int rankw = 0;
--     if(x0 > y0) rankx++; else ranky++;
--     if(x0 > z0) rankx++; else rankz++;
--     if(x0 > w0) rankx++; else rankw++;
--     if(y0 > z0) ranky++; else rankz++;
--     if(y0 > w0) ranky++; else rankw++;
--     if(z0 > w0) rankz++; else rankw++;
--     int i1, j1, k1, l1; // The integer offsets for the second simplex corner
--     int i2, j2, k2, l2; // The integer offsets for the third simplex corner
--     int i3, j3, k3, l3; // The integer offsets for the fourth simplex corner
--     -- [rankx, ranky, rankz, rankw] is a 4-vector with the numbers 0, 1, 2 and 3
--     -- in some order. We use a thresholding to set the coordinates in turn.
-- 	// Rank 3 denotes the largest coordinate.
--     i1 = rankx >= 3 ? 1 : 0;
--     j1 = ranky >= 3 ? 1 : 0;
--     k1 = rankz >= 3 ? 1 : 0;
--     l1 = rankw >= 3 ? 1 : 0;
--     -- Rank 2 denotes the second largest coordinate.
--     i2 = rankx >= 2 ? 1 : 0;
--     j2 = ranky >= 2 ? 1 : 0;
--     k2 = rankz >= 2 ? 1 : 0;
--     l2 = rankw >= 2 ? 1 : 0;
--     -- Rank 1 denotes the second smallest coordinate.
--     i3 = rankx >= 1 ? 1 : 0;
--     j3 = ranky >= 1 ? 1 : 0;
--     k3 = rankz >= 1 ? 1 : 0;
--     l3 = rankw >= 1 ? 1 : 0;
--     -- The fifth corner has all coordinate offsets = 1, so no need to compute that.
--     double x1 = x0 - i1 + G4; // Offsets for second corner in (x,y,z,w) coords
--     double y1 = y0 - j1 + G4;
--     double z1 = z0 - k1 + G4;
--     double w1 = w0 - l1 + G4;
--     double x2 = x0 - i2 + 2.0*G4; // Offsets for third corner in (x,y,z,w) coords
--     double y2 = y0 - j2 + 2.0*G4;
--     double z2 = z0 - k2 + 2.0*G4;
--     double w2 = w0 - l2 + 2.0*G4;
--     double x3 = x0 - i3 + 3.0*G4; // Offsets for fourth corner in (x,y,z,w) coords
--     double y3 = y0 - j3 + 3.0*G4;
--     double z3 = z0 - k3 + 3.0*G4;
--     double w3 = w0 - l3 + 3.0*G4;
--     double x4 = x0 - 1.0 + 4.0*G4; // Offsets for last corner in (x,y,z,w) coords
--     double y4 = y0 - 1.0 + 4.0*G4;
--     double z4 = z0 - 1.0 + 4.0*G4;
--     double w4 = w0 - 1.0 + 4.0*G4;
--     -- Work out the hashed gradient indices of the five simplex corners
--     int ii = i & 255;
--     int jj = j & 255;
--     int kk = k & 255;
--     int ll = l & 255;
--     int gi0 = perm[ii+perm[jj+perm[kk+perm[ll]]]] % 32;
--     int gi1 = perm[ii+i1+perm[jj+j1+perm[kk+k1+perm[ll+l1]]]] % 32;
--     int gi2 = perm[ii+i2+perm[jj+j2+perm[kk+k2+perm[ll+l2]]]] % 32;
--     int gi3 = perm[ii+i3+perm[jj+j3+perm[kk+k3+perm[ll+l3]]]] % 32;
--     int gi4 = perm[ii+1+perm[jj+1+perm[kk+1+perm[ll+1]]]] % 32;
--     -- Calculate the contribution from the five corners
--     double t0 = 0.6 - x0*x0 - y0*y0 - z0*z0 - w0*w0;
--     if(t0<0) n0 = 0.0;
--     else {
--       t0 *= t0;
--       n0 = t0 * t0 * dot(grad4[gi0], x0, y0, z0, w0);
--     }
--    double t1 = 0.6 - x1*x1 - y1*y1 - z1*z1 - w1*w1;
--     if(t1<0) n1 = 0.0;
--     else {
--       t1 *= t1;
--       n1 = t1 * t1 * dot(grad4[gi1], x1, y1, z1, w1);
--     }
--    double t2 = 0.6 - x2*x2 - y2*y2 - z2*z2 - w2*w2;
--     if(t2<0) n2 = 0.0;
--     else {
--       t2 *= t2;
--       n2 = t2 * t2 * dot(grad4[gi2], x2, y2, z2, w2);
--     }
--    double t3 = 0.6 - x3*x3 - y3*y3 - z3*z3 - w3*w3;
--     if(t3<0) n3 = 0.0;
--     else {
--       t3 *= t3;
--       n3 = t3 * t3 * dot(grad4[gi3], x3, y3, z3, w3);
--     }
--    double t4 = 0.6 - x4*x4 - y4*y4 - z4*z4 - w4*w4;
--     if(t4<0) n4 = 0.0;
--     else {
--       t4 *= t4;
--       n4 = t4 * t4 * dot(grad4[gi4], x4, y4, z4, w4);
--     }
--     -- Sum up and scale the result to cover the range [-1,1]
--     return 27.0 * (n0 + n1 + n2 + n3 + n4);
--   }
