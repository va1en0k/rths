{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeFamilies #-}

import Codec.Picture
import Data.Vec3
import Data.Maybe
import Data.List
import Data.Array
import Data.Function
import Control.Monad.Random
import Control.Monad.Primitive
import Control.Monad.Identity
import Debug.Trace
import Data.Time.Clock.POSIX
import System.Environment


-- import Normal

import Vectors
import Progress

res :: (Int, Int)
res = (300, 200)
kRes :: Double
kRes = fromIntegral $ uncurry min $ res

data Ray = Ray {origin :: CVec3, direction :: CVec3}

data Camera = Camera {
  cOrigin :: CVec3,
  cHorizontal :: CVec3,
  cVertical :: CVec3,
  cLowerLeftCorner :: CVec3
}

camera = Camera (CVec3 0 0 0) (CVec3 3 0 0) (CVec3 0 (-3) 0) (CVec3 (-2) 2 (-1))

-- y :: Vec3 a -> Double
y v = y'
  where (_, y', _) = toXYZ v

(*.) :: Double -> CVec3 -> CVec3
k *. v = fromXYZ (x * k, y * k, z * k)
  where (x, y, z) = toXYZ v


sky :: Ray -> Color
sky r = ((1.0 - t) *. CVec3 1 1 1) <+> (t *. CVec3 0.5 0.7 1.0)
  where un = normalize $ direction r
        t = (y un + 1) * 0.5

objects :: [Hitable_]
objects = [
  MkHitable $ Sphere (CVec3 0.7 0 (-1)) 0.5,
  MkHitable $ Sphere (CVec3 0 (-100.5) (-1)) 100 ]

maxFloat :: Double
maxFloat = fromIntegral $ snd $ floatRange (0.5::Double)
--
-- color' :: RandomGen g => Ray -> Rand g Color
-- color' r = case (hit objects r 0 maxFloat) of
--   Just (Hit t p n) -> return $ 0.5 *. (CVec3 1 1 1 <+> n)
--   -- Just (Hit t p n) ->
--   --   do ru <- randomInUnitBall
--   --      let target = p <+> n <+> ru
--   --      cl <- color $ Ray p (target <-> p)
--   --      return $ 0.5 *. cl
--   Nothing -> return $ sky r


color :: RandomGen g => Ray -> Rand g Color
color r = case (hit objects r 0.00001 maxFloat) of
  -- Just (Hit t p n) -> 0.5 *. (CVec3 1 1 1 <+> n)
  Just (Hit t p n) ->
    do ru <- randomInUnitBall
       let target = p <+> n <+> ru
       cl <- color $ Ray p (target <-> p)
       return $ 0.5 *. cl
  Nothing -> return $ sky r

data Sphere = Sphere CVec3 Double
data Hit = Hit {
  hitT :: Double,
  hitP :: CVec3,
  hitNormal :: CVec3
}

frac :: Double -> Double
frac = snd . properFraction

randomInUnitBall :: RandomGen g => Rand g CVec3
randomInUnitBall =
  -- do (a:b:c:_) <- getRandomRs (0::Double, 1)
  --    let p = (2.0 *. CVec3 a b c) <-> CVec3 1 1 1
  --    if norm p >= 1 then randomInUnitBall else return p
  do --(x:y:z:_) <- normals
     (x:y:z:_) <- getRandomRs (0::Double, 1)
     r <- getRandomR (0::Double, 1)
     let p = r * sqrt (x*x + y*y + z*z)
     return $ (1 / p) *. CVec3 x y z


rayPointAt :: Ray -> Double -> CVec3
rayPointAt (Ray o d) t = o <+> (t *. d)

renderUV :: RandomGen g => Double -> Double -> Rand g Color
-- renderUV u v = CVec3 u v 0.2
renderUV u v = color $ Ray (cOrigin camera) (cLowerLeftCorner camera <+> (u *. (cHorizontal camera)) <+> (v *. (cVertical camera)))


colorToPixel :: Color -> PixelRGB8
colorToPixel c =
  let (r, g, b) = toXYZ $ mapv ((* 255.9) {-. sqrt-}) c
  in PixelRGB8 (fromInteger $ floor r) (fromInteger $ floor g) (fromInteger $ floor b)

renderOnce :: RandomGen g => Int -> Int -> Rand g Color
renderOnce x y =
  do (a:b:_) <- getRandomRs (0::Double, 1)
     renderUV ((a + fromIntegral x) / kRes) ((b + fromIntegral y) / kRes)

-- render :: RandomGen g => Int -> Int -> Rand g PixelRGB8
-- render :: Int -> [Hitable_] -> Int -> Int -> IO PixelRGB8
render :: Int -> Int -> IO PixelRGB8
render x y = colorToPixel <$> avgv <$> rendersIO
  where
    gens = replicateM 20 newStdGen
    rendersIO :: IO [Color]

    rendersIO = do
      gs <- gens
      let rs = map (runIdentity . evalRandT (renderOnce x y)) gs
      -- when (x == 200 && y == 40) (print rs)
      return $ rs --rs `deepseq` rs

vsum xs = foldr1 (<+>) xs
--
-- renderAntialiased :: Int -> Int -> PixelRGB8
-- renderAntialiased x y = colorToPixel $ (1 / (fromIntegral $ length points)) *. vsum points
--   where
--     points =
--       do i <- [0.1..0.9]
--          j <- [0.1..0.9]
--          return $ renderUV ((i + fromIntegral x) / kRes) ((j + fromIntegral y) / kRes)

type ImgBuf = Array (Int, Int) PixelRGB8

-- mmm :: RandT g [] a -> RandT g Identity [a]
-- mmm r =


genImageBuf :: Int -> Int -> IO ImgBuf
genImageBuf w h = array ((0, 0), (w, h)) <$> lsIO
  where
    -- ls :: RandT g Identity [((Int, Int), PixelRGB8)]
    allPixels = [(i, j) | i <- [0..w], j <- [0..h]]

    ls = mapWithProgressBar (uncurry $ render) allPixels

    lsIO :: IO [((Int, Int), PixelRGB8)]
    lsIO = zip allPixels <$> ls

genImageF :: Int -> Int -> IO (Int -> Int -> PixelRGB8)
genImageF w h = f <$> genImageBuf w h
  where
    -- f b x y | trace ((show x) ++ " " ++ (show y)) False = undefined
    f b x y = b ! (x, y)


main :: IO ()
main = do
  imF <- genImageF (fst res) (snd res)
  let im = (generateImage imF (fst res) (snd res))
  -- im <- evalRandIO rim
  -- print (renderUV 0 0)
  -- print (imF 10 10)
  -- render 10 [] 10 10 >>= print
  -- print (colorToPixel $ CVec3 0.8 0.7 1.0)
  now <- getPOSIXTime
  hash <- head <$> getArgs
  writePng ("./out/image__" ++ hash ++ "__" ++ (show now) ++ ".png") im
  writePng ("./image.png") im


class Hitable a where
  hit :: a -> Ray -> Double -> Double -> Maybe Hit

data Hitable_ = forall a . Hitable a => MkHitable a

instance Hitable Sphere where
  hit (Sphere sc sr) r@(Ray org dir) mn mx =
    let oc = org <-> sc
        a = dir .* dir
        b = 2 * oc .* dir
        c = (oc .* oc) - (sr * sr)
        dsc = b * b - 4 * a * c
        hf x = Just $ Hit x p n
          where
            p = rayPointAt r x
            -- n = normalize $ p <-> CVec3 0 0 (-1)
            n = (1 / sr) *. (p <-> sc)
    in if dsc < 0 then Nothing
       else let x0 = ( - b - sqrt dsc ) / (2 * a)
                x1 = ( - b + sqrt dsc ) / (2 * a)
            in if x0 >= mn && x0 <= mx then hf x0
               else if x1 >= mn && x1 <= mx then hf x1
                 else Nothing



instance Hitable Hitable_ where
 hit (MkHitable a) r mn mx = hit a r mn mx

instance Hitable [Hitable_] where
  hit objects r mn mx = listToMaybe $ sortBy (compare `on` hitT) $ catMaybes $ map (\o -> hit o r mn mx) objects
