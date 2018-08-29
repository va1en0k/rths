module Graphics.Images.Text (text) where

-- import           Codec.Picture

import Data.Word
import Data.Bits
import           Data.Array.Unboxed
import           Data.Array.IArray
import           Data.Array.MArray

import Graphics.Rendering.Pango.Cairo
import Graphics.Rendering.Cairo

import Graphics.Images


transpSurface :: (Double, Double) -> Render ()
transpSurface (w, h) = do
  save
  rectangle 0 0 w h
  setSourceRGBA 0 0 0 0
  setOperator OperatorSource
  fill
  restore

getTextSize :: String -> IO (Double, Double)
getTextSize str = return (150, 30)
    -- withImageSurface FormatARGB32 100 100 $ \surf ->
    --   renderWith surf $
    --     do
    --       selectFontFace "sans" FontSlantNormal FontWeightNormal
    --       setFontSize 20
    --       (TextExtents xb yb w h _ _) <- textExtents str
    --       liftIO $ print (xb, yb, w, h)
    --       return (xb + w, 20 + h + abs yb)


surfaceToImg :: Surface -> IO Image
surfaceToImg surf =
  do
    surfaceWriteToPNG surf "temp.png"
    print "b"
    pxls' <- imageSurfaceGetPixels surf -- :: IO (SurfaceData Int Word32)
    w <- imageSurfaceGetWidth surf
    h <- imageSurfaceGetHeight surf
    print (w, h)

    pxls <- freeze pxls' :: IO (UArray Int Word32)
    yk <- (`div` 4) <$> imageSurfaceGetStride surf
    print "c"
    let p (x, y) = yk * y + x
    return $ Image (w, h) $ word32ToColor . (pxls !) . p


text :: String -> IO Image
text str = do
  cairoContext <- cairoCreateContext Nothing

  (w, h) <- getTextSize str
  print (w, h)

  withImageSurface FormatARGB32 (floor w) (floor h) $ \surf ->
    do
      renderWith surf (textRender (w, h) str)
      print "a"
      i <- surfaceToImg surf
      print "D"
      return i




textRender :: (Double, Double) -> String -> Render ()
textRender dims text =
  do
    selectFontFace "sans" FontSlantNormal FontWeightNormal
    setFontSize 20

    (TextExtents _ yb _ h _ _) <- textExtents text

    transpSurface dims

    setSourceRGB 1 1 1
    setLineWidth 1.0

    moveTo 0 (snd dims)
    textPath text

    fill

word32ToColor :: Word32 -> PixelRGB8
word32ToColor color = PixelRGB8 r g b where
  a = fromIntegral (shift (color .&. 0xFF000000) (-24))
  r = fromIntegral (shift (color .&. 0x00FF0000) (-16))
  g = fromIntegral (shift (color .&. 0x0000FF00) (-8))
  b = fromIntegral (shift (color .&. 0x000000FF) (-0))
