module Config where

res :: (Int, Int)
res = (900, 600)

aaGenCount :: Int
aaGenCount = 16

---

kRes :: Double
kRes = fromIntegral $ uncurry min $ res
