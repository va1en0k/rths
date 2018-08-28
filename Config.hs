module Config where

res :: (Int, Int)
res = (300, 150)

aaGenCount :: Int
aaGenCount = 4

---

kRes :: Double
kRes = fromIntegral $ uncurry min $ res
