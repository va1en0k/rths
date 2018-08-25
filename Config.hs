module Config where

res :: (Int, Int)
res = (300, 200)

aaGenCount :: Int
aaGenCount = 8

---

kRes :: Double
kRes = fromIntegral $ uncurry min $ res
