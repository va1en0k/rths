module Config where

res :: (Int, Int)
res = (600, 300)

aaGenCount :: Int
aaGenCount = 8

---

kRes :: Double
kRes = fromIntegral $ uncurry min $ res
