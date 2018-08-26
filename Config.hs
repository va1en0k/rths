module Config where

res :: (Int, Int)
res = (150, 100)

aaGenCount :: Int
aaGenCount = 4

---

kRes :: Double
kRes = fromIntegral $ uncurry min $ res
