module Mean (mean) where

mean :: (Foldable t, Num a, Fractional a) => t a -> a
mean s = sum s / fromIntegral (length s)
