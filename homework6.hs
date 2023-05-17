{-# OPTIONS_GHC -fno-warn-missing-methods #-}

data Stream a = Stream (Stream a) a

showN n (Stream s a)
  | n > 0 = show a ++ " " ++ showN (n - 1) s
  | otherwise = ""

instance Show a => Show (Stream a) where
  show = showN 20

streamToList :: Stream a -> [a]
streamToList (Stream s a) = streamToList s ++ [a]

streamRepeat :: a -> Stream a
streamRepeat a = Stream (streamRepeat a) a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream s a) = Stream (streamMap f s) (f a)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream (streamFromSeed f (f a)) a

nats = streamFromSeed (1 +) 0

x :: Stream Integer
x = Stream (Stream (streamRepeat 0) 1) 0

instance Num (Stream Integer) where
  fromInteger = Stream (streamRepeat 0)
  negate = streamMap (\n -> (-n))
  (Stream s1 a1) + (Stream s2 a2) = Stream (s1 + s2) (a1 + a2)
  (Stream s1 a1) * b@(Stream s2 a2) = Stream (streamMap (a1 *) s2 + s1 * b) (a1 * a2)

a@(Stream sa a0) `sdiv` b@(Stream sb b0) = Stream (streamMap (`div` b0) (sa - (a `sdiv` b) * sb)) (a0 `div` b0)

fibs3 = x `sdiv` (1 - x - x ^ 2)

newtype Matrix = M (Integer, Integer, Integer, Integer)

instance Num Matrix where
  M (a11, a12, a21, a22) * M (b11, b12, b21, b22) = M (a11 * b11 + a12 * b21, a11 * b12 + a12 * b22, a21 * b11 + a22 * b21, a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fn where M (_, fn, _, _) = M (1, 1, 1, 0) ^ n
