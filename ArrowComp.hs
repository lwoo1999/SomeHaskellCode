{-# LANGUAGE Arrows #-}
module Lib where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow

-- utils

assoc :: ((a, b), c) -> (a, (b, c))
assoc ~(~(a, b), c) = (a, (b, c))

unassoc :: (a, (b, c)) -> ((a, b), c)
unassoc ~(a, ~(b, c)) = ((a, b), c)

dup :: Arrow a => a b (b, b)
dup = arr $ \b -> (b, b)

-- SF

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Category SF where
  id = SF id
  SF bc . SF ab = SF $ bc . ab

instance Arrow SF where
  arr f = SF $ map f
  first (SF f) = SF $ \xys -> let
    (as, bs) = unzip xys
    in zip (f as) bs

instance ArrowChoice SF where
  left (SF f) = SF $ \xs -> combine xs $ f [x | Left x <- xs]
    where
      combine (Left x : xs) (y : ys) = Left y : combine xs ys
      combine (Right x : xs) ys = Right x : combine xs ys
      combine [] _ = []

-- Writer

newtype Writer i o = W {runW :: i -> (String, o)}

instance Category Writer where
  id = W $ dup >>> first (const "")
  W bc . W ab = W $ \a -> let
    (str, b) = ab a
    (str', c) = bc b
    in (str ++ str', c)

instance Arrow Writer where
  arr f = W $ const "" &&& f
  first (W f) = W $ \(i, c) -> let
    (str, o) = f i
    in (str, (o, c))

instance ArrowApply Writer where
  app = W $ \(W f, x) -> f x

testW :: Writer Int Int
testW = proc x -> do
  x1 <- addOne -< x
  x2 <- addTwo -< x
  returnA -< x1 + x2
  where
    addOne = W $ \i -> ("add one\n", i + 1)
    addTwo = W $ \i -> ("add two\n", i + 2)

-- Reader

newtype Reader s i o = R {runR :: (s, i) -> o}

instance Category (Reader s) where
  id = R snd
  R bc . R ab = R $ \(s, a) -> let
    b = ab (s, a)
    c = bc (s, b)
    in c

instance Arrow (Reader s) where
  arr f = R $ snd >>> f
  first (R f) = R $ \(s, (a, b)) -> (f (s, a), b)

instance ArrowApply (Reader s) where
  app = R $ \(s, (R f, x)) -> f (s, x)

-- Stream and StreamMap

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show (Cons x xs) = show x ++ ", " ++ show xs

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative Stream where
  pure a = Cons a $ pure a
  Cons f fs <*> Cons x xs = Cons (f x) $ fs <*> xs

repeatList :: [a] -> Stream a
repeatList xs = foldr Cons (repeatList xs) xs

unzipS :: Stream (a, b) -> (Stream a, Stream b)
unzipS (Cons (a, b) abs') = (Cons a as, Cons b bs)
  where (as, bs) = unzipS abs'

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS (Cons a as) (Cons b bs) = Cons (a, b) $ zipS as bs

newtype StreamMap i o = SM {runSM :: Stream i -> Stream o}

instance Category StreamMap where
  id = id
  SM bc . SM ab = SM $ bc . ab

instance Arrow StreamMap where
  arr f = SM $ fmap f
  first (SM f) = SM $ unzipS >>> first f >>> uncurry zipS


instance ArrowChoice StreamMap where
  left (SM f) = SM lf
    where
      getls (Cons (Left l) lrs) = Cons l $ getls lrs
      getls (Cons (Right r) lrs) = getls lrs
      combine (Cons (Left l) lrs) (Cons l' ls) = Cons (Left l') $ combine lrs ls
      combine (Cons (Right r) lrs) ls = Cons (Right r) $ combine lrs ls
      lf s = combine s $ f $ getls s

instance ArrowLoop StreamMap where
  loop (SM f) = SM $ \i -> let
    os = f $ zipS i $ snd $ unzipS os
    in fst $ unzipS os

-- instance ArrowCircuit StreamMap where
--   delay b = SM $ \b' -> Cons b b'

testSM :: StreamMap [Int] Bool
testSM = proc xs -> case xs of
  [] -> returnA -< False
  _ -> returnA -< True

-- NonDet

newtype NonDet i o = ND {runND :: i -> [o]}

instance Category NonDet where
  id = ND pure
  ND bc . ND ab = ND $ \a -> [c | b <- ab a, c <- bc b]

instance Arrow NonDet where
  arr f = ND $ pure . f
  first (ND f) = ND $ \(a, b) -> [(c, b) | c <- f a]

instance ArrowApply NonDet where
  app = ND $ \(ND f, x) -> f x

instance ArrowChoice NonDet where
  left (ND f) = ND lf
    where
      lf (Left x) = map Left $ f x
      lf (Right x) = [Right x]

-- Except

newtype Except a b c = E {runE :: a b (Either String c)}

instance ArrowChoice a => Category (Except a) where
  id = E $ arr $ id >>> Right
  E bc . E ab = E $ proc a -> do
    b <- ab -< a
    case b of
      Left s -> returnA -< Left s
      Right b' -> bc -< b'

instance ArrowChoice a => Arrow (Except a) where
  arr f = E $ arr $ Right . f
  first (E a) = E $ proc (b, c) -> do
    d <- a -< b
    case d of
      Left s -> returnA -< Left s
      Right d' -> returnA -< Right (d', c)

-- Auto

newtype Auto i o = A {runA :: i -> (o, Auto i o)}

instance Category Auto where
  id = A $ id &&& const id
  A bc . A ab = A $ \a -> let
    (b, bs) = ab a
    (c, cs) = bc b
    in (c, cs . bs)

instance Arrow Auto where
  arr f = A $ \i -> (f i, arr f)
  first (A f) = A $ \(a, b) -> let
    (c, cs) = f a
    in ((c, b), first cs)

instance ArrowChoice Auto where
  left (A f) = A lf
    where
      lf (Left l) = let
        (o, os) = f l
        in (Left o, left os)
      lf (Right r) = (Right r, left $ A f)

instance ArrowLoop Auto where
  loop (A f) = A $ \i -> let
    ((o, s), f') = f (i, s)
    in (o, loop f')
