{-# LANGUAGE ConstraintKinds #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Applicative
import Data.Functor.Identity
import qualified Data.IntMap as Map
import Data.Kind
import qualified Data.List as List
import Data.Map hiding (lookup)
import Test.Hspec
import Test.QuickCheck

-- to the same describe block as the others
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose ::
    (Eq (f c), Functor f) =>
    (a -> b) ->
    (b -> c) ->
    f a ->
    Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

newtype Identity' a = Identity' a
instance Functor Identity' where
    fmap f (Identity' x) = Identity' $ f x

data Pair a = Pair a a
    deriving (Eq, Show)
instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        Pair x <$> arbitrary

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        Two x <$> arbitrary

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Three x y <$> arbitrary

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b
    deriving (Eq, Show)
instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Three' x y <$> arbitrary

data Four a b c d = Four a b c d
    deriving (Eq, Show)
instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z (f w)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        Four x y z <$> arbitrary

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        Four' x y z <$> arbitrary

instance Functor (Four' a) where
    fmap f (Four' x y z w) = Four' x y z (f w)

data Trivial a = Trivial
    deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Trivial a) where
    arbitrary = return Trivial

instance Functor Trivial where
    fmap _ Trivial = Trivial

data Possibly a = LolNope | Yeppers a
    deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Possibly a) where
    arbitrary = do
        x <- arbitrary
        elements [LolNope, Yeppers x]

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers x) = Yeppers $ f x

data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First x) = First x
    fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [First x, Second y]

data Quant a b
    = Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Finance, Desk x, Bloor y]

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x
    fmap f (Bloor y) = Bloor $ f y

-- not possible because Mu's kind is (* -> *) -> *, not * -> *
-- data Mu f = InF { outF :: f (Mu f) }
-- instance Functor Mu where
--     fmap f (InF fa) = InF $ fmap (fmap f) fa

data K a b = K a
    deriving (Eq, Show)
instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a
instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary = do
        K <$> arbitrary

data EvilGoateeConst a b
    = GoatyConst b
    deriving (Eq, Show)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary = do
        GoatyConst <$> arbitrary
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut a) = LiftItOut $ fmap f a

instance (Functor f, Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
    arbitrary = LiftItOut <$> arbitrary

type MaybeLiftItOut = LiftItOut Maybe

data List' a = Nil | Cons a (List' a)
    deriving (Eq, Show)

instance Functor List' where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data TalkToMe a
    = Halt
    | Print String a
    | Read (String -> a)

data (Eq a) => Set' a = Set' [a]

-- member :: (Eq a) => a -> Set' a -> Bool
-- member x (Set' xs) = x `elem` xs

-- add :: (Eq a) => a -> Set' a -> Set' a
-- add x (Set' xs) =
--     Set' $
--         List.foldr
--             ( \x' seen ->
--                 if x' `elem` seen
--                     then seen
--                     else seen ++ [x']
--             )
--             []
--             (x : xs)

-- class Functor' f where
--     type FunctorConstraint f :: Type -> Constraint
--     fmap' ::
--         (FunctorConstraint f a, FunctorConstraint f b) =>
--         (a -> b) ->
--         f a ->
--         f b

-- instance Functor' Set' where
--     type FunctorConstraint Set' = Eq
--     fmap' f (Set' xs) = List.foldr (add . f) (Set' []) xs

main :: IO ()
main =
    print $ [1 :: Int] <|> [1, 2]

-- added :: Maybe Integer
-- added =
--     (+ 3) <$> lookup (3 :: Integer) (zip [1, 2, 3] [4, 5, 6])

-- y :: Maybe Integer
-- y = lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

-- z :: Maybe Integer
-- z = lookup (2 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

-- tupled :: Maybe (Integer, Integer)
-- -- tupled = (,) <$> y <*> z
-- tupled = liftA2 (,) y z

-- x :: Maybe Int
-- x = List.elemIndex (3 :: Integer) [1, 2, 3, 4, 5]

-- y' :: Maybe Int
-- y' = List.elemIndex (4 :: Integer) [1, 2, 3, 4, 5]

-- max' :: Int -> Int -> Int
-- max' = max

-- maxed :: Maybe Int
-- maxed = max' <$> x <*> y'

-- xs = [1, 2, 3]
-- ys = [4, 5, 6]

-- x' :: Maybe Integer
-- x' = lookup 3 $ zip xs ys

-- y'' :: Maybe Integer
-- y'' = lookup 2 $ zip xs ys

-- summed :: Maybe Integer
-- summed = (+) <$> x' <*> y''

-- data Id a = Id a

-- instance Functor Id where
--     fmap f (Id a) = Id (f a)

-- instance Applicative Id where
--     pure = Id
--     Id f <*> Id a = Id $ f a

newtype Constant' a b = Constant' {getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
    fmap _ (Constant' c) = Constant' c

instance (Monoid a) => Applicative (Constant' a) where
    pure _ = Constant' mempty
    Constant' f <*> Constant' a = Constant' $ f <> a

-- let
--     x' = f $ getConstant c
--  in
--     Constant'{getConstant = x'}

-- -- mempty = Nothing
-- mappend' m Nothing = m
-- mappend' Nothing m = m
-- mappend' (Just a) (Just a') = Just (mappend' a a')

-- hspec $ do
-- describe "Functor Laws" $ do
--     describe "Identity Law" $ do
--         it "holds for []" $
--             property (functorIdentity :: [Int] -> Bool)
--         it "holds for Maybe" $
--             property (functorIdentity :: Maybe Int -> Bool)
--         it "holds for Identity" $
--             property (functorIdentity :: Identity Int -> Bool)
--         it "holds for ZipList" $
--             property (functorIdentity :: ZipList Int -> Bool)
--         it "holds for Pair" $
--             property (functorIdentity :: Pair Int -> Bool)
--         it "holds for Two" $
--             property (functorIdentity :: Two Int Int -> Bool)
--         it "holds for Three" $
--             property (functorIdentity :: Three Int Int Int -> Bool)
--         it "holds for Four" $
--             property (functorIdentity :: Four Int Int Int Int -> Bool)
--         it "holds for Trivial" $
--             property (functorIdentity :: Trivial Int -> Bool)
--         it "holds for Possibly" $
--             property (functorIdentity :: Possibly Int -> Bool)
--         it "holds for Sum" $
--             property (functorIdentity :: Sum Int Int -> Bool)
--         it "holds for Quant" $
--             property (functorIdentity :: Quant Int Int -> Bool)
--         it "holds for K" $
--             property (functorIdentity :: K Int Int -> Bool)
--         it "holds for EvilGoateeConst" $
--             property (functorIdentity :: EvilGoateeConst Int Int -> Bool)
--         it "holds for LiftItOut" $
--             property (functorIdentity :: LiftItOut Maybe Int -> Bool)

--     describe "Composition Law" $ do
--         it "holds for []" $
--             property (functorCompose (+ 1) (* 2) :: [Int] -> Bool)
--         it "holds for Maybe" $
--             property (functorCompose (+ 1) (* 2) :: Maybe Int -> Bool)
--         it "holds for Identity" $
--             property (functorCompose (+ 1) (* 2) :: Identity Int -> Bool)
--         it "holds for ZipList" $
--             property (functorCompose (+ 1) (* 2) :: ZipList Int -> Bool)
--         it "holds for Pair" $
--             property (functorCompose (+ 1) (* 2) :: Pair Int -> Bool)
--         it "holds for Two" $
--             property (functorCompose (+ 1) (* 2) :: Two Int Int -> Bool)
--         it "holds for Three" $
--             property (functorCompose (+ 1) (* 2) :: Three Int Int Int -> Bool)
--         it "holds for Four" $
--             property (functorCompose (+ 1) (* 2) :: Four Int Int Int Int -> Bool)
--         it "holds for Trivial" $
--             property (functorCompose (+ 1) (* 2) :: Trivial Int -> Bool)
--         it "holds for Possibly" $
--             property (functorCompose (+ 1) (* 2) :: Possibly Int -> Bool)
--         it "holds for Sum" $
--             property (functorCompose (+ 1) (* 2) :: Sum Int Int -> Bool)
--         it "holds for Quant" $
--             property (functorCompose (+ 1) (* 2) :: Quant Int Int -> Bool)

-- describe "Addition" $ do
--     it "1 + 1 is greater than 1" $ (1 + 1) > (1 :: Int) `shouldBe` True
--     It "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)