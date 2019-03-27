module Generators where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Property 
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

genDigit :: Gen Char
genDigit = elements (['0'..'9'])

genDigits :: Gen String
genDigits = do
    n <- choose (0, 10)
    vectorOf n genDigit

genPrefix :: Gen String
genPrefix = listOf (elements ['-', '+', ' '])

genBasicAtoi :: Gen String
genBasicAtoi = genDigits 

genComplexAtoi :: Gen String
genComplexAtoi = liftM2 (++) genPrefix genDigits

genSpaceyAtoi :: Gen String
genSpaceyAtoi = do
    n <- choose (0, 10)
    space <- vectorOf n (elements [' ', '\n', '\t', '\v', '\f'])
    digits <- genDigits
    pure (space ++ digits)

genAtoiString :: Gen String
genAtoiString = frequency [(10, genBasicAtoi), (5, genSpaceyAtoi), (1, genComplexAtoi)]

newtype AtoiString = AtoiString { unwrapAtoiString :: String }

instance Show AtoiString where
    show = show . unwrapAtoiString

instance Arbitrary AtoiString where
    arbitrary = AtoiString <$> genAtoiString

genSafeChar :: Gen Char
genSafeChar = elements ([' '..'~'] ++ ['\n', '\t'])

genSafeString :: Gen String
genSafeString = listOf genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString

genPositive :: Gen Int
genPositive = choose (0, 1000)

newtype PositiveInt = PositiveInt { unwrapPositiveInt :: Int}
instance Show PositiveInt where
    show n = "+" ++ show (unwrapPositiveInt n) 

instance Arbitrary PositiveInt where
    arbitrary = PositiveInt <$> genPositive
