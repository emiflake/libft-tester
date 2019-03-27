{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, ViewPatterns #-}
module Main where

import Data.Maybe
import Text.Read
import Text.Printf
import Control.Monad
import Control.Monad.IO.Class
import Test.QuickCheck
import Test.QuickCheck.Property 
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr, nullPtr)
import Color
import ForeignLibft
import Generators

prop_ft_toupper :: CInt -> Bool 
prop_ft_toupper n = ft_toupper n `notElem` (toEnum . fromEnum <$> ['a'..'z'])

prop_ft_tolower :: CInt -> Bool 
prop_ft_tolower n = ft_tolower n `notElem` (toEnum . fromEnum <$> ['A'..'Z'])

prop_ft_strlen :: String -> Property
prop_ft_strlen hstr = monadicIO $ do
    str <- run $ newCString hstr
    assert $ ft_strlen str == strlen str

prop_ft_strdup :: String -> Property
prop_ft_strdup hstr = monadicIO $ do
    str <- run $ newCString hstr
    copy1 <- run $ ft_strdup str
    copy2 <- run $ strdup str
    assert $ strcmp' copy1 str
    assert $ strcmp' copy1 copy2
    assert $ copy1 /= str
    assert $ copy1 /= copy2

prop_ft_strjoin :: (SafeString, SafeString) -> Property
prop_ft_strjoin (a, b) = monadicIO $ do
    astr <- run $ newCString (unwrapSafeString a)
    bstr <- run $ newCString (unwrapSafeString b)
    my_res <- run $ newCString (unwrapSafeString a ++ unwrapSafeString b)
    ft_res <- run $ ft_strjoin astr bstr
    assert $ strcmp' my_res ft_res
    run $ free astr
    run $ free bstr
    run $ free my_res
    run $ free ft_res
    
prop_ft_atoi :: (AtoiString) -> Property
prop_ft_atoi str = monadicIO $ do
    cstr <- run $ newCString (unwrapAtoiString str)
    assert $ atoi cstr == ft_atoi cstr

prop_ft_itoa :: Int -> Property
prop_ft_itoa i = monadicIO $ do
    str <- run $ ft_itoa (toEnum $ fromEnum i)
    hstr <- run $ peekCString str 
    let read' = readMaybe hstr :: Maybe Int
    assert $ not $ isNothing read'
    case read' of
        Nothing -> pure ()
        Just n -> assert $ n == i 
    -- assert $ (read hstr :: Int) == i

prop_ft_memset :: (PositiveInt, CInt) -> Property
prop_ft_memset (unwrapPositiveInt -> size, ch) = monadicIO $ do
    space1 <- run $ size :: IO (Ptr ())
    space2 <- run $ (mallocBytes size :: IO (Ptr ()))
    run $ ft_memset space1 ch size
    run $ memset space2 ch size
    assert $ memeq space1 space2 size 

prop_ft_strcmp :: (SafeString, SafeString) -> Property
prop_ft_strcmp (a, b) = monadicIO $ do
    str1 <- run $ newCString (unwrapSafeString a)
    str2 <- run $ newCString (unwrapSafeString b) 
    assert $ ft_strcmp str1 str2 == strcmp str1 str2

-- prop_ft_strtrim :: SafeString -> Property
-- prop_ft_strtrim str = do
--     str1 <- run $ newCString str
--     hstr <- run $ peekCString str 
    
    

quickCheck' title d = do
    printf "%s => test '%s'\n%s" eCyan title eReset
    quickCheck (withMaxSuccess 10000 d)
main :: IO ()
main = do
    quickCheck' "toupper" prop_ft_toupper
    quickCheck' "tolower" prop_ft_tolower
    quickCheck' "strcmp" prop_ft_strcmp
    quickCheck' "strlen" prop_ft_strlen
    quickCheck' "strdup" prop_ft_strdup
    quickCheck' "strjoin" prop_ft_strjoin
    quickCheck' "memset" prop_ft_memset
    quickCheck' "atoi" prop_ft_atoi
    quickCheck' "itoa" prop_ft_itoa
