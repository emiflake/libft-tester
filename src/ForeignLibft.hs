{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances #-}
module ForeignLibft where

import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

-- toupper
foreign import ccall "ft_toupper" ft_toupper :: CInt -> CInt
foreign import ccall "toupper" toupper :: CInt -> CInt

-- tolower
foreign import ccall "ft_tolower" ft_tolower :: CInt -> CInt
foreign import ccall "tolower" tolower :: CInt -> CInt

-- strlen
foreign import ccall "ft_strlen" ft_strlen :: CString -> CInt
foreign import ccall "strlen" strlen :: CString -> CInt

-- strdup
foreign import ccall "ft_strdup" ft_strdup :: CString -> IO CString
foreign import ccall "strdup" strdup :: CString -> IO CString

-- strcmp
foreign import ccall "ft_strcmp" ft_strcmp :: CString -> CString -> CInt
foreign import ccall "strcmp" strcmp :: CString -> CString -> CInt

strcmp' :: CString -> CString -> Bool
strcmp' a b =  strcmp a b == 0

-- memset
foreign import ccall "ft_memset" ft_memset :: Ptr () -> CInt -> Int -> IO (Ptr ())
foreign import ccall "memset" memset :: Ptr () -> CInt -> Int -> IO (Ptr ())

-- memcmp
foreign import ccall "ft_memcmp" ft_memcmp :: Ptr () -> Ptr () -> Int -> CInt
foreign import ccall "memcmp" memcmp :: Ptr () -> Ptr () -> Int -> CInt

-- memcmp
foreign import ccall "ft_memcpy" ft_memcpy :: Ptr () -> Ptr () -> Int -> Ptr ()
foreign import ccall "memcpy" memcpy :: Ptr () -> Ptr () -> Int -> Ptr ()

memeq :: Ptr () -> Ptr () -> Int -> Bool
memeq a b i = memcmp a b i == 0

-- strjoin
foreign import ccall "ft_strjoin" ft_strjoin :: CString -> CString -> IO CString

-- atoi
foreign import ccall "ft_atoi" ft_atoi :: CString -> CInt
foreign import ccall "atoi" atoi :: CString -> CInt

-- itoa
foreign import ccall "ft_itoa" ft_itoa :: CInt -> IO CString

-- strtrim
foreign import ccall "ft_strtrim" ft_strtrim :: CString -> IO CString
