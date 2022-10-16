{-# OPTIONS_GHC -dcore-lint #-}

module Example1B where

-- base
import Data.Proxy
import GHC.Exts

-- Sam's optics
import Sam'sOptics

-- Sam's examples
import Example1A

--------------------------------------------------------------------------------

foo :: D1 Int Char (Proxy 3) -> (Int, Char)
foo d = sam'sGetField @"fld1" d

d1 :: D1 Int Char (Proxy 3)
d1 = MkD1 (1728, 'z') 13#

bar :: (Int, Char)
bar = foo d1

quux :: Int
quux = I# (sam'sGetField @"fld2" d1)

frob :: Bool
frob = sam'sGetField @"fld2" $ MkD2 3 False
