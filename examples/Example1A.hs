{-# OPTIONS_GHC -fplugin=Sam'sPlugin #-}

{-# OPTIONS_GHC -ddump-tc-trace -ddump-to-file #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Example1A ( D1(..), D2(..) ) where

-- base
import Data.Kind
import Data.Proxy
import GHC.Exts

-- Sam's optics
import Sam'sOptics

--------------------------------------------------------------------------------

type D1 :: Type -> Type -> Type -> Type
data D1 a b c = MkD1 { fld1 :: (a, b), fld2 :: Int# }

deriving
  via Sam'sPlugin
  instance forall y k (z :: k) x. Sam'sOptics (D1 x y (Proxy z))

type D2 :: Type
data D2 = MkD2 { fld1 :: Int, fld2 :: Bool }
  deriving Sam'sOptics
    via Sam'sPlugin
