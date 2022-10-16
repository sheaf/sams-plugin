{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sam'sOptics
  ( Sam'sGetField(..)
  , Sam'sSetField(..)
  , Sam'sOptics
  , Sam'sPlugin
  ) where

-- base
import Data.Kind
  ( Constraint )
import GHC.Exts
  ( TYPE )

--------------------------------------------------------------------------------

-- | A simple (but representation-polymorphic) 'GetField'-like class.
--
-- Can be derived using Sam's plugin: see 'Sam'sOptics'.
type Sam'sGetField :: forall {k} {s_rep} {a_rep}
                   .  k -> TYPE s_rep -> TYPE a_rep -> Constraint
class Sam'sGetField fld s a | fld s -> a where
  sam'sGetField :: s -> a

-- | A simple (but representation-polymorphic) 'SetField'-like class.
--
-- Can be derived using Sam's plugin: see 'Sam'sOptics'.
type Sam'sSetField :: forall {k} {s_rep} {a_rep}
                   .  k -> TYPE s_rep -> TYPE a_rep -> Constraint
class Sam'sSetField fld s a | fld s -> a where
  sam'sSetField :: a -> s -> s

-- | Deriving instances for `Sam'sOptics` will generate
-- `Sam'sGetField` and `Sam'sSetField` instances.
--
-- > data D a = MkD { fld1 :: Int, fld3 :: a } deriving Sam'Optics
--
-- This is similar to creating lenses using Template Haskell, e.g.
-- with @'mkLenses@.
--
-- Due to a limitation with the plugin, instances derived in this way will not
-- be visible in the module in which they are derived: they will only be visible
-- when importing the module. Sorry!
type Sam'sOptics :: TYPE r -> Constraint
class GenerateSam'sOptics ty => Sam'sOptics ty where

type GenerateSam'sOptics :: TYPE r -> Constraint
class GenerateSam'sOptics ty where {}

-- | An uninhabited datatype used to signpost the deriving strategy using
-- Sam's plugin with @-XDerivingVia@.
--
-- See 'Sam'sOptics'.
data Sam'sPlugin
