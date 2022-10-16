
# Sam's "deriving" typechecking plugin

This library implements a proof-of-concept typechecking plugin that
derives instances.  
For demonstration purposes, the plugin implements some optics:

```haskell
{-# OPTIONS_GHC -fplugin=Sam'sPlugin #-}
import Sam'sOptics

data D a = MkD { fld1 :: a, fld2 :: Int# }
  deriving Sam'sOptics via Sam'sPlugin
```

This will derive 'Sam'sHasField' and 'Sam'sGetField' instances for each of
the individual fields of the data type `D`. So we can then typecheck:

```haskell
foo :: D Char -> Char
foo = sam'sGetField @"fld1"

bar :: D a -> Int#
bar = sam'sGetField @"fld2"
```

Note that the typechecker plugin only needs to be enabled in the module in
which one wants to define the instances. The instances will be added to the
typechecker environment at the end of typechecking the module, and from then on
everything will behave as if one had defined instances normally.

## Limitations

This library is mostly demonstrative in nature, so it comes with several
limitations.

### Limitations of the approach

- The instances are added to the typechecker environment at the end of
  typechecking the module. This means they will not be available in the module
  in which the instances are derived. You will have to import the module for the
  instances to be available.  
  This limitation could be circumvented using some tricks that Matthew Pickering
  employed in his `DeriveLift` plugin.
- The plugin doesn't play nicely with GHCi, so reloading a module might cause
  duplicate instances to be added to the instance environment.

### Limitations of this optics-related plugin

The plugin I implemented for optics only handles simple cases, as this aspect
isn't the part of the implementation I was interested in exploring.
So it has some rather strong limitations:

- Only handles data types with a single constructor.
  (Expect a custom type error telling you so.)
- Doesn't handle datatypes with existentials, or GADTs.
  (Expect random crashes, as I haven't implemented a check preventing you
  from attempting this.)

Note however that it **does** handle unboxed types: both the field types and
the overall record type are allowed to be unboxed types.

## How it works

Internally, the `Sam'sOptics` typeclass has a superclass `GenerateSam'sOptics`.
When the user writes a deriving clause as above, he usual `deriving-via`
mechanism will emit a `GenerateSam'sOptics` Wanted constraint because of this
superclass. The constraint solver plugin then picks up this constraint and takes
it as a signal to generate instances. It generates the evidence by generating
Core directly, and adds this to an `IORef`. At the end of typechecking the
module, we then add these instances to the instance environment and their
evidence to the module's bindings, using a "typecheck result action" plugin.

## Why did you do this?

This answers a challenge posed to me by Iceland Jack. I think it points to the
necessity of implement "deriving plugins" in GHC, to avoid the complexities
incurred by trying to implement all of this in a typechecking plugin, and
to limitations inherent to the approach.
