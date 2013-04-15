

module Builtins.Types(builtInType) where

import Builtins.Builtins
import Sema.Types
import Sema.Common

-- | helper functions to create variables
var = TyVar . Symbol
a = var "a"
b = var "b"
c = var "c"
d = var "d"
r = var "r"
-- | Product helper
x = tProd
-- | Sum helper
(//) = tSum
-- | Function helper
(~~>) :: [Type a] -> [Type a] -> Type a
z ~~> y = (row z) `tFunc` (row y)
  where
    row (t:ts) = foldr (flip tProd) t (reverse ts)
    row _ = error "Empty row!"

-- | Unary function type
unary t = [a, t] ~~> [a, t]
-- | Binary function type
binary t = [a, t, t] ~~> [a, t]

-- | get type of a builtin function
builtInType :: BuiltIn -> Type Symbol

-- Combinators
builtInType BIid     = [a] ~~> [a]
builtInType BIdip    = [a, b, [a] ~~> [c]] ~~> [c, b]
builtInType BIid2    = [a, b, b] ~~> [a, b, b]
builtInType BIzap    = [a, b] ~~> [a]
builtInType BIdup    = [a, b] ~~> [a, b, b]
builtInType BIqot    = [a, b] ~~> [a, [c] ~~> [c, b]]
builtInType BIcat    = [a, [b] ~~> [c], [c] ~~> [d]] ~~> [a, [b] ~~> [d]]
builtInType BIfix    = [a, [a, [a] ~~> [b]] ~~> [b]] ~~> [b]
-- Unit
builtInType BIunit   = [a] ~~> [a, tUnit]
-- Products
builtInType BIpair   = [a, b, c] ~~> [a, b `x` c]
builtInType BIunpair = [a, b `x` c] ~~> [a, b, c]
-- Sums
builtInType BIina    = [r, a] ~~> [r, a // b]
builtInType BIinb    = [r, b] ~~> [r, a // b]
builtInType BIsel    = [r, a // b, [r, a] ~~> [c], [r, b] ~~> [c]] ~~> [c]
-- Lists
builtInType BIlistw  = [r, tMaybe (a `x` tList a)] ~~> [r, tList a]
builtInType BIlistu  = [r, tList a] ~~> [r, tMaybe (a `x` tList a)]
-- Integers
builtInType BIadd    = binary tInt
builtInType BIsub    = binary tInt
builtInType BImul    = binary tInt
builtInType BIdiv    = binary tInt
builtInType BIcmp    = [a, tInt, tInt] ~~> [a, tMaybe tBool]
-- Floats
builtInType BIfadd   = binary tFloat
builtInType BIfsub   = binary tFloat
builtInType BIfmul   = binary tFloat
builtInType BIfdiv   = binary tFloat
builtInType BIfcmp   = [a, tFloat, tFloat] ~~> [a, tMaybe tBool]
builtInType BIfsin   = unary tFloat
builtInType BIfpow   = binary tFloat
builtInType BIflog   = unary tFloat
-- Conversions
builtInType BIfloor  = [a, tFloat] ~~> [a, tInt]
builtInType BIfloat  = [a, tInt] ~~> [a, tFloat]
builtInType BIord    = [a, tChar] ~~> [a, tInt]
builtInType BIchar   = [a, tInt] ~~> [a, tChar]
 -- IO builtins
builtInType BIgetchar   = [a] ~~> [a, tMaybe tChar]
builtInType BIputchar   = [a, tChar] ~~> [a]
builtInType BIreadfile  = [a, tString] ~~> [a, tMaybe tString]
builtInType BIwritefile = [a, tString, tString] ~~> [a, tBool]
-- Compiler funcs
builtInType BIdef    = [a, [b] ~~> [c], tString] ~~> [a]
builtInType BIlet    = [a, [b] ~~> [c], tString] ~~> [c]

