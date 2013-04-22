
module Builtins.Builtins(BuiltIn(..), builtInName, allBuiltIns) where

import Sema.Symbol

-- | Built-in functions enumeration
data BuiltIn 
     -- Combinators
     = BIid            -- ^ identity function
     | BIid2           -- ^ identity function with a type restriction
     | BIzap           -- ^ pop the top of the stack
     | BIdup           -- ^ duplicate the top of the stack
     | BIqot           -- ^ quote the top of the stack
     | BIdip           -- ^ dip
     | BIcat           -- ^ quotation composition
     | BIfix           -- ^ fixed point combinator
     -- Unit
     | BIunit          -- ^ unit type
     -- Products
     | BIpair          -- ^ pair constructor
     | BIunpair        -- ^ pair deconstructor
     -- Sums
     | BIina           -- ^ left injection
     | BIinb           -- ^ right injection
     | BIsel           -- ^ selection (sum deconstructor)
     -- Lists
     | BIlistw         -- ^ list wrap by 1 level
     | BIlistu         -- ^ list unwrap by 1 level
     -- Integers
     | BIadd           -- ^ integer addition
     | BIsub           -- ^ integer subtraction
     | BImul           -- ^ integer multiplication
     | BIdiv           -- ^ integer division
     | BIcmp           -- ^ integer comparison
     -- Floats
     | BIfadd          -- ^ floating-point addition
     | BIfsub          -- ^ floating-point subtraction
     | BIfmul          -- ^ floating-point multiplication
     | BIfdiv          -- ^ floating-point division
     | BIfcmp          -- ^ floating-point comparison
     | BIfsin          -- ^ floating-point sine
     | BIfpow          -- ^ floating-point power function
     | BIflog          -- ^ floating-point natural logarithm
     -- Conversions
     | BIfloor         -- ^ floating-to-integer conversion (floor)
     | BIfloat         -- ^ integer-to-floating conversion
     | BIord           -- ^ char-to-integer conversion
     | BIchar          -- ^ integer-to-char conversion
     -- IO builtins
     | BIgetchar       -- ^ read a character from stdin
     | BIputchar       -- ^ write a character to stdout
     | BIreadfile      -- ^ read file contents
     | BIwritefile     -- ^ write file contents
     -- Compiler functions
     | BIdef           -- ^ define a function
     | BIlet           -- ^ let binding
     -- Parser functions
     | BIdefrule       -- ^ define grammar rule
     | BIinvoke        -- ^ invoke grammar rule
     deriving (Show, Bounded, Enum, Eq)


-- | get builtin name
builtInName = Symbol . tail . tail . show

-- | list all builtins
allBuiltIns :: [BuiltIn]
allBuiltIns = [(minBound)..(maxBound)]
