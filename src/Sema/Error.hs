
module Sema.Error(SemaError(..)) where

import Sema.Types

import Control.Monad.Error


-- | Semantic errors
data SemaError v = SEOccurs v (Type v)         -- ^ type checking: occurs check failed
                 | SEUnify (Type v) (Type v)   -- ^ type checking: unification failed
                 | SESymbol v                  -- ^ symbol lookup failed

instance Error (SemaError v) where


instance (Show v, Eq v) => Show (SemaError v) where
    show (SEOccurs v t) = "Occurs check failed: " ++ show v ++ " = " ++ show t
    show (SEUnify  a b) = "Could not unify: " ++ show a ++ " = " ++ show b
    show (SESymbol s)   = "Symbol lookup failed: " ++ show s
