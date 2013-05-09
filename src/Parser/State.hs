
module Parser.State (
        -- * Types
        PState(..), Meta(..), TypeInfo(..), PosInfo(..),
        -- * Metadata manipulation
        (%%), mEps, mergePos, termSetPos, termModifyType, termType, termInferredType, functionDefType,
        -- * State retrieval
        getState, lookupFunc, pTypeTable, pTypeTablePure, lookupParser
    ) where

import Parser.Rule
import Sema.Term
import Sema.Error
import Sema.Types
import Builtins.Types

import Text.Parsec
import Control.Applicative
import qualified Data.Map as M

-- | Parser state
data PState s c m = PState {
        pSymTable :: SymTable m,    -- ^ symbol table
        pRules    :: RuleTable m (Parsec s (PState s c m) ()), -- ^ rule table
        pStack    :: Stack m        -- ^ evaluation stack contents
    }

-- | Type structure specialisation used in metadata
type MType = Type Symbol
-- | Error structure for metadata
type MError = SemaError Symbol

-- | AST node type annotations
data TypeInfo = NoType                         -- ^ type has not been assigned yet
              | HasType (Either MError MType)  -- ^ inferred type or type error

-- | Source file position info
data PosInfo = NoPos  -- ^ position is not known or has none
             | HasPos FilePath (Int, Int) (Int, Int) -- ^ position file and begin/eng coordinates

-- | Merge two (overlapping) positions into one
mergePos NoPos x = x
mergePos x NoPos = x
mergePos (HasPos f1 s1 e1) (HasPos f2 s2 e2) | f1 == f2 = HasPos f1 (s1 `min` s2) (e1 `max` e2)
mergePos _ _ = NoPos

-- | Parsed terms metadata
data Meta = Meta {
        mType     :: TypeInfo,  -- ^ inferred data type
        mPosition :: PosInfo    -- ^ opsition of the term in the source file
    }
--type Meta = ()

-- | Merge two metadata instances
metaMerge (Meta _ p1) (Meta _ p2) = Meta NoType (mergePos p1 p2)
-- | infix operator for metadata mergind
(%%) = metaMerge
-- | empty metadata node
mEps = Meta NoType NoPos

-- | set source position
metaSetPos meta pos = meta { mPosition = pos }
termSetPos term pos = modifyMeta (flip metaSetPos pos) term

-- | term type metadata manipulation
termModifyType f = modifyMeta (\m -> m { mType = f (mType m) } )
-- | get term type if it has one correctly inferred
termType term = case mType (getMeta term) of
    HasType t -> t
    _ -> error "Type not inferred"
-- | get inferred type (one the term should have provided it typechecked)
termInferredType term = case mType (getMeta term) of HasType (Right t) -> Just t; _ -> Nothing

-- | Function definition type
functionDefType (FDUser t)     = termType t
functionDefType (FDBuiltIn bi) = return $ builtInType bi

-- | get table mapping symbols to types
pTypeTable = pTypeTablePure <$> getState
pTypeTablePure = fmap getT . pSymTable
  where
    getT (FDUser term) = termType term
    getT (FDBuiltIn b) = return $ builtInType b

-- | Symbol table lookup
lookupFunc sym = do
    st <- pSymTable <$> getState
    case M.lookup sym st of
        Just fn -> return fn
        Nothing -> fail ("Not in scope: " ++ show sym)

-- | Lookup parser by nonterminal name
lookupParser sym = getState >>= flip getParserForRule sym . pRules
