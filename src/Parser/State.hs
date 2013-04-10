
module Parser.State (
        PState(..), Meta,
        lookupFunc, pTypeTable
    ) where

import Parser.Rule
import Sema.Term

import Text.Parsec
import Control.Applicative
import qualified Data.Map as M

-- | Parser state
data PState m = PState {
        pSymTable :: SymTable m,  -- ^ symbol table
        pRules    :: RuleTable m, -- ^ rule table
        pStack    :: Stack        -- ^ evaluation stack contents
    }
    deriving (Show)

{-
-- | Parsed terms metadata
data Meta = Meta {
        mType     :: Type Symbol,  -- ^ inferred data type
        mPosBegin :: SourcePos,    -- ^ beginning of the term in source file
        mPosEnd   :: SourcePos     -- ^ end of the term in source file
    }
-}
type Meta = ()

-- | get table mapping symbols to types
pTypeTable = fmap functionType . pSymTable <$> getState

-- | Symbol table lookup
lookupFunc sym = do
    st <- pSymTable <$> getState
    case M.lookup sym st of
        Just fn -> return fn
        Nothing -> fail ("Not in scope: " ++ show sym)
