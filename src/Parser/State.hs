
module Parser.State (PState(..), Meta(..), lookupFunc) where

import Parser.Rule
import Sema.Types
import Sema.Term
import Sema.Common

import Text.Parsec
import Control.Applicative
import qualified Data.Map as M

-- | Parser state
data PState m = PState {
        pSymTable :: SymTable m,  -- ^ symbol table
        pRules    :: RuleTable m, -- ^ rule table
        pStack    :: Stack        -- ^ evaluation stack contents
    }

-- | Parsed terms metadata
data Meta = Meta {
        mType     :: Type Symbol,  -- ^ inferred data type
        mPosBegin :: SourcePos,    -- ^ beginning of the term in source file
        mPosEnd   :: SourcePos     -- ^ end of the term in source file
    }

-- | Symbol table lookup
lookupFunc sym = do
    st <- pSymTable <$> getState
    case M.lookup sym st of
        Just term -> return term
        Nothing   -> fail ("Not in scope: " ++ show sym)
