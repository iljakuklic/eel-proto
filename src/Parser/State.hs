
module Parser.State where

import Sema.Common
import qualified Data.Map as M
import Sema.Types
import Sema.Term
import Text.Parsec

import Parser.Rule

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

