
module Parser.State where

import Sema.Common
import qualified Data.Map as M
import Sema.Types
import Sema.Term
import Text.Parsec

import Parser.Rule

data PState m = PState {
        pSymTable :: SymTable m,
        pStack    :: Stack,
        pRules    :: RuleTable m
    }

data Meta = Meta {
        mType     :: Type Symbol,
        mPosBegin :: SourcePos,
        mPosEnd   :: SourcePos
    }

