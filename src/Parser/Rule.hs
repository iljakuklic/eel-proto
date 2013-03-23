
module Parser.Rule (
        Rule, RuleTable, Production,
        matchRules
    ) where

import Sema.Common
import Sema.Term

import Text.Parsec
import qualified Data.Map as M

-- | Grammar rule table is a mapping from nonterminal symbol to the RHS
type RuleTable m   = M.Map Symbol (Rule m)
-- | Grammar rule is a list of productions grouped by priorities
type Rule m        = M.Map Int [Production m]
-- | Grammar production is the rule RHS tepresented as a term
type Production m  = Term m

-- | match rules for given nonterminal, sorted and grouped by descending priorities
matchRules :: RuleTable m -> Symbol -> [[Production m]]
matchRules rt s = map snd . M.toDescList $ maybe M.empty id (M.lookup s rt)

