
module Parser.Rule (
        -- * Types
        Rule, RuleTable, Production,
        -- * Rule table operations
        matchRules,
        -- * Rule-based parser generation
        generateParser, generateNamedParser, parserForRule
    ) where

import Sema.Symbol
import Sema.Term

import qualified Data.Map as M
import Text.Parsec(try, choice, (<?>), ParsecT, Stream)

-- | Grammar rule table is a mapping from nonterminal symbol to the RHS
type RuleTable m   = M.Map Symbol (Rule m)
-- | Grammar rule is a list of productions grouped by priorities
type Rule m        = M.Map Int [Production m]
-- | Grammar production is the rule RHS tepresented as a term
type Production m  = Term m

-- | match rules for given nonterminal
matchRules :: RuleTable m       -- ^ symbol table w/ production rules
           -> Symbol            -- ^ non-terminal name
           -> [[Production m]]  -- ^ matched rules, grouped and ordered by priority
matchRules rt s = map snd . M.toDescList $ maybe M.empty id (M.lookup s rt)

-- | generate a parser for a single non-terminal from given ruleset
generateParser :: Stream s m t
               => (a -> ParsecT s u m b)  -- ^ term evaluator
               -> [[a]]                   -- ^ list of productions as returned by 'matchRules'
               -> ParsecT s u m b         -- ^ resulting parser
generateParser evaluator = choice . map inner
    where inner = try . choice . map evaluator

-- | generate a parser with name
generateNamedParser name evaluator prods = generateParser evaluator prods <?> name

-- | lookup productions for given nonterminal and return corresponding parser
parserForRule :: Stream s m1 t =>
       (Production m -> ParsecT s u m1 a)    -- ^ term evaluator
    -> RuleTable m                           -- ^ collection of all grammar rules
    -> Symbol                                -- ^ the nonterminal symbol
    -> ParsecT s u m1 a                      -- ^ resultant parser
parserForRule evaluator ruletable sym@(Symbol name) =
    case matchRules ruletable sym of
        [] -> fail ("No such parsing rule: " ++ name)
        ps -> generateNamedParser name evaluator ps
