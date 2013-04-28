
module Parser.Rule (
        -- * Types
        Rule, RuleTable, Production, RuleCache(..),
        -- * Rule table operations
        matchRules, getParserForRule,
        -- * Rule-based parser generation
        generateParser, generateNamedParser, generateParserForRule
    ) where

import Sema.Symbol
import Sema.Term

import qualified Data.Map as M
import Text.Parsec(try, choice, (<?>), ParsecT, Stream)

-- | Grammar rule table is a mapping from nonterminal symbol to the RHS
type RuleTable m c = M.Map Symbol (RuleCache m c)
-- | Cached rule, i.e. a rule together with a pre-generated parser
data RuleCache m c = RuleCache (Rule m) c
-- | Grammar rules for a nonterminal is a list of productions grouped by priorities
type Rule m        = M.Map Int [Production m]
-- | Grammar production is the rule RHS tepresented as a term
type Production m  = Term m

-- | match rules for given nonterminal
matchRules :: RuleTable m c     -- ^ symbol table w/ production rules
           -> Symbol            -- ^ non-terminal name
           -> [[Production m]]  -- ^ matched rules, grouped and ordered by priority
matchRules rt s = map snd . M.toDescList $ maybe M.empty uncache (M.lookup s rt)

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
generateParserForRule :: Stream s m1 t =>
       (Production m -> ParsecT s u m1 a)    -- ^ term evaluator
    -> RuleTable m c                         -- ^ collection of all grammar rules
    -> Symbol                                -- ^ the nonterminal symbol
    -> ParsecT s u m1 a                      -- ^ resultant parser
generateParserForRule evaluator ruletable sym@(Symbol name) =
    case matchRules ruletable sym of
        [] -> fail ("No such parsing rule: " ++ name)
        ps -> generateNamedParser name evaluator ps

-- | get parser associated w/ given nonterminal from the cache
getParserForRule ruletable name@(Symbol nstr) = maybe err (return . getcache) (M.lookup name ruletable)
  where err = fail ("No parser for nonterminal: " ++ nstr)

-- | get parser definition, ingnoring cache
uncache (RuleCache rule _cache) = rule
-- | get parser from cache
getcache (RuleCache _rule cache) = cache
