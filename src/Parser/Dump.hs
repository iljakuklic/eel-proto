
module Parser.Dump(dumpTerm, dumpMeta, printStack, printFuncs) where

import Parser.State
import Sema.Term
import Builtins.Types

import qualified Text.PrettyPrint.HughesPJ as P
import Text.PrettyPrint.HughesPJ((<>), (<+>), ($+$), ($$))
import qualified Data.Map as M
import Text.Printf

-- | create a block from curly braces
block hd doc = hd <+> P.lbrace $+$ P.nest 4 doc $+$ P.rbrace

-- | dump entry w/ key
entry hd cnt = P.text hd <> P.text ": " $$ P.nest 20 cnt

-- | convert metadata to a formatted document
meta2doc (Meta ty pos) = info
  where
    info = entry "Source location" (loc pos)  $+$
           entry "Type"            (genTy ty)
    unk = P.text "unknown"
    loc NoPos = unk
    loc (HasPos path b e) = P.text (show path) <+> lineCol b <+> P.text "--" <+> lineCol e
    lineCol (l, c) = P.parens (P.int l <> P.text ":" <> P.int c)
    genTy NoType = unk
    genTy (HasType t) = P.text (either show show t)

-- | render metadata to a formatted string
dumpMeta = show . meta2doc

-- | convert term to a formatted document
term2doc term = block (P.text hd) (termexpr $$ metadoc $$ cnt)
  where
    metadoc = meta2doc (getMeta term)
    termexpr = entry "Expression" (P.text . show $ term)
    (hd, cnt) = termd term
    termd (TFunc _ _ _) = ("<function>", P.empty)
    termd (TComp _ f g) = ("<composition>", term2doc f $+$ term2doc g)
    termd (TQuot _ f)   = ("<quotation>", term2doc f)
    termd _ = ("<constant>", P.empty)

-- | render term dump to a string
dumpTerm = show . term2doc

-- | Print function definitions
printFuncs :: PState s c Meta -> IO ()
printFuncs (PState st _ _ _) = sequence_ [ printFunc n f | (n, f) <- M.toList st]
printFunc  n (FDUser    d) = printFunc' n (either show show $ termType d) (show d)
printFunc  n (FDBuiltIn b) = printFunc' n (show $ builtInType b) "<<built-in>>"
printFunc' n t d = printf "%-10s : %-50s = %s\n" (show n) t d

-- | Print stack contents, one item per line
printStack (PState _ _ (Stack stk) _) = mapM_ (putStrLn . show) stk
