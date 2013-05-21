{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}


module Backend.Emit(emitTerm, emitModule, codegen, genTerm) where

import Sema.Term
import Parser.State
import Backend.CodeGen
import Backend.Preamble
import Backend.Runtime

--import Control.Applicative
import qualified Text.PrettyPrint as P
import Text.PrettyPrint((<+>))
import qualified Data.Map as M
import Control.Monad

-- | Emit LLVM code for all entries in a symbol table
emitModule mainName symTab = codegen $ do
    preamble (fmap eelName mainName)
    blankLine
    appendRaw $ P.semi <+> P.text "========= DEFINED FUNCTIONS ==========="
    sequence_ [ blankLine >> emitFunc (show n) t | (n, FDUser t) <- M.toList symTab ]
    blankLine
    anons <- getAnons
    appendRaw $ P.semi <+> P.text "============= QUOTATIONS =============="
    appendRaw . P.vcat $ P.punctuate (P.text "") anons

-- | Emit function code interactively
genTerm = show . codegen . emitFunc "cmdline"

-- | Emit EEL code for a function from a term
emitFunc fname term = generateFunc (eelName fname) cmnt (emitTerm term)
    where cmnt = P.text fname <+> P.colon <+> (P.text . either show show . termType $ term)

-- | EEL naming scheme
eelName nme = "@eel." ++ nme

-- | Emit code for EEL term
emitTerm (TComp _ f g)   = emitTerm f >> emitTerm g
emitTerm (TFunc _ nme t) = emitFD nme t
emitTerm t = do
    comment "Pushing value # to the stack" [t]
    emitVal t >>= push

-- | Emit code for EEL value
emitVal (TInt _ x)    = return $ llVarInt   x
emitVal (TFloat _ x)  = return $ llVarFloat x
emitVal (TChar _ x)   = return $ llVarChar  x
emitVal (TSumA _ x)   = emitVal x >>= mkSum "valA" 0
emitVal (TSumB _ x)   = emitVal x >>= mkSum "valB" 1
emitVal (TPair _ a b) = do va <- emitVal a; vb <- emitVal b; mkPair "pair" va vb
emitVal (TList _ xs)  = foldM mkItem (LLVar llList "null") (reverse xs)
    where mkItem a b  = do emitVal b >>= flip (mkPair "item") a
emitVal (TUnit _)     = return llVarNull
emitVal (TQuot _ f)   = do
    comment "Quotation: #" [f]
    -- backup code generated so far
    codeBak <- getCode
    stkBak <- getStack
    mapCode (const P.empty)
    -- generate quoted function
    [qname] <- freshRaw ["@eelqot."]
    generateFunc qname (P.text "Quotation:" <+> P.text (show f)) (emitTerm f)
    getCode >>= addAnon
    -- restore code
    mapCode (const codeBak)
    setStack stkBak
    -- generate quotation
    mkQotFunc (LLVar llStackFunc qname)
emitVal v = error ("Emiting value: " ++ show v)

-- | Emit function call or builtin code
emitFD nme (FDUser _term)  = comment "Call #" [nme] >> call (eelName $ show nme)
emitFD _nme (FDBuiltIn bi) = comment "Builtin #" [drop 2 (show bi)] >> emitBI bi

-- combinators
emitBI BIzap = pop llRaw "dead" >> return ()
emitBI BIdup = do x <- pop llRaw "dup"; push x; push x
emitBI BIid  = return ()
emitBI BIid2 = return ()
emitBI BIqot = pop llRaw "data" >>= mkQotData >>= push
emitBI BIdip = do fun <- pop llRaw "fun"; bak <- pop llRaw "bak"; runQot fun; push bak
emitBI BIcat = do f2 <- pop llRaw "fun"; f1 <- pop llRaw "bak"; mkQotComp f1 f2 >>= push
emitBI BIfix = pop llRaw "fixq" >>= (\q -> mkQotFixp q >>= push >> runQot q)
-- unit
emitBI BIunit = push (LLVar llRaw "null")
-- pairs
emitBI BIpair = do b <- pop llRaw "aData"; a <- pop llRaw "bData"; mkPair "pair" a b >>= push
emitBI BIunpair = do (a, b) <- pop (llPtr llPair) "pair" >>= unPair "aData" "bData"; push a; push b
-- sums (tagged union)
emitBI BIina = emitInX "unionA" 0
emitBI BIinb = emitInX "unionB" 1
emitBI BIsel = do
    [caseAL, caseBL, endselL] <- freshRaw ["caseA", "caseB", "endselect"]
    bQot <- pop llRaw "bQot"
    aQot <- pop llRaw "aQot"
    (tagI, dataP) <- pop (llPtr llSum) "union" >>= unPair "tagI" "data"
    push dataP
    tag <- instr llBool "tag" "trunc # to i1" [tagI]
    branchCond tag caseBL caseAL
    stk <- getStack
    stkA <- oneCase stk caseAL aQot endselL
    stkB <- oneCase stk caseBL bQot endselL
    label endselL
    phi llStack "stk" [(stkA, caseAL), (stkB, caseBL)] >>= setStack
  where oneCase stk lab qot endl = setStack stk >> label lab >> runQot qot >> branch endl >> getStack
-- list
emitBI BIlistw = do
    (tagI, dataP) <- pop (llPtr $ llStruct [llInt, llList]) "union" >>= unPair "tagI" "maybeList"
    tag <- instr llBool "tag" "trunc # to i1" [tagI]
    instr llList "list" "select #, #, #" [tag, LLVar llList "null", dataP] >>= push
emitBI BIlistu = do
    list <- pop (llPtr llPair) "list"
    isnil <- instr llBool "isnil" "icmp eq #, null" [list]
    tag <- instr llInt "tag" "select #, #, #" [isnil, llVarInt 1, llVarInt 0]
    mkPair "listU" tag list >>= push
-- integers
emitBI BIadd = arith llInt "add"
emitBI BIsub = arith llInt "sub"
emitBI BImul = arith llInt "mul"
emitBI BIdiv = arith llInt "sdiv"
emitBI BIcmp = compar llInt "icmp"
-- floats
emitBI BIfadd = arith llFloat "fadd"
emitBI BIfsub = arith llFloat "fsub"
emitBI BIfmul = arith llFloat "fmul"
emitBI BIfdiv = arith llFloat "fdiv"
emitBI BIfcmp = compar llFloat "fcmp"
emitBI BIfsin = ffunc "sin"
emitBI BIflog = ffunc "log"
emitBI BIfpow = do
    y <- pop llFloat "fpy"; x <- pop llFloat "fpx"
    callRaw llFloat "fpr" ("@llvm.pow.f" ++ show ptrBits) [x, y] >>= push
-- conversions
emitBI BIfloor = pop llFloat "float" >>= \flt -> instr llInt "int" "fptosi # to #" [llVarDoc flt, llTypeDoc llInt]   >>= push
emitBI BIfloat = pop llInt "int" >>= \int -> instr llFloat "float" "sitofp # to #" [llVarDoc int, llTypeDoc llFloat] >>= push
emitBI BIord   = comment "Identity: #" [BIord]
emitBI BIchar  = comment "Identity: #" [BIchar]
-- IO builtins
emitBI BIgetchar = do
    stdin <- load llRaw "stdin" (LLVar (llPtr llRaw) "@stdin")
    chr <- callRaw llIntNative "char" "@fgetc" [stdin]
    c <- if ptrBits > intBits
            then instr llInt "char" "sext # to #" [llVarDoc chr, llTypeDoc llInt]
            else return chr
    isEOF <- instr llBool "isEOF" "icmp slt #, 0" [c]
    tag <- instr llInt "tag" "select #, #, #" [isEOF, llVarInt 1, llVarInt 0]
    mkPair "maybeChar" tag c >>= push
emitBI BIputchar = do
    chr <- pop llInt "char"
    c <- if ptrBits > intBits
            then instr llIntNative "char" "trunc # to #" [llVarDoc chr, llTypeDoc llIntNative]
            else return chr
    stdout <- load llRaw "stdout" (LLVar (llPtr llRaw) "@stdout")
    _ <- callRaw llIntNative "dummy" "@fputc" [c, stdout]
    return ()
-- catch
emitBI _ = return ()

arith ty iname = do y <- pop ty "y"; x <- pop ty "x"; instr ty "r" (iname ++ " #, " ++ llVarName y) [x] >>= push
ffunc fname = do x <- pop llFloat "fx"; callRaw llFloat "fy" ("@llvm." ++ fname ++ ".f" ++ show ptrBits) [x] >>= push
compar ty iname = do
    y <- pop ty "y"; x <- pop ty "x"
    eq  <- instr llBool "eq"  (iname ++ " eq  #, " ++ llVarName y) [x]
    eqi <- instr llInt  "eqi" "zext # to #" [llVarDoc eq, llTypeDoc llInt]
    ge  <- instr llBool "ge"  (iname ++ " sge #, " ++ llVarName y) [x]
    gei <- instr llInt  "gei" "zext # to #" [llVarDoc ge, llTypeDoc llInt]
    geCmp <- mkPair "geCmp" gei (LLVar llRaw "null")
    mkPair "cmp" eqi geCmp >>= push

emitInX nme tagNo = pop llRaw "data" >>= mkSum nme tagNo >>= push
mkSum   nme tagNo = mkPair nme (LLVar llInt $ show (tagNo :: Int))
