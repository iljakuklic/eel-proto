
module Sema.TypesUtil where

import Sema.Types
import Sema.Common


infix 6 |*
infix 5 |+
infixl 4 %
infix 3 ~>

r % t = RCons r t
r1 ~> r2 = TFunc (Signature r1 r2)
t1 |* t2  = TProd t1 t2
t1 |+ t2  = TSum t1 t2

rv x = RVar $ Symbol x
tv x = TVar $ Symbol x

tA = rv "A"
tB = rv "B"
tC = rv "C"
tD = rv "D"
tE = rv "E"
tF = rv "F"
tG = rv "G"

ta = tv "a"
tb = tv "b"
tc = tv "c"
td = tv "d"
te = tv "e"
tf = tv "f"
tg = tv "g"

-- generic
t_ab a b = tA % a     ~> tA % b
t_01 t   = tA         ~> tA % t
t_11 t   = tA % t     ~> tA % t
t_21 t   = tA % t % t ~> tA % t
t_22 t   = tA % t % t ~> tA % t % t
t_1b t   = tA % t     ~> tA % tBool
t_2b t   = tA % t % t ~> tA % tBool
t_listNode t = tMaybe (t |* TList t)

tt = [
    --conbinators
    ("fix", tA % (tA % (tA ~> tB) ~> tB) ~> tB),
    ("dip", tA % tb % (tA ~> tC) ~> tC % tb),
    ("qot", t_ab tb (tC ~> tC % tb)),
    ("cat", tA % (tB ~> tC) % (tC ~> tD) ~> tA % (tB ~> tD)),
    ("dup", tA % tb ~> tA % tb % tb),
    ("zap", tA % tb ~> tA),

    -- product
    ("pair"  , tA % tb %  tc ~> tA % tb |* tc),
    ("unpair", tA % tb |* tc ~> tA % tb %  tc),

    -- sums
    ("lft", t_ab tb (tb |+ tc)),
    ("rgt", t_ab tc (tb |+ tc)),
    ("sel", tA % (tb |+ tc) % (tA % tb ~> tD) % (tA % tc ~> tD) ~> tD),

    -- list
    ("listwrap"  , t_ab (t_listNode tb) (TList tb)),
    ("listunwrap", t_ab (TList tb) (t_listNode tb)),

    -- unit
    ("unit",   t_01 tUnit),
    ("ununit", tA % tUnit ~> tA),

    -- numbers
    ("n0" , t_01 tNum ),
    ("n1" , t_01 tNum),
    ("add", t_21 tNum),
    ("sub", t_21 tNum),
    ("mul", t_21 tNum),
    ("div", t_21 tNum),
    ("ge" , t_2b tNum),

    -- chars
    ("ord", t_ab tChar tNum),
    ("chr", t_ab tNum tChar),

    -- reals
    ("floor", t_ab tReal tNum),
    ("real" , t_ab tNum tReal),
    ("f0"  , t_01 tReal ),
    ("f1"  , t_01 tReal),
    ("fadd", t_21 tReal),
    ("fsub", t_21 tReal),
    ("fmul", t_21 tReal),
    ("fdiv", t_21 tReal),
    ("fge" , t_2b tReal),
    ("fexp", t_11 tReal),

    -- IO
    ("getchar"  , t_01 tChar),
    ("putchar"  , tA % tChar ~> tA),
    ("readfile" , t_ab tString (tMaybe tString)),
    ("writefile", tA % tString % tString % tBool ~> tA % tBool)
  ]

ttPrint = putStr $ unlines $ map ((\(n, t) -> take 12 (n ++ repeat ' ') ++ " : " ++ show t)) tt

