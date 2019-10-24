{-# LANGUAGE
    GADTs,
    ScopedTypeVariables,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- * Pretty printing of function representations

module Test.QuickCheck.HigherOrder.Function.Pretty where

import Control.Applicative (liftA2)
import Data.List (sortBy)
import Data.Ord (comparing)

import Test.QuickCheck.HigherOrder.Function.Types
  ((:->)(..), Branches(..), Fields(..), Bin(..), ConName, FunName, TypeName)

-- * Interface

prettyFun :: forall a r. (r -> C Expr) -> (a :-> r) -> String
prettyFun prettyR h = unExpr_ (tFun prettyR h defCtx) ""

-- * Implementation

-- ** Strings

type DString = String -> String
type PrecDString = Int -> DString

sid :: DString
sid = id

sstring :: String -> DString
sstring = (++)

(%) :: DString -> DString -> DString
(%) = (.)

(~%) :: String -> DString -> DString
(~%) = (%) . sstring

infixr 1 %, ~%

sparens :: Int -> DString -> PrecDString
sparens d0 s d = if d > d0 then "(" ~% s % ")" ~% sid else s

-- ** Pretty-printed expressions

newtype Expr = Expr { unExpr :: PrecDString }

type Pattern = Expr

unExpr_ :: Expr -> DString
unExpr_ e = unExpr e 0

data Var = Var String !Int

-- Context mapping variables to expressions.
data Ctx = (Var, Expr) :. Ctx

defCtx :: Ctx
defCtx = addVar [Var "a" 0] badCtx

badCtx :: Ctx
badCtx = addVar [Var "unknown" 0] badCtx

-- | Type of values under some context
type C a = Ctx -> a

-- ** Basic expression constructors

-- Naming convention:
-- - s for strings,
-- - e for expressions (Expr),
-- - t for terms (C Expr, expression under some context).

eWild :: Pattern
eWild = Expr (\_ -> sstring "_")

eConst :: String -> Expr
eConst s = Expr (\_ -> sstring s)

tConst :: String -> C Expr
tConst s _ = eConst s

eInt :: Integer -> Expr
eInt n = Expr (\_ -> show n ~% sid)

eApp :: Expr -> Expr -> Expr
eApp f x = Expr (sparens 10 (unExpr f 10 % " " ~% unExpr x 11))

tShow :: Show a => a -> C Expr
tShow a _ = Expr (flip showsPrec a)

sVar :: Var -> DString
sVar (Var s i) = s ~% show i ~% sid

eVar :: Var -> Expr
eVar v = Expr (\_ -> sVar v)

addVar :: [Var] -> Ctx -> Ctx
addVar [] vs = vs
addVar (v : ps) vs = (v, eVar v) :. addVar ps vs

-- ** Main implementation

-- | Pretty-print a function representation.
tFun :: forall a r. (r -> C Expr) -> (a :-> r) -> C Expr
tFun prettyR = go where
  go :: forall b. (b :-> r) -> C Expr
  go (Absurd _) = tAbsurd
  go (Const r) = prettyR r
  go (CoApply a _ h) = tCoApply a (tFun go h)
  go (Apply fn _ h) = tApply fn (go h)
  go (Case tn _ r b) = tCase tn (appendIf (partialBranches b) (tBranches prettyR b) (bWild (prettyR r)))
  go (CaseInteger tn _ r b) = tCase tn (tBin prettyR b <> (bWild (prettyR r)))

tApply :: FunName -> C Expr -> C Expr
tApply f y ((v, t) :. vs) =
  y ((v, eApp (eConst f) t) :. vs)

tCoApply :: Show w => w -> C Expr -> C Expr
tCoApply a y ((v, t) :. vs) =
  y ((v, eApp t (eConst (showsPrec 11 a ""))) :. vs)

tAbsurd :: C Expr
tAbsurd ((_, t) :. _) = Expr (\_ -> "case " ~% unExpr_ t % " of {}" ~% sid)

appendIf :: Semigroup m => Bool -> m -> m -> m
appendIf True = (<>)
appendIf False = const

-- | @True@ if there is a @Fail@ branch.
partialBranches :: Branches x r -> Bool
partialBranches Fail = True
partialBranches (Pat _ _) = False
partialBranches (Alt b1 b2) = partialBranches b1 || partialBranches b2

-- The patterns are parameterized by a fresh variable.
type CBranches = Var -> C [(Pattern, Expr)]

bWild :: C Expr -> CBranches
bWild e _ vs = [(eWild, e vs)]

tCase :: TypeName -> CBranches -> C Expr
tCase tn bs ((v, t) :. vs) = Expr (\_ ->
    "case " ~% unExpr_ t % " :: " ~% tn ~% " of { "
      ~% sBranches (bs v vs)
      % " }" ~% sid)
  where
    p ?-> e = unExpr_ p % " -> " ~% unExpr_ e
    sBranches [] = sid
    sBranches ((p0, e0) : bs_) =
      (p0 ?-> e0) %
      foldr (\(p, e) ebs -> " ; " ~% (p ?-> e) % ebs) sid bs_

tBranches :: forall x r. (r -> C Expr) -> Branches x r -> CBranches
tBranches prettyR = go where
  go :: forall y. Branches y r -> CBranches
  go Fail = \_ _ -> []
  go (Alt b1 b2) = (liftA2 . liftA2) (++) (go b1) (go b2)
  go (Pat cn d) = tFields prettyR d cn []

tFields :: forall x r. (r -> C Expr) -> Fields x r -> ConName -> [Var] -> CBranches
tFields prettyR = go where
  go :: forall y. Fields y r -> ConName -> [Var] -> CBranches
  go (NoField h) cn ps _ vs = [(mkPattern cn ps, prettyR h (addVar ps vs))]
  go (Field d) cn ps v vs = tFields (tFun prettyR) d cn (v' : ps) v' vs where
    v' = nextVar v

nextVar :: Var -> Var
nextVar (Var s i) = Var s (i + 1)

mkPattern :: ConName -> [Var] -> Pattern
mkPattern cn vs = Expr (\_ -> cn ~% foldr (\v s -> " " ~% sVar v % s) sid vs)

tBin :: (r -> C Expr) -> Bin r -> CBranches
tBin prettyR b _ vs =
  fmap (\(n, e) -> (eInt n, e))
    (sortBy (comparing fst) (tBin' prettyR b vs))

tBin' :: (r -> C Expr) -> Bin r -> C [(Integer, Expr)]
tBin' prettyR b vs = go_ b where
  go_ BinEmpty = []
  go_ (BinAlt r b0 b1) =
    (0, prettyR r vs) : (go 1 1 b0 . go (-1) (-1) b1) []
  go _ _ BinEmpty k = k
  go i n (BinAlt r b0 b1) k =
    (n, prettyR r vs) : (go i (2 * n) b0 . go i (2 * n + i) b1) k
