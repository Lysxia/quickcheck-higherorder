{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances #-}

module Test.QuickCheck.HigherOrder.Function where

import Control.Applicative (liftA2)
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Semigroup (Semigroup(..))
import Data.Void

import Test.QuickCheck (Arbitrary(..), Gen)

import Test.QuickCheck.HigherOrder.Constructible

infixr 1 :->

type FunName = String
type TypeName = String
type ConName = String

newtype Repr_ a = Repr_ (Repr a)

showRepr_ :: Show (Repr a) => Repr_ a -> Int -> String
showRepr_ (Repr_ r) d = showsPrec d r ""

instance Arbitrary (Repr a) => Arbitrary (Repr_ a) where
  arbitrary = Repr_ <$> arbitrary
  shrink (Repr_ r) = Repr_ <$> shrink r

-- Representation of functions @(a -> r)@.
data a :-> r where
  Const :: r -> a :-> r
  CoApply :: Constructible a => Repr_ a -> (b :-> (a -> b) :-> r) -> (a -> b) :-> r
  Apply :: FunName -> (a -> b) -> (b :-> r) -> (a :-> r)
  Case :: TypeName -> (a -> x) -> r -> Branches x r -> (a :-> r)
  CaseInteger :: TypeName -> (a -> Integer) -> r -> Bin r -> (a :-> r)
  Absurd :: (a -> Void) -> a :-> r

data Branches x r where
  Fail :: Branches x r
  Alt :: Branches x r -> Branches y r -> Branches (Either x y) r
  Pat :: ConName -> Fields x r -> Branches x r

-- Smart constructors to enforce some invariants

coapply :: Constructible a => Repr_ a -> (b :-> (a -> b) :-> r) -> (a -> b) :-> r
coapply _ (Const h) = h
coapply a h = CoApply a h

apply :: FunName -> (a -> b) -> (b :-> r) -> (a :-> r)
apply _ _ (Const r) = Const r
apply fn f h = Apply fn f h

case_ :: TypeName -> (a -> x) -> r -> Branches x r -> (a :-> r)
case_ _ _ r Fail = Const r
case_ tn f r b = Case tn f r b

caseInteger :: TypeName -> (a -> Integer) -> r -> Bin r -> (a :-> r)
caseInteger _ _ r BinEmpty = Const r
caseInteger tn f r b = CaseInteger tn f r b

alt :: Branches x r -> Branches y r -> Branches (Either x y) r
alt Fail Fail = Fail
alt b1 b2 = Alt b1 b2

data Fields x r where
  NoField :: r -> Fields () r
  Field :: Fields x (y :-> r) -> Fields (x, y) r

data Bin r
  = BinEmpty
  | BinAlt r (Bin r) (Bin r)

-- * Random generation

instance (CoArbitrary a, Arbitrary r) => Arbitrary (a :-> r) where
  arbitrary = coarbitrary arbitrary

-- A reimplementation of @CoArbitrary@ from QuickCheck,
-- but instead of functions we generate representations of functions.
class CoArbitrary a where
  coarbitrary :: Gen r -> Gen (a :-> r)

instance CoArbitrary () where
  coarbitrary g = Const <$> g

instance CoArbitrary Void where
  coarbitrary _ = pure (Absurd id)

instance CoArbitrary Integer where
  coarbitrary = genIntegral "Integer" id

instance CoArbitrary Int where
  coarbitrary = genIntegral "Int" fromIntegral

genIntegral :: TypeName -> (a -> Integer) -> Gen r -> Gen (a :-> r)
genIntegral tn f g = CaseInteger tn f <$> g <*> genBin g

genBin :: Gen r -> Gen (Bin r)
genBin g = self where
  self = BinAlt <$> g <*> self <*> self

-- * Pretty printing

data Var = Var String !Int

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

newtype Expr = Expr { unExpr :: PrecDString }

unExpr_ :: Expr -> DString
unExpr_ e = unExpr e 0

type Pattern = Expr

-- Context mapping variables to expressions.
data Ctx = (Var, Expr) :. Ctx

type C a = Ctx -> a

pWild :: Pattern
pWild = Expr (\_ -> sstring "_")

tConst :: String -> Expr
tConst s = Expr (\_ -> sstring s)

eConst :: String -> C Expr
eConst s _ = tConst s

tApp :: Expr -> Expr -> Expr
tApp f x = Expr (sparens 10 (unExpr f 10 % " " ~% unExpr x 11))

-- The patterns are parameterized by a fresh variable.
type CBranches = Var -> C [(Pattern, Expr)]

eCase :: TypeName -> CBranches -> C Expr
eCase tn bs ((v, t) :. vs) = Expr (\_ ->
    "case " ~% unExpr_ t % " :: " ~% tn ~% " of { "
      ~% sBranches (bs v vs)
      % " }" ~% sid)
  where
    p ?-> e = unExpr_ p % " -> " ~% unExpr_ e
    sBranches [] = sid
    sBranches ((p0, e0) : bs_) =
      (p0 ?-> e0) %
      foldr (\(p, e) ebs -> " ; " ~% (p ?-> e) % ebs) sid bs_

eApply :: FunName -> C Expr -> C Expr
eApply f y ((v, t) :. vs) =
  y ((v, tApp (tConst f) t) :. vs)

eCoApply :: Show (Repr a) => Repr_ a -> C Expr -> C Expr
eCoApply a y ((v, t) :. vs) =
  y ((v, tApp t (tConst (showRepr_ a 11))) :. vs)

eAbsurd :: C Expr
eAbsurd ((_, t) :. _) = Expr (\_ -> "case " ~% unExpr_ t % " of {}" ~% sid)

eFun :: forall a r. (r -> C Expr) -> (a :-> r) -> C Expr
eFun prettyR = go where
  go :: forall b. (b :-> r) -> C Expr
  go (Absurd _) = eAbsurd
  go (Const r) = prettyR r
  go (CoApply a h) = eCoApply a (eFun go h)
  go (Apply fn _ h) = eApply fn (go h)
  go (Case tn _ r b) = eCase tn (appendIf (partialBranches b) (eBranches prettyR b) (bWild (prettyR r)))
  go (CaseInteger tn _ r b) = eCase tn (pBin prettyR b <> (bWild (prettyR r)))

appendIf :: Semigroup m => Bool -> m -> m -> m
appendIf True = (<>)
appendIf False = const

partialBranches :: Branches x r -> Bool
partialBranches Fail = True
partialBranches (Pat _ _) = False
partialBranches (Alt b1 b2) = partialBranches b1 || partialBranches b2

bWild :: C Expr -> CBranches
bWild e _ vs = [(pWild, e vs)]

prettyFun :: forall a r. (r -> C Expr) -> (a :-> r) -> String
prettyFun prettyR h = unExpr_ (eFun prettyR h defCtx) ""

defCtx :: Ctx
defCtx = addVar [Var "a" 0] badCtx

badCtx :: Ctx
badCtx = addVar [Var "unknown" 0] badCtx

eBranches :: forall x r. (r -> C Expr) -> Branches x r -> CBranches
eBranches prettyR = go where
  go :: forall y. Branches y r -> CBranches
  go Fail = \_ _ -> []
  go (Alt b1 b2) = (liftA2 . liftA2) (++) (go b1) (go b2)
  go (Pat cn d) = eFields prettyR d cn []

eFields :: forall x r. (r -> C Expr) -> Fields x r -> ConName -> [Var] -> CBranches
eFields prettyR = go where
  go :: forall y. Fields y r -> ConName -> [Var] -> CBranches
  go (NoField h) cn ps _ vs = [(mkPattern cn ps, prettyR h (addVar ps vs))]
  go (Field d) cn ps v vs = eFields (eFun prettyR) d cn (v' : ps) v' vs where
    v' = nextVar v

nextVar :: Var -> Var
nextVar (Var s i) = Var s (i + 1)

mkPattern :: ConName -> [Var] -> Pattern
mkPattern cn vs = Expr (\_ -> cn ~% foldr (\v s -> " " ~% sVar v % s) sid vs)

sVar :: Var -> DString
sVar (Var s i) = s ~% show i ~% sid

eVar :: Var -> Expr
eVar v = Expr (\_ -> sVar v)

addVar :: [Var] -> Ctx -> Ctx
addVar [] vs = vs
addVar (v : ps) vs = (v, eVar v) :. addVar ps vs

eInt :: Integer -> Expr
eInt n = Expr (\_ -> show n ~% sid)

pBin :: (r -> C Expr) -> Bin r -> CBranches
pBin prettyR b _ vs =
  fmap (\(n, e) -> (eInt n, e))
    (sortBy (comparing fst) (pBin' prettyR b vs))

pBin' :: (r -> C Expr) -> Bin r -> C [(Integer, Expr)]
pBin' prettyR b vs = go_ b where
  go_ BinEmpty = []
  go_ (BinAlt r b0 b1) =
    (0, prettyR r vs) : (go 1 1 b0 . go (-1) (-1) b1) []
  go _ _ BinEmpty k = k
  go i n (BinAlt r b0 b1) k =
    (n, prettyR r vs) : (go i (2 * n) b0 . go i (2 * n + i) b1) k

---

shrinkFun :: forall a r. (r -> [r]) -> (a :-> r) -> [a :-> r]
shrinkFun shrinkR = go where
  go :: forall b. (b :-> r) -> [b :-> r]
  go (Absurd _) = []
  go (Const r) = fmap Const (shrinkR r)
  go (CoApply a h) = fmap (coapply a) (shrinkFun go h) ++ fmap (\a' -> CoApply a' h) (shrink a)
  go (Apply fn f h) = apply fn f <$> go h
  go (Case tn f r b) = Const r : fmap (\b' -> case_ tn f r b') (shrinkBranches shrinkR b)
  go (CaseInteger tn f r b) = root b : fmap (\b' -> caseInteger tn f r b') (shrinkBin shrinkR b) where
    root BinEmpty = Const r
    root (BinAlt r' _ _) = Const r'

shrinkBranches :: forall x r. (r -> [r]) -> Branches x r -> [Branches x r]
shrinkBranches shrinkR = go where
  go :: forall y. Branches y r -> [Branches y r]
  go Fail = []
  go (Alt b1 b2) = Fail : fmap (\b1' -> alt b1' b2) (go b1) ++ fmap (alt b1) (go b2)
  go (Pat cn d) = Fail : fmap (Pat cn) (shrinkFields shrinkR d)

shrinkFields :: forall x r. (r -> [r]) -> Fields x r -> [Fields x r]
shrinkFields shrinkR = go where
  go :: forall y. Fields y r -> [Fields y r]
  go (NoField r) = fmap NoField (shrinkR r)
  go (Field h) = fmap Field (shrinkFields (shrinkFun shrinkR) h)

shrinkBin :: forall r. (r -> [r]) -> Bin r -> [Bin r]
shrinkBin shrinkR = go where
  go BinEmpty = []
  go (BinAlt r b0 b1) =
    BinEmpty : b0 : b1
      :  fmap (\b0' -> BinAlt r b0' b1) (go b0)
      ++ fmap (\b1' -> BinAlt r b0 b1') (go b1)
      ++ fmap (\r' -> BinAlt r' b0 b1) (shrinkR r)
