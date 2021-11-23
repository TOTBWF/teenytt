module TeenyTT.Core.Domain
  ( Type(..)
  , Head(..)
  , Frame(..)
  , Neutral(..)
  , Clo(..)
  , Value(..)
  , Env(..)
  -- * Environments
  , bindVal
  , bindTp
  , cloEnv
  -- * Smart Constructors
  , var
  , global
  , hole
  ) where

import GHC.Generics
import Control.DeepSeq

import TeenyTT.Core.Ident
import TeenyTT.Core.Pretty
import TeenyTT.Core.Pretty.Unicode qualified as Pp

import TeenyTT.Core.Env (Level)
import TeenyTT.Core.Env qualified as Env

import TeenyTT.Core.Syntax qualified as S

data Env = Env { tps :: Env.Env Type, vals :: Env.Env Value }
    deriving (Show, Generic)

instance NFData Env

instance Semigroup Env where
    env0 <> env1 = Env { tps = tps env0 <> tps env1, vals = vals env0 <> vals env1 }

instance Monoid Env where
    mempty = Env { tps = mempty, vals = mempty }

-- | A @Clo@ represents some environment, along with a piece of the syntax (IE: an 'S.Term' or 'S.Type')
-- that binds an additional variable.
data Clo a = Clo Env a
    deriving (Show, Generic)

instance NFData a => NFData (Clo a)

data Value
    = Lam Ident (Clo S.Term)
    | Zero
    | Suc Value
    | Cut Neutral Type
    | Rel Type Value
    | NatSmall
    | PiSmall Value Value
    deriving (Show, Generic)

instance NFData Value

-- | A 'Neutral' value consists of some variable that evaluation is stuck on
-- along with a stack of eliminators that are blocked on that variable.
-- We call that variable the 'Head', and those eliminators 'Frame's.
data Neutral = Neutral { hd :: Head, frames :: [Frame] }
    deriving (Show, Generic)

instance NFData Neutral

-- | A 'Head' is some sort of variable that evaluation got stuck on.
data Head
    = Local Level
    | Global Level ~Value
    | Hole Ident
    deriving (Show, Generic)

instance NFData Head

-- | A 'Frame' is some elimination form that is blocked on a 'Head'.
data Frame = App Type ~Value
    deriving (Show, Generic)

instance NFData Frame

data Type
    = Univ Int
    | Nat
    | Pi Ident Type (Clo S.Type)
    | El Type Value
    | ElCut Type Neutral
    | Small Type Type
    deriving (Show, Generic)

instance NFData Type

--------------------------------------------------------------------------------
-- Environments

bindVal :: Value -> Env -> Env
bindVal val env = env { vals = Env.extend (vals env) val }

bindTp :: Type -> Env -> Env
bindTp tp env = env { tps = Env.extend (tps env) tp }

cloEnv :: Clo a -> Env
cloEnv (Clo env _) = env

--------------------------------------------------------------------------------
-- Helpers for constructing domain values.

var :: Level -> Type -> Value
var lvl tp = Cut (Neutral (Local lvl) []) tp

global :: Level -> Value -> Type -> Value
global lvl ~u tp = Cut (Neutral (Global lvl u) []) tp

hole :: Ident -> Type -> Value
hole nm tp = Cut (Neutral (Hole nm) []) tp

--------------------------------------------------------------------------------
-- Pretty Printing

-- NOTE: We only really care about the values here, the type environment isn't
-- of too much interest.
instance Debug Env where
    dump prec (Env {..}) = dump prec vals

instance Debug Value where
    dump prec  (Lam x clo) = Pp.lambda <> brackets (dumpClo clo x)
    dump _     Zero = "zero"
    dump _    (Suc v) = "suc" <+> dump AppPrec v
    dump prec (Cut neu tp) = parensIf (prec >= AnnPrec) (dump AnnPrec neu <+> colon <+> dump AnnPrec tp)
    dump prec (Rel tp small) = parensIf (prec >= AppPrec) $ "rel" <+> dump AppPrec tp <+> dump AppPrec small
    dump _ NatSmall = "nat-small"
    dump prec (PiSmall base fam) = parensIf (prec >= AppPrec) $ "pi-small" <+> dump AppPrec base <+> dump AppPrec fam

instance Debug Type where
    dump prec (Univ i) = parensIf (prec >= AppPrec) $ "Type" <+> pretty i
    dump _    Nat = Pp.nat
    dump prec (Pi x base tpclo) = parensIf (prec >= AppPrec) $ Pp.forall <+> dump AppPrec base <+> brackets (dumpClo tpclo x)
    dump prec (El tp val) = parensIf (prec >= AppPrec) $ "El" <+> dump AppPrec tp <+> dump AppPrec val
    dump prec (ElCut tp neu) = brackets (dump AnnPrec neu <+> colon <+> dump AnnPrec tp)
    dump prec (Small a univ) = parensIf (prec >= AppPrec) $ "Small" <+> dump AppPrec a <+> dump AppPrec univ

instance Debug Neutral where
    dump prec (Neutral hd sp) = dump AnnPrec hd <+> slash <+> hsep (fmap (parens . dump AnnPrec) sp)

instance Debug Head where
    dump _ (Local lvl) = pretty lvl
    dump _ (Global lvl _) = pretty lvl
    dump _ (Hole nm) = "?" <> pretty nm

instance Debug Frame where
    dump prec (App tp v) = parensIf (prec >= AnnPrec) $ dump AnnPrec v <+> colon <+> dump AnnPrec tp

instance (Debug a) => Debug (Clo a) where
    dump prec (Clo env a) = dump NoPrec env <+> brackets "_" <+> Pp.turnstile <+> dump NoPrec a

dumpClo :: (Debug a) => Clo a -> Ident -> Doc ann
dumpClo (Clo env a) x = dump NoPrec env <+> brackets (dump NoPrec x) <+> Pp.turnstile <+> dump NoPrec a
