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

import TeenyTT.Core.Env (Level, Index)
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
    dump (Env {..}) = dump vals

instance Debug Value where
    dump (Lam x clo) = Pp.lambda <> brackets (dumpClo clo x)
    dump Zero = "zero"
    dump (Suc v) = "suc" <+> parens (dump v)
    dump (Cut neu tp) = brackets (dump neu <+> colon <+> dump tp)
    dump (Rel tp small) = "rel" <+> parens (dump tp) <+> parens (dump small)
    dump NatSmall = "nat-small"
    dump (PiSmall base fam) = "pi-small" <+> parens (dump base) <+> parens (dump fam)

instance Debug Type where
    dump (Univ i) = "Type" <+> pretty i
    dump Nat = Pp.nat
    dump (Pi x base tpclo) = Pp.forall <+> parens (dump base) <+> brackets (dumpClo tpclo x)
    dump (El tp val) = "El" <+> parens (dump tp) <+> (dump val)
    dump (ElCut tp neu) = brackets (dump neu <+> colon <+> dump tp)
    dump (Small a univ) = "Small" <+> parens (dump a) <+> parens (dump univ)

instance Debug Neutral where
    dump (Neutral hd sp) = dump hd <+> slash <+> hsep (fmap (parens . dump) sp)

instance Debug Head where
    dump (Local lvl) = dump lvl
    dump (Global lvl _) = dump lvl
    dump (Hole nm) = "?" <> dump nm

instance Debug Frame where
    dump (App tp v) = parens (dump v <+> colon <+> dump tp)

instance (Debug a) => Debug (Clo a) where
    dump (Clo env a) = dump env <+> brackets "x" <+> Pp.turnstile <+> dump a

dumpClo :: (Debug a) => Clo a -> Ident -> Doc ann
dumpClo (Clo env a) x = dump env <+> brackets (dump x) <+> Pp.turnstile <+> dump a
