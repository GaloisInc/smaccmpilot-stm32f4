module SymDiff (
    VarName(..), Sym, var, (^.), lnSym,
    diff, jacobian
) where

import Data.String

infixr 8 ^.

newtype VarName = VarName String
    deriving (Eq, Ord)

instance Show VarName where
    show (VarName s) = s

instance IsString VarName where
    fromString = VarName . fromString

data Sym
    = Const Rational
    | Var VarName
    | Add Sym Sym
    | Mul Sym Sym
    | Pow Sym Sym
    | Ln Sym

instance Show Sym where
    show (Const v) = show (fromRational v :: Double)
    show (Var n) = show n
    show (Add a (Mul b (Const (-1)))) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a (Pow b (Const (-1)))) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Pow a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Ln a) = "ln " ++ show a

var :: VarName -> Sym
var = Var

instance Num Sym where
    a + b = simplifyOnce $ Add a b
    a * b = simplifyOnce $ Mul a b
    a - b = a + (negate b)
    negate a = a * Const (-1)
    fromInteger = Const . fromInteger

instance Fractional Sym where
    a / b = a * recip b
    recip a = a ^. Const (-1)
    fromRational = Const

(^.) :: Sym -> Sym -> Sym
a ^. b = simplifyOnce $ Pow a b

lnSym :: Sym -> Sym
lnSym = simplifyOnce . Ln

descend :: (Sym -> Sym) -> Sym -> Sym
descend _ (Const x) = Const x
descend _ (Var v) = Var v
descend f (Add a b) = Add (f a) (f b)
descend f (Mul a b) = Mul (f a) (f b)
descend f (Pow a b) = Pow (f a) (f b)
descend f (Ln a) = Ln (f a)

simplify :: Sym -> Sym
simplify = simplifyOnce . descend simplify

simplifyOnce :: Sym -> Sym
simplifyOnce (Add (Const 0) x) = x
simplifyOnce (Add x (Const 0)) = x
simplifyOnce (Add (Const a) (Const b)) = Const $ a + b
simplifyOnce (Mul (Const 0) x) = Const 0
simplifyOnce (Mul x (Const 0)) = Const 0
simplifyOnce (Mul (Const 1) x) = x
simplifyOnce (Mul x (Const 1)) = x
simplifyOnce (Mul (Const a) (Const b)) = Const $ a * b
simplifyOnce (Pow (Const 0) x) = Const 0
simplifyOnce (Pow x (Const 0)) = Const 1
simplifyOnce (Pow (Const 1) _) = Const 1
simplifyOnce (Pow x (Const 1)) = x
simplifyOnce (Pow (Const a) (Const b)) | (b', 0) <- properFraction b = Const $ a ^^ b'
simplifyOnce e = e

diff :: VarName -> Sym -> Sym
diff _ (Const _) = 0
diff wrt (Var v) = if v == wrt then 1 else 0
diff wrt (Add a b) = diff wrt a + diff wrt b
diff wrt (Mul a b) = diff wrt a * b + a * diff wrt b
diff wrt (Pow a b) = Pow a b * (diff wrt a * b / a + diff wrt b * Ln a)
diff wrt (Ln a) = recip a

jacobian :: [Sym] -> [VarName] -> [[Sym]]
jacobian fns vars = [ [ diff var fn | var <- vars ] | fn <- fns ]
