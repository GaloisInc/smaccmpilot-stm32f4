module SymDiff (
    VarName(..), Sym, var, (^.),
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
    | Pow Sym Rational

instance Show Sym where
    show (Const v) = show (fromRational v :: Double)
    show (Var n) = show n
    show (Add a (Mul (Const (-1)) b)) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a (Pow b (-1))) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Pow a b) = "(" ++ show a ++ " ^ " ++ show (fromRational b :: Double) ++ ")"

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
    recip a = a ^. (-1)
    fromRational = Const

(^.) :: Sym -> Rational -> Sym
a ^. b = simplifyOnce $ Pow a b

descend :: (Sym -> Sym) -> Sym -> Sym
descend _ (Const x) = Const x
descend _ (Var v) = Var v
descend f (Add a b) = Add (f a) (f b)
descend f (Mul a b) = Mul (f a) (f b)
descend f (Pow a b) = Pow (f a) b

simplify :: Sym -> Sym
simplify = simplifyOnce . descend simplify

simplifyOnce :: Sym -> Sym
simplifyOnce (Add (Var v1) (Var v2)) | v1 == v2 = Mul (Const 2) (Var v1)
simplifyOnce (Add (Const 0) x) = x
simplifyOnce (Add (Const a) (Const b)) = Const $ a + b
simplifyOnce (Add (Const a) (Add (Const b) c)) = (Const $ a + b) + c
simplifyOnce (Add (Add a b) c) = a + (b + c)
simplifyOnce (Add a (Const b)) = Const b + a
simplifyOnce (Add a (Add (Const b) c)) = Const b + (a + c)
simplifyOnce (Mul (Var v1) (Var v2)) | v1 == v2 = Pow (Var v1) 2
simplifyOnce (Mul (Const 0) _) = Const 0
simplifyOnce (Mul (Const 1) x) = x
simplifyOnce (Mul (Const a) (Const b)) = Const $ a * b
simplifyOnce (Mul (Const a) (Mul (Const b) c)) = (Const $ a * b) * c
simplifyOnce (Mul (Mul a b) c) = a * (b * c)
simplifyOnce (Mul a (Const b)) = Const b * a
simplifyOnce (Mul a (Mul (Const b) c)) = Const b * (a * c)
simplifyOnce (Pow _ 0) = Const 1
simplifyOnce (Pow x 1) = x
simplifyOnce (Pow (Const a) b) | (b', 0) <- properFraction b = Const $ a ^^ b'
simplifyOnce (Pow (Pow x a) b) = x ^. (a * b)
simplifyOnce e = e

diff :: VarName -> Sym -> Sym
diff _ (Const _) = 0
diff wrt (Var v) = if v == wrt then 1 else 0
diff wrt (Add a b) = diff wrt a + diff wrt b
diff wrt (Mul a b) = diff wrt a * b + a * diff wrt b
diff wrt (Pow a b) = (a ^. (b - 1)) * diff wrt a * Const b

jacobian :: [Sym] -> [VarName] -> [[Sym]]
jacobian fns vars = [ [ diff var fn | var <- vars ] | fn <- fns ]
