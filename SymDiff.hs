module SymDiff (
    Sym, var, (^.), lnSym,
    diff, jacobian
) where

import Data.Ratio

infixr 8 ^.

data Sym var
    = Const Rational
    | Var var
    | Add (Sym var) (Sym var)
    | Mul (Sym var) (Sym var)
    | Pow (Sym var) (Sym var)
    | Ln (Sym var)

instance Show var => Show (Sym var) where
    show (Const v) = show (fromRational v :: Double)
    show (Var n) = show n
    show (Add a (Mul (Const c) b)) | c < 0 = "(" ++ show a ++ " - " ++ show (Const (negate c) * b) ++ ")"
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a (Pow b (Const c))) | c < 0 = "(" ++ show a ++ " / " ++ show (b ^. Const (negate c)) ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Pow a b) = case b of
        Const c -> case denominator c of
            2 -> "sqrt(" ++ show (a ^. (fromIntegral $ numerator c)) ++ ")"
            3 -> "cbrt(" ++ show (a ^. (fromIntegral $ numerator c)) ++ ")"
            _ -> generic
        _ -> generic
        where generic = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Ln a) = "ln " ++ show a

var :: var -> Sym var
var = Var

instance Num (Sym var) where
    a + b = simplifyOnce $ Add a b
    a * b = simplifyOnce $ Mul a b
    a - b = a + (negate b)
    negate a = a * Const (-1)
    fromInteger = Const . fromInteger
    abs = error "abs of a symbolic expression is not differentiable"
    signum = error "signum of a symbolic expression is not differentiable"

instance Fractional (Sym var) where
    a / b = a * recip b
    recip a = a ^. Const (-1)
    fromRational = Const

(^.) :: Sym var -> Sym var -> Sym var
a ^. b = simplifyOnce $ Pow a b

lnSym :: Sym var -> Sym var
lnSym = simplifyOnce . Ln

descend :: (Sym var -> Sym var) -> Sym var -> Sym var
descend _ (Const x) = Const x
descend _ (Var v) = Var v
descend f (Add a b) = Add (f a) (f b)
descend f (Mul a b) = Mul (f a) (f b)
descend f (Pow a b) = Pow (f a) (f b)
descend f (Ln a) = Ln (f a)

simplify :: Sym var -> Sym var
simplify = simplifyOnce . descend simplify

simplifyOnce :: Sym var -> Sym var
simplifyOnce (Add (Const 0) x) = x
simplifyOnce (Add (Const a) (Const b)) = Const $ a + b
simplifyOnce (Add (Const a) (Add (Const b) c)) = (Const $ a + b) + c
simplifyOnce (Add (Add a b) c) = a + (b + c)
simplifyOnce (Add a (Const b)) = Const b + a
simplifyOnce (Add a (Add (Const b) c)) = Const b + (a + c)
simplifyOnce (Mul (Const 0) _) = Const 0
simplifyOnce (Mul (Const 1) x) = x
simplifyOnce (Mul (Const a) (Const b)) = Const $ a * b
simplifyOnce (Mul (Const a) (Mul (Const b) c)) = (Const $ a * b) * c
simplifyOnce (Mul (Mul a b) c) = a * (b * c)
simplifyOnce (Mul a (Const b)) = Const b * a
simplifyOnce (Mul a (Mul (Const b) c)) = Const b * (a * c)
simplifyOnce (Pow (Const 0) _) = Const 0
simplifyOnce (Pow _ (Const 0)) = Const 1
simplifyOnce (Pow (Const 1) _) = Const 1
simplifyOnce (Pow x (Const 1)) = x
simplifyOnce (Pow (Const a) (Const b)) | denominator b == 1 = Const $ a ^^ numerator b
simplifyOnce (Pow (Pow x a) b) = x ^. (a * b)
simplifyOnce e = e

diff :: Eq var => var -> Sym var -> Sym var
diff _ (Const _) = 0
diff wrt (Var v) = if v == wrt then 1 else 0
diff wrt (Add a b) = diff wrt a + diff wrt b
diff wrt (Mul a b) = diff wrt a * b + a * diff wrt b
diff wrt (Pow a (Const b)) = (a ^. Const (b - 1)) * diff wrt a * Const b -- simplify the most common power-rule case
diff wrt (Pow a b) = Pow a b * (diff wrt a * b / a + diff wrt b * Ln a)
diff wrt (Ln a) = recip a

jacobian :: Eq var => [Sym var] -> [var] -> [[Sym var]]
jacobian fns vars = [ [ diff var fn | var <- vars ] | fn <- fns ]
