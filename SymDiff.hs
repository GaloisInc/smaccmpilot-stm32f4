module SymDiff (
    Sym, var, eval,
    diff, jacobian
) where

import Data.Ratio

data Sym var
    = Const Rational
    | Var var
    | Add (Sym var) (Sym var)
    | Mul (Sym var) (Sym var)
    | Pow (Sym var) (Sym var)
    | Ln (Sym var)
    | Exp (Sym var)
    | Sin (Sym var)
    | Cos (Sym var)
    | ArcSin (Sym var)
    | ArcCos (Sym var)
    | ArcTan (Sym var)

instance Show var => Show (Sym var) where
    show (Const v) = show (fromRational v :: Double)
    show (Var n) = show n
    show (Add a (Mul (Const c) b)) | c < 0 = "(" ++ show a ++ " - " ++ show (Const (negate c) * b) ++ ")"
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a (Pow b (Const c))) | c < 0 = "(" ++ show a ++ " / " ++ show (b ** Const (negate c)) ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Pow a b) = case b of
        Const c -> case denominator c of
            2 -> "sqrt(" ++ show (a ** (fromIntegral $ numerator c)) ++ ")"
            3 -> "cbrt(" ++ show (a ** (fromIntegral $ numerator c)) ++ ")"
            _ -> generic
        _ -> generic
        where generic = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Ln a) = "ln " ++ show a
    show (Exp a) = "e ^ " ++ show a
    show (Sin a) = "sin " ++ show a
    show (Cos a) = "cos " ++ show a
    show (ArcSin a) = "asin " ++ show a
    show (ArcCos a) = "acos " ++ show a
    show (ArcTan a) = "atan " ++ show a

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
    recip a = a ** Const (-1)
    fromRational = Const

instance Floating (Sym var) where
    pi = error "Pi isn't symbolic. It's real."
    sqrt x = x ** 0.5
    exp = simplifyOnce . Exp
    log = simplifyOnce . Ln
    a ** b = simplifyOnce $ Pow a b
    sin = simplifyOnce . Sin
    cos = simplifyOnce . Cos
    asin = simplifyOnce . ArcSin
    acos = simplifyOnce . ArcCos
    atan = simplifyOnce . ArcTan

instance Functor Sym where
    fmap _ (Const x) = Const x
    fmap f (Var v) = Var (f v)
    fmap f (Add a b) = Add (fmap f a) (fmap f b)
    fmap f (Mul a b) = Mul (fmap f a) (fmap f b)
    fmap f (Pow a b) = Pow (fmap f a) (fmap f b)
    fmap f (Ln a) = Ln (fmap f a)
    fmap f (Exp a) = Exp (fmap f a)
    fmap f (Sin a) = Sin (fmap f a)
    fmap f (Cos a) = Cos (fmap f a)
    fmap f (ArcSin a) = ArcSin (fmap f a)
    fmap f (ArcCos a) = ArcCos (fmap f a)
    fmap f (ArcTan a) = ArcTan (fmap f a)

descend :: (Sym var -> Sym var) -> Sym var -> Sym var
descend _ (Const x) = Const x
descend _ (Var v) = Var v
descend f (Add a b) = Add (f a) (f b)
descend f (Mul a b) = Mul (f a) (f b)
descend f (Pow a b) = Pow (f a) (f b)
descend f (Ln a) = Ln (f a)
descend f (Exp a) = Exp (f a)
descend f (Sin a) = Sin (f a)
descend f (Cos a) = Cos (f a)
descend f (ArcSin a) = ArcSin (f a)
descend f (ArcCos a) = ArcCos (f a)
descend f (ArcTan a) = ArcTan (f a)

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
simplifyOnce (Pow (Pow x a) b) = x ** (a * b)
simplifyOnce (Pow (Exp a) b) = exp (a * b)
simplifyOnce (Ln (Exp x)) = x
simplifyOnce (Exp (Ln x)) = x
simplifyOnce (Exp (Add a b)) = exp a * exp b
simplifyOnce (Ln (Mul a b)) = log a + log b
simplifyOnce (Ln (Pow a b)) = b * log a
simplifyOnce (ArcSin (Sin x)) = x
simplifyOnce (Sin (ArcSin x)) = x
simplifyOnce (ArcCos (Cos x)) = x
simplifyOnce (Cos (ArcCos x)) = x
simplifyOnce e = e

diff :: Eq var => var -> Sym var -> Sym var
diff _ (Const _) = 0
diff wrt (Var v) = if v == wrt then 1 else 0
diff wrt (Add a b) = diff wrt a + diff wrt b
diff wrt (Mul a b) = diff wrt a * b + a * diff wrt b
diff wrt (Pow a (Const b)) = (a ** Const (b - 1)) * diff wrt a * Const b -- simplify the most common power-rule case
diff wrt (Pow a b) = Pow a b * (diff wrt a * b / a + diff wrt b * log a)
diff wrt (Ln a) = recip a
diff wrt (Exp a) = Exp a
diff wrt (Sin a) = cos a * diff wrt a
diff wrt (Cos a) = negate (sin a) * diff wrt a
diff wrt (ArcSin a) = recip (sqrt (1 - a ** 2)) * diff wrt a
diff wrt (ArcCos a) = negate (recip (sqrt (1 - a ** 2))) * diff wrt a
diff wrt (ArcTan a) = recip (a ** 2 + 1) * diff wrt a

jacobian :: Eq var => [Sym var] -> [var] -> [[Sym var]]
jacobian fns vars = [ [ diff var fn | var <- vars ] | fn <- fns ]

eval :: Floating a => Sym a -> a
eval (Const x) = fromRational x
eval (Var v) = v
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Pow a b) = eval a ** eval b
eval (Ln a) = log $ eval a
eval (Exp a) = exp $ eval a
eval (Sin a) = sin $ eval a
eval (Cos a) = cos $ eval a
eval (ArcSin a) = asin $ eval a
eval (ArcCos a) = acos $ eval a
eval (ArcTan a) = atan $ eval a
