module Quat where

newtype Quat a = Quat (a, a, a, a)
    deriving (Eq, Show)

instance Functor Quat where
    fmap f (Quat (a, b, c, d)) = Quat (f a, f b, f c, f d)

quatFromList :: [a] -> Quat a
quatFromList [a, b, c, d] = Quat (a, b, c, d)

quatToList :: Quat a -> [a]
quatToList (Quat (a, b, c, d)) = [a, b, c, d]

quatMult :: Num a => Quat a -> Quat a -> Quat a
quatMult (Quat (a1, b1, c1, d1)) (Quat (a2, b2, c2, d2)) = Quat (
        a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2,
        a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2,
        a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2,
        a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
    )

quatRotation :: Num a => Quat a -> [[a]]
quatRotation (Quat (q0, q1, q2, q3)) = [
        [ q0 ^ 2 + q1 ^ 2 - q2 ^ 2 - q3 ^ 2, 2 * (q1 * q2 - q0 * q3), 2 * (q1 * q3 + q0 * q2) ],
        [ 2 * (q1 * q2 + q0 * q3), q0 ^ 2 - q1 ^ 2 + q2 ^ 2 - q3 ^ 2, 2 * (q2 * q3 - q0 * q1) ],
        [ 2 * (q1 * q3 - q0 * q2), 2 * (q2 * q3 + q0 * q1), q0 ^ 2 - q1 ^ 2 - q2 ^ 2 + q3 ^ 2 ]
    ]
