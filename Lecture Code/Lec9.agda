

data Greeting : Set where
  hello : Greeting
  goodbye : Greeting
-- ^ Haskell equivalent: data Greeting = Hello | Goodbye

greet : Greeting
greet = hello
-- ^ Haskell equivalent: greet = Hello

-- Ctrl+c Ctrl+l: load file
-- Ctrl+c Ctrl+d: deduce type
-- Ctrl+c Ctrl+n: normalise expression

f : Greeting → Greeting
f = λ x → x


data Bool : Set where
  true : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

open import Agda.Builtin.Nat hiding (_+_; _*_; _<_)


_+_ : Nat → Nat → Nat
zero    + y = y
(suc x) + y = suc (x + y)

infixr 10 _+_


open import Agda.Builtin.Int


-- Ctrl+c Ctrl+c: case split
-- Ctrl+c Ctrl-,: information on a hole
-- Ctrl+c Ctrl-space: give solution to the hole


maximum : Nat → Nat → Nat
maximum zero y = y
maximum (suc x) zero = suc x
maximum (suc x) (suc y) = suc (maximum x y)

_*_ : Nat → Nat → Nat
zero * y = zero
suc x * y = y + (x * y)

-- \le ≤
_<_ : Nat → Nat → Bool
x < zero = false
zero < suc y = true
suc x < suc y = x < y

_≤_ : Nat → Nat → Bool
x ≤ y = x < suc y


Ty : Bool → Set
Ty true = Nat
Ty false = Bool

foo : Ty true → Ty false
foo x = x < 5

{-# TERMINATING #-}
f' : Nat → Nat
f' (suc (suc x)) = f' zero
f' (suc x) = f' (suc (suc x))
f' zero = zero
