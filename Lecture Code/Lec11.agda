
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

{-# BUILTIN NATURAL Nat #-}

_+_ : Nat → Nat → Nat
zero    + y = y
(suc x) + y = suc (x + y)

_*_ : Nat → Nat → Nat
zero * y = zero
suc x * y = (x * y) + y

infixr 10 _+_
infixr 20 _*_

_^_ : Nat → Nat → Nat
x ^ zero = 1
x ^ suc y = x * (x ^ y)

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

_==_ : Nat → Nat → Bool
zero  == zero  = true
zero  == suc y = false
suc x == zero  = false
suc x == suc y = x == y

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

infixr 5 _::_

length : {A : Set} → List A → Nat
length [] = 0
length (x :: xs) = suc (length xs)

map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x :: xs) = f x :: map f xs

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y

modusPonens : {P Q : Set} → (P → Q) × P → Q
modusPonens (f , x) = f x


data Either (A B : Set) : Set where
  left  : A → Either A B
  right : B → Either A B

cases : {P Q R : Set}
      → Either P Q → (P → R) × (Q → R) → R
cases (left x) (f , g) = f x
cases (right y) (f , g) = g y


ex : {A B C : Set} → (A → B) → (A × C) → (B × C)
ex f p = f (fst p) , snd p


data ⊤ : Set where
  tt : ⊤

data ⊥ : Set where

absurd : {P : Set} → ⊥ → P
absurd ()


-- P ⇒ (Q ⇒ R)    ⇔     (P ∧ Q) ⇒ R

exercise1 : {P Q R : Set} → (P → Q) × (Q → R) → (P → R)
--exercise1 = λ x → λ y → (snd x) ((fst x) y)
exercise1 (f , g) x = g (f x)

exercise2 : {P Q : Set} → (P → ⊥) × (Q → ⊥) → (Either P Q → ⊥)
--exercise2 (f , g) (left x) = f x
--exercise2 (f , g) (right x) = g x
exercise2 fg x = cases x fg

exercise3 : {P Q : Set} → (P × (P → ⊥)) → Q
exercise3 (x , f) = absurd (f x)


¬_ : Set → Set
¬ X = X → ⊥

infix 1 ¬_

exercise4 : {P : Set} → ¬ ¬ (¬ ¬ P → P)
exercise4 p = p (λ f → absurd (f (λ x → p (λ _ → x))))


data IsEven : Nat → Set

data IsEven where
  e-zero : IsEven 0
  e-suc2 : {n : Nat} → IsEven n → IsEven (suc (suc n))

double : Nat → Nat
double zero = zero
double (suc n) = suc (suc (double n))

double-even : (n : Nat) → IsEven (double n)
double-even zero = e-zero
double-even (suc n) = e-suc2 (double-even n)



