open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List renaming (_∷_ to _::_)

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y

if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

eqBool : Bool → Bool → Bool
eqBool false false = true
eqBool false true = false
eqBool true false = false
eqBool true true = true


_!!_ : {A : Set} → List A → Nat → A
(x :: xs) !! zero    = x
(x :: xs) !! (suc i) = xs !! i
[]        !! i       = {!   !} -- cannot be filled

data Empty : Set where
  -- no constructors

test : Empty
test = [] !! 0




data Flavour : Set where
  cheesy : Flavour
  chocolatey : Flavour

data Food : Flavour → Set where
  pizza : Food cheesy            -- must be cheesy
  cake  : Food chocolatey        -- must be chocolatey
  bread : {f : Flavour} → Food f -- can be cheesy or chocolatey

amountOfCheese : Food cheesy → Nat
amountOfCheese pizza = 100
amountOfCheese bread = 20



data Ingredient : Flavour → Set where
  flour     : {f : Flavour} → Ingredient f
  cheese    : Ingredient cheesy
  tomato    : Ingredient cheesy
  sugar     : Ingredient chocolatey
  chocolate : Ingredient chocolatey
  eggs      : {f : Flavour} → Ingredient f

ingredients : {f : Flavour} → Food f → List (Ingredient f)
ingredients pizza = cheese :: tomato :: flour :: []
ingredients cake = chocolate :: eggs :: flour :: []
ingredients bread = flour :: []



data Vec (A : Set) : Nat → Set where
  []    : Vec A 0
  _::_  : {n : Nat} → A → Vec A n → Vec A (suc n)

infixr 5 _::_

testVec0 : Vec Bool 0
testVec0 = []

testVec1 : Vec Bool 1
testVec1 = true :: []

testVec2 : Vec Bool 2
testVec2 = true :: false :: []

testVec : Vec Bool 3
testVec = 
  let xs = true :: false :: true :: []
  in xs



map : {A B : Set} → {n : Nat} → (A → B) → Vec A n → Vec B n
map f [] = []
map f (x :: xs) = f x :: map f xs

head : {A : Set} → {n : Nat} → Vec A (suc n) → A
head (x :: xs) = x

tail : {A : Set} → {n : Nat} → Vec A (suc n) → Vec A n
tail (x :: xs) = xs

-- xs = tail []   
-- ^ TYPE ERROR

zipVec : {A B : Set} {n : Nat} → Vec A n → Vec B n → Vec (A × B) n
zipVec [] [] = []
zipVec (x :: xs) (y :: ys) = (x , y) :: zipVec xs ys

testZipVec = zipVec (1 :: 2 :: 3 :: []) (4 :: 5 :: 6 :: [])




lookup : {A : Set} {n : Nat} → Vec A n → Nat → A
lookup (x :: xs) zero    = x
lookup (x :: xs) (suc i) = lookup xs i
lookup []        i       = {!   !} -- still cannot be filled



data Fin : Nat → Set where
  zero  : {n : Nat} → Fin (suc n)
  suc   : {n : Nat} → Fin n → Fin (suc n)


three : Fin 5
three = suc (suc (suc zero))


noFinZero : {A : Set} → Fin 0 → A
noFinZero ()

lookupVec : {A : Set} {n : Nat} → Vec A n → Fin n → A
lookupVec [] ()
lookupVec (x :: xs) zero = x
lookupVec (x :: xs) (suc i) = lookupVec xs i

testLookupVec = lookupVec testVec (suc zero)


length : {A : Set} → List A → Nat
length [] = 0
length (_ :: xs) = suc (length xs)

lookupList : {A : Set} → (xs : List A) → Fin (length xs) → A
lookupList (x :: xs) zero = x
lookupList (x :: xs) (suc i) = lookupList xs i

testLookup = lookupVec (true :: false :: []) (suc zero)


data Ty : Set where
  tInt : Ty
  tBool : Ty

data Expr : Ty → Set where
  lit : Nat → Expr tInt
  tru fls : Expr tBool
  plus minus mult : Expr tInt → Expr tInt → Expr tInt
  lt : Expr tInt → Expr tInt → Expr tBool
  eq : {t : Ty} → Expr t → Expr t → Expr tBool
  ite : {t : Ty} → Expr tBool → Expr t → Expr t → Expr t

testExpr : Expr tInt
testExpr = ite (eq (lit 2) (lit 3)) (plus (lit 42) (lit 42)) (mult (lit 5) (lit 7))

Result : Ty → Set
Result tInt = Nat
Result tBool = Bool

eval : {t : Ty} → Expr t → Result t
eval (lit x) = x
eval tru = true
eval fls = false
eval (plus e1 e2) = eval e1 + eval e2
eval (minus e1 e2) = eval e1 - eval e2
eval (mult e1 e2) = eval e1 * eval e2
eval (lt e1 e2) = eval e1 < eval e2
eval (eq {tInt} e1 e2) = eval e1 == eval e2
eval (eq {tBool} e1 e2) = eqBool (eval e1) (eval e2)
eval (ite e1 e2 e3) = if eval e1 then eval e2 else eval e3
