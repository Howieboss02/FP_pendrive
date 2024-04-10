open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List renaming (_∷_ to _::_)

id : {A : Set} → A → A
id x = x

map : {A B : Set} → (A → B) → List A → List B
map f []         = []
map f (x :: xs)  = f x :: map f xs

length : {A : Set} → List A → Nat
length [] = 0
length (x :: xs) = suc (length xs)


data IsTrue : Bool → Set where
  is-true : IsTrue true

map-length : {A B : Set}
  → (f : A → B) (xs : List A)
  → IsTrue (length (map f xs) == length xs)
map-length f [] = is-true
map-length f (x :: xs') = map-length f xs'


data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x
infix 4 _≡_

{-# BUILTIN EQUALITY _≡_ #-}


quiz : Bool → Set
quiz = λ b → b ≡ true

quiz-test : quiz true
quiz-test = refl


sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

cong : {A B : Set} {x y : A} →
  (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

subst : {A : Set} (P : A → Set)
      → {x y : A} → x ≡ y → P x → P y
subst P refl p = p 


data IsEven : Nat → Set

data IsEven where
  e-zero : IsEven 0
  e-suc2 : {n : Nat} → IsEven n → IsEven (suc (suc n))

cast-IsEven : {k l : Nat} → k ≡ l → IsEven k → IsEven l
cast-IsEven refl x = x

plus-suc : (k l : Nat) → k + suc l ≡ suc (k + l)
plus-suc zero l = refl
plus-suc (suc k) l = cong suc (plus-suc k l)

double-even' : (n : Nat) → IsEven (n + n)
double-even' zero = e-zero
double-even' (suc n) = 
  cast-IsEven 
    (cong suc (sym (plus-suc n n))) 
    (e-suc2 (double-even' n))


begin_ : {A : Set} → {x y : A}
       → x ≡ y → x ≡ y
begin p = p

_end : {A : Set} → (x : A) → x ≡ x
x end = refl

_=⟨_⟩_ : {A : Set} → (x : A) → {y z : A}
        → x ≡ y → y ≡ z → x ≡ z
x =⟨ refl ⟩ q = q

_=⟨⟩_ : {A : Set} → (x : A) → {y : A}
      → x ≡ y → x ≡ y
x =⟨⟩ q = x =⟨ refl ⟩ q

infix   1  begin_
infix   3  _end
infixr  2  _=⟨_⟩_
infixr  2  _=⟨⟩_


not : Bool → Bool
not true = false
not false = true

not-not : (b : Bool) → not (not b) ≡ b
not-not false =
  begin
    not (not false)
  =⟨⟩
    not (not false)
  =⟨⟩
    not (not false)
  end
not-not true = refl


plus-zero : (n : Nat) -> n + zero ≡ n
plus-zero zero =
  begin
    zero + zero
  =⟨⟩
    zero
  end
plus-zero (suc n) =
  begin
    suc n + zero
  =⟨ cong suc (plus-zero n)  ⟩
    suc n
  end


add-assoc : (x y z : Nat) → x + (y + z) ≡ (x + y) + z
add-assoc zero y z = refl
add-assoc (suc x) y z = 
  begin
    (suc x) + (y + z)
  =⟨⟩
    suc (x + (y + z))
  =⟨ cong suc (add-assoc x y z) ⟩
    suc ((x + y) + z)
  =⟨⟩
    (suc x + y) + z
  end


-- \circ or \o
_∘_ : {A B C : Set} → (B → C) → (A → B) → (A → C)
f ∘ g = λ x → f (g x)

list-functor-comp : {A B C : Set} →
  (f : B → C) (g : A → B) (xs : List A) →
  map (f ∘ g) xs ≡ (map f ∘ map g) xs

list-functor-comp f g [] = 
  begin
    map (f ∘ g) []
  =⟨⟩
    []
  =⟨⟩
    map f (map g [])
  =⟨⟩
    (map f ∘ map g) []
  end

list-functor-comp f g (x :: xs) =
  let gxs = map g xs in
  begin
    map (f ∘ g) (x :: xs)
  =⟨⟩
    (f ∘ g) x :: map (f ∘ g) xs
  =⟨ cong (_::_ ((f ∘ g) x)) (list-functor-comp f g xs) ⟩
    (f ∘ g) x :: (map f ∘ map g) xs
  =⟨⟩
    f (g x) :: map f (map g xs)
  =⟨⟩ 
    map f (map g (x :: xs))
  =⟨⟩
    (map f ∘ map g) (x :: xs)
  end
