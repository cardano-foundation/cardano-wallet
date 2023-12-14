# Reasoning about Haskell programs

## Synopsis

```agda
{-# OPTIONS --erasure #-}
module Haskell.Reasoning where
```

This module bundles tools for reasoning about Haskell programs in [Agda][] and [Agda2hs][].

The main purpose of the `Haskell.Prelude` module from [Agda2hs][] is to embed Haskell into Agda. The purpose of the `Haskell.Reasoning` module is to provide tools to reason *about* the embedded Haskell.

In other words, the focus of `Haskell.Prelude` is to embed Haskell functions into Agda such that [Agda2hs][] can translate them back to the target language Haskell. In contrast, the focus of the `Haskell.Reasoning` module is to provide Agda functions for proving properties of the Haskell functions; this module is not meant to be translated back to Haskell. Another way to put this is to say that `Haskell.Reasoning` concerns the metatheory (meta = "about") of Haskell.

(Agda2hs already provides many reasoning utilities, such as equational reasoning, and `sym` or `trans` — this module supplies the parts that I believe are missing, and also provides more explanation.)

  [agda]: https://github.com/agda/agda
  [lagda]: https://agda.readthedocs.io/en/v2.6.4/tools/literate-programming.html
  [agda2hs]: https://github.com/agda/agda2hs

## Concepts

### Type Theory

When reasoning about Haskell programs in Agda, we do not rely on set theory and classical logic — instead, Agda implements a dependent **type theory**, which corresponds to a constructive logic. This is a natural choice of logic for reasoning about programs in a functional programming language such as Haskell, because of the Curry-Howards correspondence, which states that "A proof is a program, and the formula it proves is the type for the program".

For an in-depth introduction to type theory and how it relates to functional programming, see also

* S. Thompson, [Type Theory & Functional Programming][thompson99], (1999)

  [thompson99]: https://www.cs.kent.ac.uk/people/staff/sjt/TTFP/ttfp.pdf

### Propositional Logic: Bool vs Set

The distinction between a target language (Haskell) and its metatheory (here formalized in Agda) manifests in a seeming duplication of logical propositions and logical connectives.

In Haskell, the type `Bool` represents truth values, such as `p, q :: Bool`, and the logical connectives like `not` or `(||)` are functions operating on `Bool`. In Agda however, thanks to the Curry-Howard-isomorphism, **logical propositions** are represented as **types**, such as `P Q : Set`, and the logical connectives such as `¬_` and `_⋁_` operate on types. It is important to distinguish `(p || q)`, which is a value in the target language Haskell, from the logical proposition `(P ⋁ Q)`, which expresses that `P` or `Q` hold in the metatheory. Put differently, `p` and `q` are values **in** Haskell, whereas `P` and `Q` are propositions **about** Haskell.

The equality type `_≡_` is able to connect target language and metatheory. For example, `(p ≡ True) ⋁ (q ≡ True)` is a proposition in the metatheory that states that `p` or `q` are equal to the value `True :: Bool`. It is possible to prove that this proposition is equivalent to the proposition `(p || q) ≡ True`, by using the definition of the function `(||)`, but — the first proposition is a statement in the metatheory, whereas the second proposition refers to value in the target language.

When describing properties **about** Haskell programs, using the logical connectives of the metatheory is preferred. If in doubt, ask yourself: Will this proposition be compiled to Haskell code? If not, then `Set` is the right choice.

### Propositional logic: Logical connectives

The logical connectives exported by `Haskell.Reasoning` are taken from the Agda standard library for compatiblity.
However, we use a notation that is more familiar from classical logic.

The standard logical connectives are:

* `⋀` denotes the **logical and** (conjunction).
* `⋁` denotes the **logical or** (disjunction). But note that this denotes the constructive *or* — the law of excluded middle does **not** hold.
* `¬` denotes **logical negation**.

In Agda, first two logical connectives correspond to data types.
The constructors of these data types are

    inl   : A → A ⋁ B
    inr   : B → A ⋁ B
    _and_ : A → B → A ⋀ B   -- infix notation

Negation is the function type

    ¬ A = A → ⊥

together with the observation that a contradiction allows us to conclude anything

    magic : {A : Set} → ⊥ → A

In order to construct proofs of logical propositions, you have to pattern match on the constructors.

We import and rename from the Agda standard library as much as possible:

```agda
open import Data.Sum public using () renaming
  ( _⊎_ to _⋁_
  ; inj₁ to inl
  ; inj₂ to inr
  )
open import Data.Product using () renaming
  ( _×_ to _⋀_
  ; proj₁ to projl
  ; proj₂ to projr
  ; _,′_ to _`and`_
  )

open import Data.Product public
  using (∃; ∃-syntax)

_`witness`_
  : {A : Set} {B : A → Set}
  → (x : A) → (y : B x) → ∃ B
_`witness`_ = Data.Product._,_

infixr 4 _`witness`_

open import Haskell.Prim public using (⊥; magic)

infix 3 ¬_
¬_ : Set → Set
¬ A = A → ⊥
```

For example, here is the statement that the first case of a logical *or* must hold if the second case leads to a contradiction, and its proof:

```agda
first-alternative : ∀ {A B : Set} → (A ⋁ B) → ¬ B → A
first-alternative (inl a) f = a
first-alternative (inr b) f = magic (f b) 
```

More information about proving statements involving these connectives can be found in Wadler's text [Programming Language Foundations in Agda][wadler]:

* `_⊎_; inj₁; inj₂` — [Connectives: Conjunction, disjunction, and implication](https://plfa.github.io/Connectives/)
* ``¬_; ⊥; ⊥-elim`` — [Negation: Negation, with intuitionistic and classical logic](https://plfa.github.io/Negation/). Note that Agda is an intuitionistic logic, not a classical logic, and the law of excluded middle does not hold for propositions.
* ``∃; ∃-syntax`` — [Quantifiers: Universals and existentials](https://plfa.github.io/Quantifiers/)

Unicode symbols:

    ≡   \==     IDENTICAL TO
    ¬   \neg    NOT SIGN
    ⊥   \bot    UP TACK
    ⋁   \Or     N-ARY LOGICAL OR
    ⋀   \And    N-ARY LOGICAL AND
    ∃   \ex     THERE EXISTS
    ∀   \all    FOR ALL

  [wadler]: https://plfa.github.io

### Equational reasoning

One of the, if not *the*, most important methods for proving the correctness of Haskell programs is the method of **equational reasoning**. The idea is to compute with pure functions as one would compute with natural numbers or other mathematical quantitites.

For example, if we assume the equality `map g ∘ map f ≡ map (g ∘ f)` holds for the function `map`, we can compute that the following equality holds as well:

      map h ∘ map g ∘ map f
    ≡ ⟨ apply the property to g and f ⟩
      map h ∘ map (g ∘ f)
    ≡ ⟨ apply the property to h and (g ∘ f) ⟩
      map (h ∘ (g ∘ f))

Here, for each intermediate step, we have given the reason why the equality holds in brackets `⟨⟩` after the `≡` sign. Typically, such a reason describes a replacement of one subexpression by another, equal subexpression.

Haskell's syntax has been optimized specifically for equational reasoning. In particular, every **pattern match** of a [function binding][haskell-syntax-fun] can be read as an equation between the left-hand side and the right-hand side that holds by definition.
For example, consider the following definition of append `++` for lists:

    []     ++ ys = ys
    (x:xs) ++ ys = x:(xs ++ ys)

Here, each of the defining equations can be read as an equality to be used in equational reasoning. [Wadler '87][wadler87] discusses this example in detail and compares it to a definition that uses λ-expressions. If we wish to prove the law of associativity

    (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

then we can reason by structural induction as follows:

* Base case `xs = []`:

          ([] ++ ys) ++ zs
        ≡⟨ definition of ++, first binding ⟩
          ys ++ zs
        ≡⟨ definition of ++, first binding, applied in reverse ⟩
          [] ++ (ys ++ zs)

* Inductive case `xs = x:xs'`:

          (xs ++ ys) ++ zs
        ≡⟨ definition ⟩
          ((x:xs') ++ ys) ++ zs
        ≡⟨ definition of ++, second binding ⟩
          (x:(xs' ++ ys)) ++ zs
        ≡⟨ definition of ++, second binding ⟩
          x:((xs' ++ ys) ++ zs)
        ≡⟨ induction hypothesis ⟩
          x:(xs' ++ (ys ++ zs))
        ≡⟨ definition of ++, second binding, applied in reverse ⟩
          (x:xs') ++ (ys ++ zs)
        ≡⟨ definition ⟩
          xs ++ (ys ++ zs)

Note how the function bindings that define `++` are conveniently used as equations to rewrite expressions.

For more examples of equational in reasoning Haskell, see also

* R. Bird, [A program to solve Sudoku][bird06], (2006)
* G. Hutton, Advanced Function Programming in Haskell, video lecture [AFP 13 - Making Append Vanish: Fast Reverse][hutton-afp-13], (2021)
* G. Hutton, [Programming in Haskell][hutton16], 2nd edition, (2016)

Compared to other methods, equational reasonsing is so economical that it can be used to write proofs by hand. I strongly recommend that you practice equational reasoning with pen and paper, away from a computer. **If you are not confident writing proofs on a piece of paper, it is unlikely that you will be able to consistently write machine-readable proofs that are accepted by Agda.**

Agda allows us to formalize proofs by equational reasoning. The notation is as follows:

* `begin` starts a proof by equational reasoning
* `≡⟨⟩` states an equation that Agda can verify by evaluating both sides to normal form
* `≡⟨ p ⟩` states an equation that is proven by `p : expr1 ≡ expr2`
* `∎` concludes the proof.

For example, the proof by equational reasoning above is formalized in Agda as follows:

```agda
open import Haskell.Prelude

prop
  : ∀ {a : Set} (xs ys zs : List a)
  → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
prop [] ys zs =
  begin
    ([] ++ ys) ++ zs
  ≡⟨⟩
    ys ++ zs
  ≡⟨⟩
    [] ++ (ys ++ zs)
  ∎
prop xs@(x ∷ xs') ys zs =
  begin
    (xs ++ ys) ++ zs
  ≡⟨⟩
    ((x ∷ xs') ++ ys) ++ zs
  ≡⟨⟩
    (x ∷ (xs' ++ ys)) ++ zs
  ≡⟨⟩
    x ∷ ((xs' ++ ys) ++ zs)
  ≡⟨ cong (λ s → x ∷ s) (prop xs' ys zs) ⟩
    x ∷ (xs' ++ (ys ++ zs))
  ≡⟨⟩
    (x ∷ xs') ++ (ys ++ zs)
  ≡⟨⟩
    xs ++ (ys ++ zs)
  ∎
```

For more information on how to do equational reasoning in Agda, we highly recommend

* J. Cockx, [Programming and Proving in Agda][cockx23], (2023)

  [haskell-syntax-fun]: https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-840004.4.3.1
  [wadler87]: https://www.cs.kent.ac.uk/people/staff/dat/miranda/wadler87.pdf
  [bird06]: https://www.cs.tufts.edu/~nr/cs257/archive/richard-bird/sudoku.pdf
  [hutton-afp-13]: https://www.youtube.com/watch?v=WQy7Bzr03R4&list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc&index=14
  [hutton16]: https://www.cs.nott.ac.uk/~pszgmh/pih.html
  [cockx23]: https://github.com/jespercockx/agda-lecture-notes/blob/master/agda.pdf

### Extensional equality

Equality is a surprisingly subtle concept, especially when it comes to function types.

In Agda, we can distinguish three notions of equality:

* **Definitional equality** — Two terms are definitionally equal if they have the same definition. For example, the terms `1` and `suc 0` are considered definitionally equal, because `1` is, in fact, defined as `1 = suc 0`. Note that definitional equality is a concept *about* Agda terms (metatheory) — it cannot be discussed *within* the language Agda.

* **Propositional equality** — Two terms are propositionally equal if they evaluate to the same normal form. Unlike definitional equality, propositional equality is a concept that can be used within Agda, via the equality type, denoted `≡`. If two terms `x` and `y` have the same normal form, then Agda will accept the following proof that they are equal

        proof : x ≡ y
        proof = refl

    Here,

        refl  : ∀ {A : Set} {x : A} → x ≡ x

    is the fact that equality is **reflexive** (`refl`), meaning that two terms which have the same *definition* are equal in the propositional sense. In Agda, "has the same definition" is synonymous within "has the same normal term", so there is some computation involved when comparing definitions, but this fact is unobservable from within Agda.
    
    Propositional equality also satisfies the properties of **symmetry** (`sym`) and **transitivity** (`cong`), as well as the **Leibniz rule** (`cong`)

        sym   : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
        trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
        cong  : {A B : Set} → ∀ (f : A → B) {x y} → x ≡ y → f x ≡ f y

* **Extensional equality** — The above definitions are useful for inductive data types, which typically have a unique normal form, but they don't quite work for functions. Two functions `f` and `g` are considered **extensionally** equal if they give equal results when applied to the same function argument `x`, that is when the proposition

        ∀ x → f x ≡ g x

    holds. This notion is much more lenient than definitional equality, because two functions may always compute the same result, but have significantly different implementations and normal forms, that is `f` could be a simple, but slow implementation, and `g` a complex, but fast implementation.

For the purpose of **reasoning about functions**, it is desirable to adapt extensional equality as the default notion of equality for functions. In Agda, this is not automatic; we need to postulate an axiom that any two functions are considered propositionally equal, `f ≡ g`, if they are extensionally equal. The full axiom reads:
        
        Extensionality : (a b : Level) → Set _
        Extensionality a b =
          {A : Set a} {B : A → Set b} {f g : (x : A) → B x} →
          (∀ x → f x ≡ g x) → f ≡ g

and we now postulate it:

```agda
open import Axiom.Extensionality.Propositional public
  using (Extensionality)

postulate ext : ∀ {a b} → Extensionality a b
```

This axiom is consistent with Agda: There are models of type theory in which this axiom holds true. However, the negation of this axiom is also consistent: there are also models of type theory in which is axiom does not hold. In other words, this axiom is independent — but generally considered to be highly desirable.
