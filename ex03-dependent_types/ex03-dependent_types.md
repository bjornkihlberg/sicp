# Dependent types

```scheme
(load "libs/unit-test.ss")
```

---

## Types

- `Atom` inhabited by symbols
  - ex: `'a`
- `Nat` inhabited by natural numbers
  - ex: `0`
  - `0` can also be written `zero`

## Judgements

- *_ is a _* **ex:**
  - `'a` is an `Atom`
  - `'a : Atom`
  - `(cons 'a 'b) : (Pair Atom Atom)`
- *_ is the same _ as _* **ex:**
  - `'a` is the same `Atom` as `'a`
  - `(type-of 'a) ∼ (type-of 'a)`
- *_ is a type* **ex:**
  - `Atom` is a type
  - `Atom : Type`
- *_ and _ are the same type* **ex:**
  - `Atom` and `Atom` are the same type
  - `Atom ∼ Atom`

## Normal form

- simplest possible way to express a value **ex:**
  - `'a` is the normal form of `(car (cons 'a 'b))` of `Atom`
- two expressions that are the same have identical normal forms
- two expressions with identical normal forms are the same
- a normal form is always with respect to a type
- every expression with respect to a type has a normal form
- we can check whether two expressions are the same by comparing their normal form

To define `x` to be `0`, we must first *claim* the judgement that `x` *is a* `Nat`.

```lisp
(claim x Nat)
(define x 0)
```
