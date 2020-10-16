# Functional programming

## Piping

A common issue is nested structures can get difficult to reason about.

```lisp
(add1 (add1 (add1 (add1 (add1 (add1 0))))))
```

We can create a macro that pipes results from right to left.

```lisp
(define-syntax $
  (syntax-rules ()
    ((_ f x) (f x))
    ((_ fs ... f x) ($ fs ... (f x)))))
```

Now we can write the same expression like this.

```lisp
($ add1 add1 add1 add1 add1 add1 0)
```

*This could be written as a procedure but the expression can be simplified during compilation so might as well do it then.*

## Partial application

Now that we can pipe we have another slight inconvenience. We'd need a `lambda` expression for every pipe component that evaluates to a value.

```lisp
($
  (lambda (l) (map sub1 l))
  (lambda (l) (filter even? l))
  (list 0 1 2 3 4 5 6 7 8 9))
```

```lisp
(define-syntax $
  (syntax-rules ()
    ((_ (f xs ...) x)         (f xs ... x))
    ((_ f x)                  (f x))
    ((_ fs ... (f xs ...) x)  ($ fs ... (f xs ... x)))
    ((_ fs ... f x)           ($ fs ... (f x)))))
```

We can now write it this way instead.

```lisp
($
  (map sub1)
  (filter even?)
  (list 0 1 2 3 4 5 6 7 8 9))
```

*This is a neat trick but I'm a little bit concerned this isn't reliable. Yet. If you use a `lambda` expression (which is unecessary), the pipe doesn't pass the value but appends it in the expression. With a proper type system we might be able fix this.*

## Composition

```lisp
(define-syntax ∘
  (syntax-rules ()
    ((_ (f xs ...))         (lambda (x) (f xs ... x)))
    ((_ f)                  f)
    ((_ fs ... (f xs ...))  (lambda (x) ($ fs ... (f xs ... x))))
    ((_ fs ... f)           (lambda (x) ($ fs ... (f x))))))
```

---

This requires more work.
