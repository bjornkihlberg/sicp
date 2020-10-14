# Unit Testing

A problem encountered regularly is that without a static type system it feels like programming in scheme is like walking in a field of rakes. I want to see if there is a clean way of writing unit tests.

## Basic idea

```lisp
(lambda (f)
  (and (or (and (eq? (f 4) 4) f)
           (error #f "Test failed: it should add (on (4) eq? 7)"))
       (or (and (eq? (f 2 0) 2) f)
           (error #f "Test failed: it should add (on (0) eq? 0)"))
       (or (and (eq? (f 1 4 2) 7) f)
           (error #f "Test failed: it should add (on (1) eq? 2)"))))
```

This is obscure and noisy. Can we make new syntax that allows us to express this in a more familair way?

```lisp
(define-syntax it
  (syntax-rules ()
    ((_ msg ((args ...) t res) ...)
      (lambda (f) (and (or (and (t (f args ...) res) f)
                           (error #f (format "it \"~a\" failed on ~a" msg '((args ...) t res))))
                       ...)))))
```

```lisp
(define mytest (it "adds" ((4)     eq? 4)
                          ((0 2)   eq? 2)
                          ((1 4 2) eq? 6)))

(mytest +)
```

```
Exception: it "adds" failed on ((1 4 2) eq? 6)
```

```lisp
(define mytest (it "adds" ((4)     eq? 4)
                          ((0 2)   eq? 2)
                          ((1 4 2) eq? 7)))

(mytest +)
```

```
#<procedure +>
```

This is really nice when working with the repl because I can test a new definition before I send it to the repl.

```lisp
(define mytest (it "doubles" ((4)  eq?  8)
                             ((0)  eq?  0)
                             ((-1) eq? -2)))

(define f (mytest (lambda (x) (* 3 x))))
```

```
Exception: it "doubles" failed on ((4) eq? 8)
```

Cool! Excellent! This is also a little noisy by the way. I wonder if we can clean it up further.

```lisp
(define-syntax define-with-test
  (syntax-rules ()
    ((_ t (f args ...) body ...) (define f (t (lambda (args ...) body ...))))))
```

```lisp
(define-with-test mytest (f x) (* 3 x))
```

```
Exception: it "doubles" failed on ((4) eq? 8)
```

Mind blown...
