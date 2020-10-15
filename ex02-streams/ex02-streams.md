# Streams

I'm going to include my little testing framework from **ex01-unit-testing**.

```scheme
(load "unit-test.ss")
```

Contents of `unit-test.ss`:

```lisp
(define (test-failure-report msg test)
  (error #f (format "it \"~a\" failed on ~a" msg test)))

(define-syntax it
  (syntax-rules ()
    ((_ msg ((args ...) t res) ...)
      (lambda (f)
        (and (or (and (t (f args ...) res) f)
                 (test-failure-report msg '((args ...) t res)))
             ...)))))

(define-syntax define-with-test
  (syntax-rules ()
    ((_ t (f args ...) body ...)
      (define f (t (letrec ((f (lambda (args ...) body ...))) f))))))
```

This is how we could define a lazy stream.

```lisp
(lambda () (cons 'a
  (lambda () (cons 'b
    (lambda () (cons 'c '()))))))
```

Let's clean this up with nicer syntax.

```lisp
(define-syntax stream
  (syntax-rules ()
    ((_)              '())
    ((_ arg)          (lambda () (cons arg '())))
    ((_ arg args ...) (lambda () (cons arg (stream args ...))))))
```

Now we can write it this way instead.

```lisp
(stream 'a 'b 'c)
```

## Basic construction procedures

Let's define some procedures for working with streams.

```lisp
(define-with-test
  (it "checks if stream is empty"
    (((stream))          eq? #t)
    (((stream 'a))       eq? #f)
    (((stream 'b 'c 'd)) eq? #f))

  (stream-null? x)
    (not (procedure? x)))

(define-with-test
  (it "evaluates and returns the head"
    (((stream))          eq? '())
    (((stream 'a))       eq? 'a)
    (((stream 'b 'c 'd)) eq? 'b))

  (stream-car x)
    (if (procedure? x) (car (x)) '()))

(define-with-test
  (it "evaluates the head and returns the tail"
    (((stream))    eq? '())
    (((stream 'a)) eq? '()))

  (stream-cdr x)
    (if (procedure? x) (cdr (x)) '()))

(define-syntax stream-cons
  (syntax-rules () ((_ head tail) (lambda () (cons head tail)))))
```

That became a lot easier with unit testing! I tried implementing this in the SICP chapter on streams and it was unbearable working without some kind of static checking.
