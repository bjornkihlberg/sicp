# Streams

I'm going to include my little testing framework from **ex01-unit-testing**.

```scheme
(load "libs/unit-test.ss")
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

## Higher level procedures

Let's implement a few more useful constructs for working with streams.

```scheme
(define-with-test
  (it "compares streams"
    (((stream) (stream)) eq? #t)
    (((stream 'a) (stream)) eq? #f)
    (((stream 'a) (stream 'a)) eq? #t)
    (((stream 'a) (stream 'b)) eq? #f)
    (((stream 'a 'b 'c) (stream 'a 'b 'c)) eq? #t)
    (((stream 'a 'b 'c) (stream 'a 'b 'c 'd)) eq? #f)
    (((stream 'a 'b 'c) (stream 'a 'd 'c)) eq? #f))

  (stream-eq? x y)
    (or (and (stream-null? x) (stream-null? y))
        (and (eq? (stream-car x) (stream-car y))
             (stream-eq? (stream-cdr x) (stream-cdr y)))))

(define-with-test
  (it "creates an infinite stream"
    ((add1 0) (lambda (x y) (eq? (stream-car                         x)   y)) 0)
    ((add1 5) (lambda (x y) (eq? (stream-car (stream-cdr             x))  y)) 6)
    ((sub1 5) (lambda (x y) (eq? (stream-car (stream-cdr (stream-cdr x))) y)) 3))

  (stream-iterate f init)
    (stream-cons init (stream-iterate f (f init))))

(define-with-test
  (it "takes the first n elements"
    ((0 (stream 'a 'b)) stream-eq? '())
    ((1 (stream 'a 'b)) stream-eq? (stream 'a)))

  (stream-take n x)
    '())

(define-with-test
  (it "takes the first elements which passes a predicate" (() eq? #f))

  (stream-take-while)
    #f)

(define-with-test
  (it "skips the first n elements" (() eq? #f))

  (stream-skip)
    #f)

(define-with-test
  (it "skips the first elements which passes a predicate" (() eq? #f))

  (stream-skip-while)
    #f)

(define-with-test
  (it "maps elements in stream" (() eq? #f))

  (stream-map)
    #f)

(define-with-test
  (it "filters elements in stream which passes a predicate" (() eq? #f))

  (stream-filter)
    #f)

(define-with-test
  (it "scans elements in stream" (() eq? #f))

  (stream-scan)
    #f)

(define-with-test
  (it "folds elements in stream" (() eq? #f))

  (stream-fold)
    #f)

; (define (f . args) (apply + args))
; (f 1 2 3 4 5)
(define-with-test
  (it "zips n streams with a zipping procedure" (() eq? #f))

  (stream-zip)
    #f)

(define-with-test
  (it "tests whether all values in a stream passes a predicate" (() eq? #f))

  (stream-all?)
    #f)

(define-with-test
  (it "tests whether any values in a stream passes a predicate" (() eq? #f))

  (stream-any?)
    #f)
```
