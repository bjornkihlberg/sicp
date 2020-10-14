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
