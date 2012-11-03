/*
    usage: runhaskell Racket/Interpreter.hs < test.rkt
*/
(env)
(list 1 2 3)
(define (plus x y) (+ x y))
(env)
(plus 1 2)
(define
    fact
    (lambda (x)
        (begin
            (define myrange (range 1 x))
            (apply * (myrange))
        )
    )
)
((fact) 100)
(define x 0)
(== 0 (x))

(+ 1 2 3)
(- 3 1)

(define
    (dec x)
    (if (== 0 x)
        '()
        (cons x (dec (- x 1)))
    )
)
(dec 3)
(define (eq x y) (== x y))
(eq 0 1)
(define
    (fib x)
    (if (== 0 x)
        1
        (if (== 1 x)
            1
            (+ (fib (- x 1)) (fib (- x 2)))
        )
    )
)
// don't try to go deeper
(fib 15)
(head (quote dec fib ()))
(/ 1 5 1)

(define (binary op) (op 1 2))
(binary *)

(map / '((10 5) (1 3) (0 1)))

(and #t (== 1 1.0) (> 3 5))
