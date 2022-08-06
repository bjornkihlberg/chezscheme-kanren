(library (kanren)
  (export == conde failure fresh ifte once run stream->list stream-take success)
  (import (chezscheme) (stream))
  (define (var id) (vector 'var id))
  (define (var? x) (and (vector? x) (eq? 'var (vector-ref x 0))))
  (define (var-id x) (assert (var? x)) (vector-ref x 1))
  (define (var=? x y) (and (var? x) (var? y) (eq? (var-id x) (var-id y))))
  (define (walk x s)
    (let loop ([x x])
      (let ([r (assp (lambda (y) (var=? x y)) s)]) (if r (loop (cdr r)) x))))
  (define (extend-s x y s) (cons (cons x y) s))
  (define (unify x y s)
    (let ([x (walk x s)] [y (walk y s)])
      (cond
        [(and (var? x) (var? y) (var=? x y)) s]
        [(var? x) (extend-s x y s)]
        [(var? y) (extend-s y x s)]
        [(and (pair? x) (pair? y))
         (let ([s (unify (car x) (car y) s)]) (and s (unify (cdr x) (cdr y) s)))]
        [else (and (eqv? x y) s)])))
  (define (== x y)
    (lambda (s) (let ([s (unify x y s)]) (if s (stream s) (stream)))))
  (define disj
    (case-lambda
      [(x) x]
      [xs (lambda (s) (stream-flat-map (lambda (x) (x s)) (list->stream xs)))]))
  (define conj
    (case-lambda
      [(x) x]
      [(x y) (lambda (s) (stream-flat-map y (x s)))]
      [(x . xs) (conj x (apply conj xs))]))
  (define-syntax fresh
    (syntax-rules ()
      [(_ (x ...) body ...) ((lambda (x ...) (conj body ...)) (var 'x) ...)]))
  (define-syntax run
    (syntax-rules ()
      [(_ x body ...)
       (stream-map (lambda (s) (walk (var 'x) s)) ((fresh (x) body ...) '()))]))
  (define (success s) (stream s))
  (define (failure s) (stream))
  (define (ifte g0 g1 g2)
    (lambda (s0)
      (let ([s1 (g0 s0)]) (if (null? (s1)) (g2 s0) (stream-flat-map g1 s1)))))
  (define (once g) (lambda (s) (stream-once (g s))))
  (define-syntax conde
    (syntax-rules () [(_ (g ...) ...) (disj (conj g ...) ...)])))
