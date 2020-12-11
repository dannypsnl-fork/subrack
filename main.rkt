#lang racket/base

(require nanopass)
(require "lang.rkt")

(provide eval)
(define-pass elim-let* : SubRack (ir) -> SubRack ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(let* () ,body* ... ,body)
         `(let () ,body* ... ,body)]
        [(let* ([,var ,e]) ,body* ... ,body)
         `(let ([,var ,e]) ,body* ... ,body)]
        [(let* ([,var ,e] [,var* ,e*] ...) ,body* ... ,body)
         `(let ([,var ,e])
            ,(elim-let*
              `(let* ([,var* ,e*] ...) ,body* ... ,body)))])
  (let ([x (Expr ir)])
    (printf "elim-let* ~a\n" ir)
    (printf "elim-let* ~a\n" x)
    x))
(define-pass elim-letrec : SubRack (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(letrec ([,var* ,e*] ...) ,body* ... ,body)
         `(let ([,var* ',var*] ...)
            (set! ,var* ,e*) ...
            ,body* ... ,body)])
  (let ([x (Expr ir)])
    (printf "elim-letrec ~a\n" ir)
    (printf "elim-letrec ~a\n" x)
    x))

(define-pass elim-let : L1 (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(let ([,var* ,e*] ...) ,body* ... ,body)
         `((λ (,var* ...)
             ,(map elim-let body*) ... ,(elim-let body))
           ,e* ...)])
  (Expr ir))
(define-pass L1->L2 : L1 (ir) -> L2 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ())
  (Expr ir))

(define (final-pass ir)
  (L1->L2
   (elim-let
    (elim-letrec
     (elim-let* ir)))))

(define-parser parse-SubRack SubRack)
(final-pass (parse-SubRack `(let* () 1)))
(final-pass (parse-SubRack `(let* ([x 1]) x)))
(final-pass (parse-SubRack `(let* ([x 1] [y x] [z y] [j z]) y)))
(final-pass (parse-SubRack `(letrec ([x y]
                                     [y x]) 1)))
