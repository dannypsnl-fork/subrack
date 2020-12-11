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
  (Expr ir))
(define-pass elim-letrec : SubRack (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(letrec ([,var* ,e*] ...) ,body* ... ,body)
         `(let ([,var* ',var*] ...)
            (set! ,var* ,e*) ...
            ,body* ... ,body)])
  (Expr ir))

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

(define (subst cur-binds)
  (λ (e)
    (define cur-subst (subst cur-binds))
    (with-output-language (L2 Expr)
      (nanopass-case (L2 Expr) e
                     [(λ (,var* ...) ,body* ... ,body)
                      (define new-binds (foldl (λ (var cur-binds)
                                                 (hash-remove cur-binds var))
                                               cur-binds
                                               var*))
                      (define cur-subst (subst new-binds))
                      `(λ (,var* ...)
                         ,(map cur-subst body*) ... ,(cur-subst body))]
                     [(if ,e0 ,e1)
                      `(if ,(cur-subst e0) ,(cur-subst e1))]
                     [(if ,e0 ,e1 ,e2)
                      `(if ,(cur-subst e0) ,(cur-subst e1) ,(cur-subst e2))]
                     [(,e0 ,e1 ...)
                      `(,(cur-subst e0) ,(map cur-subst e1) ...)]
                     ;; no record in binding is normal, for example a nested λ in current target
                     [,var (hash-ref cur-binds var var)]
                     [else e]))))
(define-pass elim-immediate-call : L2 (ir) -> L2 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(,e0 ,e1 ...)
         (nanopass-case (L2 Expr) e0
                        [(λ (,var* ...) ,body* ... ,body)
                         (define (cur-subst e)
                           (elim-immediate-call ((subst (make-immutable-hash (map (λ (v e) (cons v e)) var* e1))) e)))
                         `(begin ,(map cur-subst body*) ... ,(cur-subst body))]
                        [else `(,e0 ,e1 ...)])])
  (Expr ir))

(define (final-pass ir)
  (elim-immediate-call
   (L1->L2
    (elim-let
     (elim-letrec
      (elim-let* ir))))))

(define-parser parse-SubRack SubRack)
(final-pass (parse-SubRack `(let* () 1)))
(final-pass (parse-SubRack `(let* ([x 1]) x)))
(final-pass (parse-SubRack `(let* ([x 1] [y x] [z y]) z)))
