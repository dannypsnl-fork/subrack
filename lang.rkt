#lang nanopass

(provide (all-defined-out))

(define (variable? x) (symbol? x))
(define (constant? x)
  (or (number? x)
      (char? x)
      (string? x)))
;;; quoted expression
(define (datum? x) #t)
;;; raw input language
(define-language SubRack
  (terminals
   (variable (var))
   (primitive (primitive))
   (datum (d))
   (constant (constant)))
  (Expr (e body)
        var
        primitive
        'd
        constant
        ;;; forms
        ;;; if
        (if e0 e1) ; auto void e2
        (if e0 e1 e2)
        ;;; let
        (let ([var* e*] ...) body* ... body)
        (let* ([var* e*] ...) body* ... body)
        (letrec ([var* e*] ...) body* ... body)
        ;;; define/set!
        (define var e*)
        (set! var e*)
        ;;; lambda
        (λ (var* ...) body* ... body)
        ;;; application
        (e0 e1 ...)))

(define-language L1
  (extends SubRack)
  (Expr (e body)
        (- (let* ([var* e*] ...) body* ... body)
           (letrec ([var* e*] ...) body* ... body))))

(define-language L2
  (extends L1)
  (Expr (e body)
        (- (let ([var* e*] ...) body* ... body))))
