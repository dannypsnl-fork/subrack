#lang nanopass

(module+ test
  (require rackunit))

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
        ;;; lambda
        (λ (var* ...) body* ... body)
        ;;; application
        (e0 e1 ...)))

(module+ test
  (define-parser parse-sub-rack SubRack)
  )
