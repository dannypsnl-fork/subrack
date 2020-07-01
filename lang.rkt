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
        ;;; define/set!
        (define var e*)
        (set! var e*)
        ;;; lambda
        (λ (var* ...) body* ... body)
        ;;; application
        (e0 e1 ...)))

(define-pass elim-complex-let : SubRack (ir) -> SubRack ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(let* () ,body* ... ,body)
         `(let () ,body* ... ,body)]
        [(let* ([,var ,e]) ,body* ... ,body)
         `(let ([,var ,e]) ,body* ... ,body)]
        [(let* ([,var ,e] [,var* ,e*] ...) ,body* ... ,body)
         `(let ([,var ,e])
            ,(elim-complex-let
              `(let* ([,var* ,e*] ...) ,body* ... ,body)))]
        ;;; TODO: fix letrec with correct transformation
        [(letrec ([,var* ,e*] ...) ,body* ... ,body)
         `(let ([,var* ,e*] ...) ,body* ... ,body)])
  (Expr ir))

(define-language L1
  (extends SubRack)
  (Expr (e body)
        (- (let* ([var* e*] ...) body* ... body)
           (letrec ([var* e*] ...) body* ... body))))

(module+ test
  (define-parser parse-SubRack SubRack)
  (elim-complex-let (parse-SubRack `(let* () 1)))
  ; (language:L1 '(let () 1))
  (elim-complex-let (parse-SubRack `(let* ([x 1]) x)))
  ; (language:L1 '(let ((x 1)) x))
  (elim-complex-let (parse-SubRack `(let* ([x 1] [y x] [z y] [j z]) y)))
  ; (language:L1 '(let ((x 1)) (let ((y x) (z y)) y)))
  )
