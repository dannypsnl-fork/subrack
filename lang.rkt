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

(define-language L1
  (extends SubRack)
  (Expr (e body)
        (- (let* ([var* e*] ...) body* ... body)
           (letrec ([var* e*] ...) body* ... body))))
(define-pass elim-letrec : SubRack (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(letrec ([,var* ,e*] ...) ,body* ... ,body)
         `(let ([,var* ',var*] ...)
            (set! ,var* ,e*) ...
            ,body* ... ,body)])
  (Expr ir))

(define-pass final-pass : SubRack (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [else (elim-letrec
               (elim-let* ir))])
  (Expr ir))

(module+ test
  (define-parser parse-SubRack SubRack)
  (final-pass (parse-SubRack `(let* () 1)))
  ; (language:L1 '(let () 1))
  (final-pass (parse-SubRack `(let* ([x 1]) x)))
  ; (language:L1 '(let ((x 1)) x))
  (final-pass (parse-SubRack `(let* ([x 1] [y x] [z y] [j z]) y)))
  ; (language:L1 '(let ((x 1)) (let ((y x) (z y)) y)))
  (final-pass (parse-SubRack `(letrec ([x y]
                                       [y x]) 1)))
  ; (language:L1 '(let ((x 'x) (y 'y)) (set! x y) (set! y x) 1))
  )
