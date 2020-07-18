#lang racket/base

(require "lang.rkt")

(eval 1)
(eval (let ([x 1]) x))
(eval (let* ([x 1] [y x]) y))
