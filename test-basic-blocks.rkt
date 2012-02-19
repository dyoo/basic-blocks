#lang racket
(require "basic-blocks.rkt"
         rackunit)


(define (frac stmts)
  (define bblocks (fracture stmts))
  (map bblock-stmts bblocks))


(check-equal? (frac '(entry))
              '((entry)))
