#lang racket
(require "basic-blocks.rkt"
         rackunit)


(define (frac stmts)
  (define bblocks (fracture stmts))
  (map bblock-stmts bblocks))


(check-equal? (frac '(entry))
              '((entry)))

(check-equal? (frac '(entry
                      (printf "hello world")))
              '((entry
                 (printf "hello world"))))

(check-equal? (frac '(entry
                       (printf "hello world")
                       (printf "this is a test")))
              '((entry
                 (printf "hello world")
                 (printf "this is a test"))))


(check-equal? (frac '(entry
                      (goto entry)))
              '((entry
                 (goto entry))))