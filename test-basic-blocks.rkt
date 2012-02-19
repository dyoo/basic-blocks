#lang racket
(require "basic-blocks.rkt"
         rackunit)


(define (frac stmts)
  (define bblocks (fracture stmts))
  (map bblock-stmts bblocks))


(check-equal? (frac '(entry))
              '((entry)))

;; This block should have no successors
(check-equal? (bblock-succs (first (fracture '(entry))))
              (set))

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

;; This one should have a link to itself.
(check-equal? (bblock-succs (first (fracture '(entry
                                               (goto entry)))))
              (set 'entry))


(check-equal? (frac '(entry
                      (goto entry)
                      dead-label))
              '((entry
                 (goto entry))))

(check-equal? (frac '(entry
                      (goto (reg v))
                      dead-label))
              '((entry
                 (goto (reg v)))))

(check-equal? (frac '(entry
                      dead-label
                      (goto entry)))
              '((entry
                 (goto entry))))



(check-true (match (frac '(entry
                           (blah)
                           (baz)
                           (if something goto entry)))
              [(list (list 'entry
                           '(blah)
                           '(baz)
                           '(if something goto entry))
                     (list (? symbol?)))
               #t]
              [else
               #f]))



(check-true (match (frac '(entry
                           (blah)
                           (baz)
                           (if something goto entry)
                           (now do something else)))
              [(list (list 'entry
                           '(blah)
                           '(baz)
                           '(if something goto entry))
                     (list (? symbol?)
                           '(now do something else)))
               #t]
              [else
               #f]))

