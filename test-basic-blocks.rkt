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
                      (printf "hello world 1")))
              '((entry
                 (printf "hello world 1"))))

(check-equal? (frac '(entry
                       (printf "hello world 2")
                       (printf "this is a test")))
              '((entry
                 (printf "hello world 2")
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


(check-equal? (fracture '(entry
                          (blah)
                          (baz)
                          (if something goto entry)
                          consequent
                          (now do something else)))
              (list (make-bblock 'entry
                                 #t
                                 '(entry
                                   (blah)
                                   (baz)
                                   (if something goto entry))
                                 (set 'entry 'consequent)
                                 'consequent)
                    (make-bblock 'consequent
                                 #f
                                 '(consequent
                                   (now do something else))
                                 (set)
                                 #f)))


(check-equal? (fracture '(entry
                          (if (= n 0) goto end)
                          consequent
                          (<- acc (* acc n))
                          (<- n (sub1 n))
                          (goto entry)
                          end
                          (goto (reg return))))
              (list (make-bblock 'entry
                                 #t
                                 '(entry
                                   (if (= n 0) goto end))
                                 (set 'end 'consequent)
                                 'consequent)
                    (make-bblock 'consequent
                                 #f
                                 '(consequent
                                   (<- acc (* acc n))
                                   (<- n (sub1 n))
                                   (goto entry))
                                 (set 'entry)
                                 #f)
                    (make-bblock 'end
                                 #f
                                 '(end
                                   (goto (reg return)))
                                 (set DYNAMIC)
                                 #f)))


;; Check to see that #:entry-names is doing the right thing.
(check-equal? (fracture '(entry-1
                          (<- val (* n n))
                          (goto (reg return))
                          
                          entry-2
                          (<- val (sqrt n))
                          (goto (reg return)))
                        #:entry-names '(entry-1 entry-2))
              (list (make-bblock 'entry-1
                                 #t
                                 '(entry-1
                                   (<- val (* n n))
                                   (goto (reg return)))
                                 (set DYNAMIC)
                                 #f)
                    (make-bblock 'entry-2
                                 #t
                                 '(entry-2
                                   (<- val (sqrt n))
                                   (goto (reg return)))
                                 (set DYNAMIC)
                                 #f)))