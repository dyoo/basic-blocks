#lang racket/base

(require racket/match)

(provide NEXT
         DYNAMIC-JUMP
         (struct-out bblock)
         fracture)


;; Basic blocks
(define-struct bblock (name stmts preds succs) #:mutable)


(define-struct next-jump ())
(define-struct dynamic-jump ())
(define NEXT (make-next-jump))
(define DYNAMIC-JUMP (make-dynamic-jump))



;; fracture: (listof (U stmt label)) -> (listof bblock)
;; Given a sequence of statements and labels, as well as the names of entry points,
;; returns a list of bblocks.
(define (fracture stmts
                  #:entry-names (entry-names '())
                  #:fresh-label (fresh-label default-fresh-label)
                  #:label? (label? default-label?)
                  #:label-name (label-name default-label-name)
                  #:jump? (jump? default-jump?)
                  #:jump-targets (jump-targets default-jump-targets))
  (match stmts
    [(list (? label?) rest ...)
     (void)]
    [else
     (raise-type-error 'fracture "nonempty list of statements beginning with a label" stmts)]))

     
(define (default-label? x)
  (symbol? x))


(define (default-label-name a-label)
  (cond
    [(symbol? a-label) a-label]
    [else
     (raise-type-error 'default-label-name "symbol" a-label)]))

(define (default-fresh-label)
  (gensym 'label))

(define (default-jump? x)
  (match x
    [(list 'goto target)
     #t]
    [(list 'if condition 'goto target)
     #t]
    [else #f]))

(define (default-jump-targets x)
  (match x
    [(list 'goto target)
     target]
    [(list 'if condition 'goto target)
     (list target NEXT)]
    [else
     (raise-type-error 'default-jump-targets "Statement with jump targets" x)]))