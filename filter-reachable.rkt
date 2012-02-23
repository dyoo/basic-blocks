#lang racket/base

(require "basic-blocks.rkt"
         racket/list)

(provide filter-reachable)

;; Given a sequence of basic blocks, returns the sequence of those
;; reachable by starting at an entry block and jumping.  Basic DFS.
(define (filter-reachable bblocks)
  (define ht (make-hasheq))
  (for ([b bblocks])
    (hash-set! ht (bblock-name b) b))

  
  (define visited (make-hasheq))  
  (define (dfs queue)
    (cond
     [(empty? queue)
      (void)]
     [(hash-has-key? visited (first queue))
      (dfs (rest queue))]
     [else
      (hash-set! visited (first queue) #t)
      (dfs (append (for/list ([neighbor (bblock-succs (hash-ref ht (first queue)))]
                              #:when (not (hash-has-key? visited neighbor)))
                             neighbor)
                   (rest queue)))]))
  (dfs (for/list ([b bblocks]
                  #:when (bblock-entry? b))
                 (bblock-name b)))


  (for/list ([b bblocks]
             #:when (hash-has-key? visited (bblock-name b)))
            b))
