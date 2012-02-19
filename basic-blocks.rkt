#lang racket/base

(require racket/match 
         racket/set
         racket/list)

(provide NEXT
         DYNAMIC-JUMP
         (struct-out bblock)
         fracture)


;; Basic blocks
(define-struct bblock (name  ;; symbol
                       stmts ;; (listof stmt)
                       preds ;; (listof bblock DYNAMIC)
                       succs ;; (listof bblock DYNAMIC)
                       aux   ;; any
                       )
  #:mutable)


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
  (check-good-stmts! stmts label?)

  
  ;; Main loop.  Watch for leaders.
  (let-values ([(leaders stmts)
                (find/inject-leaders stmts entry-names jump? jump-targets
                                     label? label-name fresh-label)])

    ;; leader?: stmt -> boolean
    ;; Returns true if the statement is a leader.
    (define (leader? stmt)
      (and (label? stmt) (set-member? leaders (label-name stmt))))
    
    (let loop ([bblocks '()]
               [pending-block-name (label-name (first stmts))]
               [pending-stmts/rev (list (first stmts))]
               [stmts (rest stmts)])
      (cond
        [(empty? stmts)
         (reverse (cons (make-bblock pending-block-name
                                     (reverse pending-stmts/rev)
                                     '()
                                     '()
                                     #f)
                        bblocks))]
        [(leader? (first stmts))
         (loop (cons (make-bblock pending-block-name
                                  (reverse pending-stmts/rev)
                                  '()
                                  '()
                                  #f)
                     bblocks)
               (label-name (first stmts))
               (list (first stmts))
               (rest stmts))]
        
        [else
         (loop bblocks 
               pending-block-name 
               ;; Omit dead labels.
               (if (label? (first stmts)) pending-stmts/rev 
                   (cons (first stmts) pending-stmts/rev))
               (rest stmts))]))))

  
;; Make sure we get a good list of statements for fracture.
(define (check-good-stmts! stmts label?)
  (match stmts
    [(list (? label?) rest ...)
     (void)]
    [else
     (raise-type-error 'fracture "nonempty list of statements beginning with a label" stmts)]))



;; find/inject-leaders: -> (values (setof symbol) (listof stmt))
;; Preprocesses the statements and computes leaders, and injects them if necessary.
(define (find/inject-leaders stmts entry-names jump? jump-targets label? label-name fresh-label)
  (let loop ([leaders (cons (label-name (first stmts)) entry-names)]
             [stmts-seen/rev (list (first stmts))]
             [stmts-to-see (rest stmts)])
    (cond
      [(empty? stmts-to-see)
       (values (list->set leaders) (reverse stmts-seen/rev))]
      [(jump? (first stmts-to-see))
       (define targets (jump-targets (first stmts-to-see)))
       (define named-targets
         (filter (lambda (t)
                   (and (not (eq? t NEXT))
                        (not (eq? t DYNAMIC-JUMP))))
                 targets))
       (cond [(member NEXT targets)
              (cond
                [(or (empty? (rest stmts-to-see))
                     (not (label? (second stmts-to-see))))
                 (define fresh-stmt (fresh-label))
                 (loop (append named-targets (cons (label-name fresh-stmt) leaders))
                       (cons fresh-stmt (cons (first stmts-to-see) stmts-seen/rev))
                       (rest stmts-to-see))]
                [else
                 (loop (cons (second stmts-to-see) (append named-targets leaders))
                       (cons (first stmts-to-see) stmts-seen/rev)
                       (rest stmts-to-see))])]
             [else
              (loop leaders
                    (cons (first stmts-to-see) stmts-seen/rev)
                    (rest stmts-to-see))])]
      [else
       (loop leaders 
             (cons (first stmts-to-see) stmts-seen/rev)
             (rest stmts-to-see))])))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     (cond [(symbol? target)
            (list target)]
           [else
            (list DYNAMIC-JUMP)])]
    [(list 'if condition 'goto target)
     (list (cond [(symbol? target)
                  target]
                 [else
                  DYNAMIC-JUMP])
           NEXT)]
    [else
     (raise-type-error 'default-jump-targets "Statement with jump targets" x)]))