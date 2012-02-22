#lang racket/base

(require racket/match 
         racket/set
         racket/list)


(provide NEXT
         DYNAMIC
         (struct-out bblock)
         fracture)


;; Basic blocks
(define-struct bblock (name   ;; symbol
                       entry? ;; boolean
                       stmts  ;; (listof stmt)
                       succs  ;; (setof (U symbol DYNAMIC-JUMP))
                       next-succ ;; (U #f symbol)
                       )
  #:transparent)

  

(define-struct next ())
(define-struct dynamic ())
(define NEXT (make-next))
(define DYNAMIC (make-dynamic))


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
  (let-values ([(entry-names-set) (list->set (cons (label-name (first stmts)) entry-names))]
               [(leaders stmts)
                (find/inject-leaders stmts entry-names jump? jump-targets
                                     label? label-name fresh-label)])

    ;; leader?: stmt -> boolean
    ;; Returns true if the statement is a leader.
    (define (leader? stmt)
      (and (label? stmt) (set-member? leaders (label-name stmt))))
    
    (let loop ([bblocks '()]
               [pending-block-name (label-name (first stmts))]
               [pending-stmts/rev (list (first stmts))]
               [pending-jump-targets (set)]
               [pending-next-succ #f]
               [stmts (rest stmts)])
      (cond
        [(empty? stmts)
         (reverse (cons (make-bblock pending-block-name
                                     (set-member? entry-names-set pending-block-name)
                                     (reverse pending-stmts/rev)
                                     pending-jump-targets
                                     pending-next-succ)
                        bblocks))]
        [(leader? (first stmts))
         (loop (cons (make-bblock pending-block-name
                                  (set-member? entry-names-set pending-block-name)
                                  (reverse pending-stmts/rev)
                                  pending-jump-targets
                                  pending-next-succ)
                     bblocks)
               (label-name (first stmts))
               (list (first stmts))
               (set)
               #f
               (rest stmts))]
        
        [else
         (loop bblocks 
               pending-block-name 
               ;; Omit dead labels.
               (if (label? (first stmts)) pending-stmts/rev 
                   (cons (first stmts) pending-stmts/rev))
               (if (jump? (first stmts))
                   (set-union (list->set (map (lambda (t)
                                                (cond [(eq? t NEXT)
                                                       (label-name (second stmts))]
                                                      [(eq? t DYNAMIC)
                                                       DYNAMIC]
                                                      [else t]))
                                              (jump-targets (first stmts))))
                              pending-jump-targets)
                   pending-jump-targets)
               (if (and (jump? (first stmts))
                        (memq NEXT (jump-targets (first stmts))))
                   (label-name (second stmts))
                   #f)
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
                        (not (eq? t DYNAMIC))))
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
            (list DYNAMIC)])]
    [(list 'if condition 'goto target)
     (list (cond [(symbol? target)
                  target]
                 [else
                  DYNAMIC])
           NEXT)]
    [else
     (raise-type-error 'default-jump-targets "Statement with jump targets" x)]))