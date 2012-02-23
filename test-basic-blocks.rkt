#lang racket
(require "basic-blocks.rkt"
         rackunit)


(define (frac stmts)
  (define bblocks (fracture stmts))
  (map (lambda (b) (cons (bblock-name b) (bblock-stmts b)))
       bblocks))


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

(check-equal? (frac '(entry
                      (goto entry)
                      (dead-command)))
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
                                 '((blah)
                                   (baz)
                                   (if something goto entry))
                                 (set 'entry 'consequent)
                                 'consequent)
                    (make-bblock 'consequent
                                 #f
                                 '((now do something else))
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
                                 '((if (= n 0) goto end))
                                 (set 'end 'consequent)
                                 'consequent)
                    (make-bblock 'consequent
                                 #f
                                 '((<- acc (* acc n))
                                   (<- n (sub1 n))
                                   (goto entry))
                                 (set 'entry)
                                 #f)
                    (make-bblock 'end
                                 #f
                                 '((goto (reg return)))
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
                                 '((<- val (* n n))
                                   (goto (reg return)))
                                 (set DYNAMIC)
                                 #f)
                    (make-bblock 'entry-2
                                 #t
                                 '((<- val (sqrt n))
                                   (goto (reg return)))
                                 (set DYNAMIC)
                                 #f)))

;; Big example from:
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-35.html#%_sec_5.5.5
;;
(define factorial-snippet
  '(START
    ;; construct the procedure and skip over code for the procedure body
    (assign val
            (op make-compiled-procedure) (label entry2) (reg env))
    (goto (label after-lambda1))
    
    entry2     ; calls to factorial will enter here
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env
            (op extend-environment) (const (n)) (reg argl) (reg env))
    ;; begin actual procedure body
    (save continue)
    (save env)

    ;; compute (= n 1)
    (assign proc (op lookup-variable-value) (const =) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch17))
    compiled-branch16
    (assign continue (label after-call15))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch17
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

    after-call15   ; val now contains result of (= n 1)
    (restore env)
    (restore continue)
    (test (op false?) (reg val))
    (branch (label false-branch4))
    true-branch5  ; return 1
    (assign val (const 1))
    (goto (reg continue))

    false-branch4
    ;; compute and return (* (factorial (- n 1)) n)
    (assign proc (op lookup-variable-value) (const *) (reg env))
    (save continue)
    (save proc)   ; save * procedure
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op list) (reg val))
    (save argl)   ; save partial argument list for *

    ;; compute (factorial (- n 1)), which is the other argument for *
    (assign proc
            (op lookup-variable-value) (const factorial) (reg env))
    (save proc)  ; save factorial procedure
    ;; compute (- n 1), which is the argument for factorial
    (assign proc (op lookup-variable-value) (const -) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch8))
    compiled-branch7
    (assign continue (label after-call6))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch8
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

    after-call6   ; val now contains result of (- n 1)
    (assign argl (op list) (reg val))
    (restore proc) ; restore factorial
    ;; apply factorial
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch11))
    compiled-branch10
    (assign continue (label after-call9))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch11
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

    after-call9      ; val now contains result of (factorial (- n 1))
    (restore argl) ; restore partial argument list for *
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc) ; restore *
    (restore continue)
    ;; apply * and return its value
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch14))
    compiled-branch13
    ;; note that a compound procedure here is called tail-recursively
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch14
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    after-call12
    after-if3
    after-lambda1
    ;; assign the procedure to the variable factorial
    (perform
     (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))))


(fracture
  factorial-snippet
  #:entry-names '(START entry2 after-call15)

  #:fresh-label (let ([counter 0])
                  (lambda ()
                    (set! counter (add1 counter))
                    (string->symbol (format "l~a" counter))))
  #:label? symbol?
  #:label-name identity
  #:jump? (lambda (stmt)
            (match stmt
              [(list 'goto place) #t]
              [(list 'branch place) #t]
              [else #f]))
  #:jump-targets (lambda (a-jump)
                   (match a-jump
                     [(list 'goto place)
                      (match place
                        [(list 'label name)
                         (list name)]
                        [else
                         (list DYNAMIC)])]
                     [(list 'branch place)
                      (match place
                        [(list 'label name)
                         (list name NEXT)]
                        [else
                         (list DYNAMIC NEXT)])])))



#;(check-equal?
 (fracture
  factorial-snippet
  #:entry-names '(START entry2 after-call15)

  #:fresh-label (let ([counter 0])
                  (lambda ()
                    (set! counter (add1 counter))
                    (string->symbol (format "l~a" counter))))
  #:label? symbol?
  #:label-name identity
  #:jump? (lambda (stmt)
            (match stmt
              [(list 'goto place) #t]
              [(list 'branch place) #t]
              [else #f]))
  #:jump-targets (lambda (a-jump)
                   (match a-jump
                     [(list 'goto place)
                      (match place
                        [(list 'label name)
                         (list name)]
                        [else
                         (list DYNAMIC)])]
                     [(list 'branch place)
                      (match place
                        [(list 'label name)
                         (list name NEXT)]
                        [else
                         (list DYNAMIC NEXT)])])))


 (list (make-bblock 'START
                    #t
                    '((assign val
                              (op make-compiled-procedure)
                              (label entry2)
                              (reg env))
                      (goto (label after-lambda1)))
                    (set 'after-lambda1)
                    #f)
       (make-bblock 'entry2
                    #t
                    '((assign env (op compiled-procedure-env) (reg proc))
                      (assign env
                              (op extend-environment)
                              (const (n))
                              (reg argl)
                              (reg env))
                      ;; begin actual procedure body
                      (save continue)
                      (save env)
                      ;; compute (= n 1)
                      (assign proc
                              (op lookup-variable-value)
                              (const =)
                              (reg env))
                      (assign val (const 1))
                      (assign argl (op list) (reg val))
                      (assign val (op lookup-variable-value)
                              (const n)
                              (reg env))
                      (assign argl (op cons) (reg val) (reg argl))
                      (test (op primitive-procedure?) (reg proc))
                      (branch (label primitive-branch17)))
                    (set 'primitive-branch17 'compiled-branch16)
                    'compiled-branch16)
       (make-bblock 'compiled-branch16
                    #f
                    '((assign continue (label after-call15))
                      (assign val (op compiled-procedure-entry) (reg proc))
                      (goto (reg val)))
                    (set DYNAMIC)
                    #f)
       (make-bblock 'primitive-branch17
                    #f
                    '((assign val
                              (op apply-primitive-procedure)
                              (reg proc)
                              (reg argl)))
                    (set 'after-call15)
                    'after-call15)
       (make-bblock 'after-call15
                    #t
                    '(; val now contains result of (= n 1)
                      (restore env)
                      (restore continue)
                      (test (op false?) (reg val))
                      (branch (label false-branch4)))
                    (set 'false-branch4 'true-branch5)
                    'true-branch5)
       (make-bblock 'true-branch5
                    #f
                    '(; return 1
                      (assign val (const 1))
                      (goto (reg continue)))
                    (set DYNAMIC)
                    #f)
       (make-bblock 'false-branch4
                    #f
                    '(;; compute and return (* (factorial (- n 1)) n)
                      (assign proc (op lookup-variable-value) (const *) (reg env))
                      (save continue)
                      (save proc)   ; save * procedure
                      (assign val (op lookup-variable-value) (const n) (reg env))
                      (assign argl (op list) (reg val))
                      (save argl)   ; save partial argument list for *

                      ;; compute (factorial (- n 1)), which is the other argument for *
                      (assign proc
                              (op lookup-variable-value) (const factorial) (reg env))
                      (save proc))
                    (set)
                    #f)))

