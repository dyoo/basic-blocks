#lang scribble/manual
@(require scribble/eval
          planet/scribble
          (for-label (this-package-in main)
                     racket/base
                     (except-in racket/contract -> or/c)
                     racket/set))
@(define my-evaluator (make-base-eval))
@(my-evaluator '(require racket/set racket/list))

@title{basic-blocks: compute basic blocks from a list of statements and labels}

@author+email["Danny Yoo" "dyoo@hashcollision.org"]

@section{Introduction}

This package takes a list of statements and labels, and breaks them
down into @link["http://en.wikipedia.org/wiki/Basic_block"]{basic
blocks}.  As a quick example:

@interaction[#:eval my-evaluator
(require (planet dyoo/basic-blocks))
(define a-block (fracture '(entry
                            (blah)
                            (baz)
                            (goto entry))))
(length a-block)
(bblock-name (first a-block))
(bblock-stmts (first a-block))
(bblock-succs (first a-block))
]

See @secref["larger example"] for a more substantial use of
this library.



@section{API}
@defmodule/this-package[main]

The main structure that this library produces is the @racket[bblock]
basic-block structure.

@defstruct[bblock ([name symbol]
                   [entry? boolean]
                   [stmts (listof statement)]
                   [succs (set/c (or/c symbol DYNAMIC))]
                   [next-succ (or/c symbol #f)])]{
The basic block structure.  The first block generated by fracture is
always labeled as an entry, as are any other blocks whose names are
explicitly passed to @racket[fracture] using @racket[#:entry-names].

@racket[bblock] also stores what blocks are successors of a given
block; the set of these jump targets can be accessed with
@racket[bblock-succs].  If the block has a dynamic (computed) jump,
then @racket[DYNAMIC] is a member of @racket[bblock-succs].

For convenience, if the block ends with a conditional jump, then
@racket[bblock-next-succ] refers to the block that follows immediately
next.
}



@defproc[(fracture [stmts (listof (or/c statement label))]
                   [#:entry-names entry-names (listof symbol) '()]
                   [#:label? label? (any/c -> boolean) default-label?]
                   [#:label-name label-name (label -> symbol) default-label-name]
                   [#:jump? jump? (any/c -> boolean) default-jump?]
                   [#:jump-targets jump-target (jump -> (listof (or/c symbol? NEXT DYNAMIC))) default-jump-targets]
                   [#:fresh-block-name fresh-block-name (-> symbol) default-fresh-block-name])
         (listof bblock)]{

Computes the list of basic blocks from a sequence of statements and
labels.  Labels are assumed to be disjoint from statements.
@racket[fracture] takes in several optional keyword arguments to
customize what it means to be a statement or a jump.  It produces a
list of @racket[bblock] structures.


The very first statement of @racket[stmts] must be a label; it's assumed
to be the central entry point.  It's important to provide a
@racket[#:entry-names] that names all entry points we care about.
Although @racket[fracture] does not do much optimization, it does omit
blocks that can not be reached by any entry-point basic block.

For example:
@interaction[#:eval my-evaluator
(fracture '(entry
            (hello world)
            (goto done)
            another-entry-point
            (this is another block)
            done
            (bye)))]

Note that @racket[another-entry-point] does not occur in the outputted set
of basic blocks, because @racket[fracture] could not find a path from
any entry to it.

By providing a @racket[#:entry-names], we can convince @racket[fracture]
to maintain the block starting with @racket[another-entry-point]:
@interaction[#:eval my-evaluator
(fracture #:entry-names '(another-entry-point)
          '(entry
            (hello world)
            (goto done)
            another-entry-point
            (this is another block)
            done
            (bye)))
]
@racket[#:entry-names] implicitly includes the name of the first basic
block.


All of the other default values assume a particular structure for
labels and jump statements.  The default can be overriden by providing
for the keyword arguments.  The example in @secref["larger example"]
shows how to use @racket[fracture] on a somewhat different statement
structure than the default.



@subsection{Labels}
By default, a label is defined to be a symbol, and getting its name is
just the identity.

By default, @racket[#:label?] is:
@racketblock[
(define (default-label? x)
  (symbol? x))

(define (default-label-name a-label)
  (cond
    [(symbol? a-label) a-label]
    [else
     (raise-type-error 'default-label-name "symbol" a-label)]))
]



@subsection{Jumps}
A jump is either conditional or unconditional, and may jump to a
static or dynamic target.  @racket[#:jump?] consumes a statement, and
produces true if the statement is a conditional or unconditional jump.

By default, @racket[#:jump?] is:
@racketblock[
(define (default-jump? x)
  (match x
    [(list 'goto target)
     #t]
    [(list 'if condition 'goto target)
     #t]
    [else #f]))
]


Given a jump statement, @racket[#:jump-targets] produces a list of the
targets that the jump can go to.  We can specify that it's a
conditional jump by including the constant @racket[NEXT] as one of its
targets.  We can also use the constant @racket[DYNAMIC] to indicate that the
jump has a runtime-dependent target.

@defthing[NEXT jump-target]{Indicates a jump to the following
statement.  Use this in combination with other jump targets to
indicate a conditional jump.}

@defthing[DYNAMIC jump-target]{Indicates a jump to a runtime-dependent target.}


By default, @racket[#:jump-targets] is:
@racketblock[
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
]


Under some situations, @racket[fracture] may need to synthesize a
fresh name for a basic block.  It uses @racket[#:fresh-block-name] to
generate the name.  By default, @racket[#:fresh-block-name] is:
@racketblock[
(define (default-fresh-block-name)
  (gensym 'label))
]

}






@section[#:tag "larger example"]{A larger example}

As a more substantial example, we can use @racket[fracture] to
construct the basic blocks from one of the
@link["http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-35.html#%_sec_5.5.5"]{examples}
of @link["http://mitpress.mit.edu/sicp/"]{Structure and Interpretation
of Computer Programs}.  We pass in additional keyword arguments to
teach @racket[fracture] what statements are labels and jumps.

@interaction[#:eval my-evaluator

(define factorial-snippet
  '(START
    (assign val (op make-compiled-procedure) (label entry2) (reg env))
    (goto (label after-lambda1))
    entry2
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
    (save continue)
    (save env)
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
    after-call15
    (restore env)
    (restore continue)
    (test (op false?) (reg val))
    (branch (label false-branch4))
    true-branch5
    (assign val (const 1))
    (goto (reg continue))
    false-branch4
    (assign proc (op lookup-variable-value) (const *) (reg env))
    (save continue)
    (save proc)
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op list) (reg val))
    (save argl)
    (assign proc (op lookup-variable-value) (const factorial) (reg env))
    (save proc)
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
    after-call6
    (assign argl (op list) (reg val))
    (restore proc)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch11))
    compiled-branch10
    (assign continue (label after-call9))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch11
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call9
    (restore argl)
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)
    (restore continue)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch14))
    compiled-branch13
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch14
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    after-call12
    after-if3
    after-lambda1
    (perform (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))))

(require racket/match)

(define blocks 
  (fracture factorial-snippet
            #:entry-names '(START entry2 after-call15 after-call6 after-call9)

            #:fresh-block-name (let ([counter 0])
                                 (lambda ()
                                   (set! counter (add1 counter))
                                   (string->symbol (format "l~a" counter))))
            #:label? symbol?
            #:label-name (lambda (x) x)
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
                                   (list DYNAMIC NEXT)])]))))

(for ([b blocks])
  (printf "~a -> ~a\n"
          (string-append (symbol->string (bblock-name b))
                         (if (bblock-entry? b) "*" ""))
          (set->list (bblock-succs b))))]
