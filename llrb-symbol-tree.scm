(include "common.scm")
(module
 llrb-symbol-tree
 (
  make-binding-set
  empty-binding-set
  binding-set-empty?
  binding-set-ref/default
  binding-set-ref
  binding-set-delete
  binding-set-insert
  binding-set-update
  binding-set-cons
  binding-set-fold
  binding-set-union
  ;;
  make-table
  table-copy
  table?
  table-set!
  table-delete!
  table-ref/default
  table-ref
  table-update!
  )
 (import scheme)
 (cond-expand
  (chicken-4
   (import chicken (only data-structures identity)))
  (else
   (import
    (chicken base)
    (chicken type)
    (chicken fixnum)
    (only miscmacros ensure))))
 (import llrb-syntax)
 ;;(include "llrbsyn.scm")

 (cond-expand
  (own-struct
   (define-syntax make-struct (syntax-rules () ((_ args ...) ((##core#primitive "C_make_structureX") args ...)))))
  (else
   (define-syntax make-struct (syntax-rules () ((_ args ...) (##sys#make-structure args ...))))))

 (cond-expand
  (unsafe
   (define-syntax typecheck
     (syntax-rules ()
       ((_ obj typetag loc) (begin))))
   (define-syntax typecheckp
     (syntax-rules ()
       ((_ obj typepred loc) (begin)))))
  (else
   (define-syntax typecheck
     (syntax-rules ()
       ((_ obj typetag loc) (##sys#check-structure obj typetag loc))))
   (define-syntax typecheckp
     (syntax-rules ()
       ((_ obj typepred loc) (typepred obj))))))

 (define-syntax checkbinding-node
   (syntax-rules ()
     ((_ obj loc) (typecheck obj '<symbol-binding-node> loc))))

 (define-inline (make-binding-node color left right key value)
   (make-struct '<symbol-binding-node> color left right key value))
 (define (binding-node? obj)
   (##sys#structure? obj '<symbol-binding-node>))
 (define-syntax binding-node-color
   (syntax-rules ()
     ((_ n) (##sys#slot n 1))))
 (define-syntax binding-node-left
   (syntax-rules ()
     ((_ n) (##sys#slot n 2))))
 (define-syntax binding-node-right
   (syntax-rules ()
     ((_ n) (##sys#slot n 3))))
 (define-syntax %binding-node-name
   (syntax-rules ()
     ((_ n) (##sys#slot n 4))))
 (define (binding-node-name n)
   (checkbinding-node n 'binding-node-name)
   (%binding-node-name n))
 (define-syntax %binding-node-value
   (syntax-rules ()
     ((_ n) (##sys#slot n 5))))
 (define (binding-node-value n)
   (checkbinding-node n 'binding-node-value)
   (%binding-node-value n))

 (define-syntax %symbol->string
   (syntax-rules ()
     ((_ s) (##sys#slot s 1) #;(##sys#symbol->string s))))

 (define-syntax %string<?
   (syntax-rules ()
     ((_ s1 s2)
      (let ((len1 (##core#inline "C_block_size" s1))
	    (len2 (##core#inline "C_block_size" s2)))
	(let ((cmp (##core#inline
		    "C_string_compare" s1 s2
		    (if (fx< len1 len2) len1 len2) )))
	  (or (fx< cmp 0)
	      (and (fx< len1 len2)
		   (eq? cmp 0) ) ))))))

 (define-syntax binding-set-update-node
   (syntax-rules (left: right: color:)
     ((_ 1 n l r c ())
      (make-binding-node c l r (%binding-node-name n) (%binding-node-value n)))
     ((_ 1 n l r c (left: v . more))
      (binding-set-update-node 1 n v r c more))
     ((_ 1 n l r c (right: v . more))
      (binding-set-update-node 1 n l v c more))
     ((_ 1 n l r c (color: v . more))
      (binding-set-update-node 1 n l r v more))
     ((_ n . more)
      (binding-set-update-node
       1 n (binding-node-left n) (binding-node-right n) (binding-node-color n) more))))

 (define-syntax binding-k-n-eq?
   (syntax-rules () ((_ k n) (eq? k (%binding-node-name n)))))

 (define-syntax binding-n-n-eq?
   (syntax-rules () ((_ n1 n2) (eq? (%binding-node-name n1) (%binding-node-name n2)))))

 (define-syntax binding-k-n-lt
   (syntax-rules () ((_ k n) (%string<? k (%binding-node-name n)))))

 (define-syntax binding-n-n-lt
   (syntax-rules () ((_ node1 node2) (%string<? (%binding-node-name node1)
						(%binding-node-name node2)))))

 (define-llrbtree/positional
   (ordered pure)
   binding-set-update-node
   binding-set-init!    ;; defined
   binding-set-lookup   ;; defined
   #f			   ;; no min defined
   %binding-set-fold	   ;; defined
   #f			   ;; no for-each defined
   %binding-set-insert   ;; defined
   binding-node-delete   ;; defined
   #f			   ;; no delete-min defined
   %binding-set-empty?   ;; defined
   binding-k-n-eq?
   binding-n-n-eq?
   binding-k-n-lt
   binding-n-n-lt
   binding-node-left
   binding-node-right
   binding-node-color
   )

 (define (binding-set-empty? x)
   (ensure binding-node? x)
   (%binding-set-empty? x))

 ;; Constructors

 ;; 0X0

 (define (%make-new-binding-node k v)	; internal/unclean
   (make-binding-node #f #f #f k v))

 (define %empty-binding-set		; internal
   (binding-set-init! (make-binding-node #f #f #f #f #f)))

 (define (empty-binding-set) %empty-binding-set)	; export

 ;; 0Xpairs

 (: make-binding-set (&rest -> (struct <symbol-binding-node>)))
 (define (make-binding-set . lst)	; export
   (if (null? lst)
       (empty-binding-set)
       (do ((lst lst (cdr lst))
	    (set %empty-binding-set
		 (let* ((x (car lst)) (k (%symbol->string (car x))))
		   (%binding-set-insert
		    set k #f (%make-new-binding-node k (cdr x)) #f))))
	   ((null? lst) set))))

 (define (%binding-set-ref/thunk envt k thunk success) ; internal
   (let ((entry (binding-set-lookup envt k)))
       (if entry (if success (success (%binding-node-value entry)) (%binding-node-value entry)) (thunk))))

 (define (%binding-set-ref/default envt k default) ; internal
   (let ((entry (binding-set-lookup envt k)))
     (if entry (%binding-node-value entry) default)))

 (: binding-set-ref/default ((struct <symbol-binding-node>) symbol * --> *))
 (define (binding-set-ref/default envt k default) ; export
   (checkbinding-node envt 'binding-set-ref/default)
   (ensure symbol? k)
   (%binding-set-ref/default envt (%symbol->string k) default))

 (: binding-set-ref ((struct <symbol-binding-node>) symbol &optional (procedure () *) (procedure (*) *) --> *))
 (define (binding-set-ref envt k . thunk+success) ; export
   (checkbinding-node envt 'binding-set-ref)
   (ensure symbol? k)
   (%binding-set-ref/thunk
    envt (%symbol->string k)
    (if (pair? thunk+success) (car thunk+success)
	(lambda ()
	  (error "binding-set-ref unbound key" k)))
    (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success))))

 (: binding-set-delete (symbol (struct <symbol-binding-node>) --> (struct <symbol-binding-node>)))
 (define (binding-set-delete k envt)
   (checkbinding-node envt 'binding-set-delete)
   (ensure symbol? k)
   (binding-node-delete envt (%symbol->string k)))

 ;; setXkeyXvalue

 (: binding-set-insert ((struct <symbol-binding-node>) symbol * --> (struct <symbol-binding-node>)))
 (define (binding-set-insert nodeset k v) ; export
   (checkbinding-node nodeset 'binding-set-insert)
   (ensure binding-node? nodeset)
   (ensure symbol? k)
   (let ((k (%symbol->string k)))
     (%binding-set-insert nodeset k #f (%make-new-binding-node k v) #f)))

 (: binding-set-update ((struct <symbol-binding-node>) symbol (procedure (*) *) (procedure () *) --> (struct <symbol-binding-node>)))
 (define (binding-set-update nodeset k update dflt) ; export
   (checkbinding-node nodeset 'binding-set-update)
   (ensure symbol? k)
   (ensure procedure? update)
   (ensure procedure? dflt)
   (let ((k (%symbol->string k)))
     (%binding-set-insert
      nodeset k #f
      (lambda (n)
	(let ((v (update (binding-node-value n))))
	  (make-binding-node #f #f #f (%binding-node-name n) v)))
      (lambda () (%make-new-binding-node k (dflt))))))

 ;; srfi-1::alist-cons compatible
 (: binding-set-cons (symbol * (struct <symbol-binding-node>) --> (struct <symbol-binding-node>)))
 (define (binding-set-cons k v nodeset) ; export
   (binding-set-insert nodeset k v))

 (: binding-set-fold ((procedure (* *) . *) * (struct <symbol-binding-node>) -> *))
 (define (binding-set-fold kvcons nil nodeset)
   (checkbinding-node nodeset 'binding-set-fold)
   (ensure procedure? kvcons)
   (%binding-set-fold (lambda (e i) (kvcons (string->symbol (%binding-node-name e)) (%binding-node-value e) i)) nil nodeset))

 ;; setXset

 (: binding-set-union
    ((struct <symbol-binding-node>) (struct <symbol-binding-node>) --> (struct <symbol-binding-node>)))
 (define (binding-set-union inner outer) ; export
   (checkbinding-node inner 'binding-union)
   (checkbinding-node outer 'binding-union)
   (%binding-set-fold (lambda (node init) (%binding-set-insert init (%binding-node-name node) #f node #f)) outer inner))

 (define-type |:table:| (struct <llrb-symbol-table>))
 (define-record-type <llrb-symbol-table>
   (%make-symbol-table root)
   table?
   (root root root-set!))

 (define-syntax check-table
   (syntax-rules ()
     ((_ obj loc) (typecheckp obj table? loc))))

 (: make-table ( --> |:table:|))
 (define (make-table)
   (%make-symbol-table (empty-binding-set)))

 (: table-copy (|:table:| --> |:table:|))
 (define (table-copy table)
   (check-table table 'symbol-table-copy)
   (%make-symbol-table (root table)))

 (: table-delete! (|:table:| symbol -> *))
 (define (table-delete! table key)
   (check-table table 'symbol-table-delete!)
   (ensure symbol? key)
   (root-set! table (binding-node-delete (root table) (%symbol->string key))))

 (: table-set! (|:table:| symbol * -> *))
 (define (table-set! table key value)
   (check-table table 'symbol-table-set!)
   (ensure symbol? key)
   (let ((key (%symbol->string key)))
     (root-set! table (%binding-set-insert (root table) key #f (%make-new-binding-node key value) #f))))

 (: table-ref/default (|:table:| symbol * --> *))
 (define (table-ref/default table key default)
   (check-table table 'symbol-table-ref/default)
   (ensure symbol? key)
   (%binding-set-ref/default (root table) (%symbol->string key) default))

 (: table-ref (|:table:| symbol &optional (procedure () *) (procedure (*) *) -> *))
 (define (table-ref table key . thunk+success)
   (check-table table 'symbol-table-ref)
   (ensure symbol? key)
   (%binding-set-ref/thunk
    (root table) (%symbol->string key)
    (if (pair? thunk+success) (car thunk+success)
	(lambda ()
	  (error "symbol-table-ref unbound key" key)))
    (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success))))

 (: table-update! (|:table:| symbol procedure &rest procedure -> *))
 (define (table-update! table key update . default)
   (check-table table 'symbol-table-update!)
   (ensure symbol? key)
   (ensure procedure? update)
   (let loop ((key (%symbol->string key))
	      (old (root table))
	      (result #f))
     (let ((new
	    (%binding-set-insert
	     old
	     key
	     #f
	     (let ((update (or update identity)))
	       (lambda (n)
		 (let ((v (update (binding-node-value n))))
		   (set! result v)
		   (make-binding-node #f #f #f (%binding-node-name n) v))))
	     (if (and (pair? default) (procedure? (car default)))
		 (let ((thunk (car default))) (lambda () (%make-new-binding-node key (thunk))))
		 (lambda () (error "symbol-table-update! no default" default))))))
       (if (eq? old (root table))
	   (begin
	     (root-set! table new)
	     result)
	   (loop key (root table) #f)))))
 )
