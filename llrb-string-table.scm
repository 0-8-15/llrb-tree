(include "common.scm")
(module
 llrb-string-table
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
  table?
  table-empty?
  table-copy
  table-delete!
  table-set!
  table-ref/default
  table-ref
  table-update!
  table-fold
  table-for-each
  table-min
  table-delete-min!
  ;;
  wrap-one-string-arg
  str2sym ;; caches string->symbol
  )
 (import scheme)
 (cond-expand
  (chicken-4
   (import chicken (only data-structures identity)))
  (else
   (import
    (chicken base)
    (chicken type)
    (chicken foreign)
    (chicken fixnum)
    (only miscmacros ensure))))
 ;; (import (only data-structures identity))
 ;; (import (only lolevel mutate-procedure!))
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
     ((_ obj loc) (typecheck obj '<string-binding-node> loc))))
 
 (define-inline (make-binding-node color left right key value)
   (make-struct '<string-binding-node> color left right key value))
 (define (binding-node? obj)
   (##sys#structure? obj '<string-binding-node>))
 (define-syntax binding-node-color
   (syntax-rules ()
     ((_ n) (##sys#slot n 1))))
 (define-syntax binding-node-left
   (syntax-rules ()
     ((_ n) (##sys#slot n 2))))
 (define-syntax binding-node-right
   (syntax-rules ()
     ((_ n) (##sys#slot n 3))))
 (define-syntax binding-node-key
   (syntax-rules ()
     ((_ n) (##sys#slot n 4))))
 (define-syntax binding-node-value
   (syntax-rules ()
     ((_ n) (##sys#slot n 5))))

 (define-syntax binding-set-update-node
   (syntax-rules (left: right: color:)
     ((_ 1 n l r c ())
      (make-binding-node c l r (binding-node-key n) (binding-node-value n)))
     ((_ 1 n l r c (left: v . more))
      (binding-set-update-node 1 n v r c more))
     ((_ 1 n l r c (right: v . more))
      (binding-set-update-node 1 n l v c more))
     ((_ 1 n l r c (color: v . more))
      (binding-set-update-node 1 n l r v more))
     ((_ n . more)
      (binding-set-update-node
       1 n (binding-node-left n) (binding-node-right n) (binding-node-color n) more))))
 
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

 (define-syntax %string=?
   (syntax-rules ()
     ((_ s1 s2)
      (or (eq? s1 s2)
	  (let ((len1 (##core#inline "C_block_size" s1))
		(len2 (##core#inline "C_block_size" s2)))
	    (and (eq? len1 len2)
		 (eq?
		  (##core#inline
		   "C_string_compare" s1 s2
		   (if (fx< len1 len2) len1 len2) )
		  0)))))))

 (define-syntax binding-node-key-node-eq?
   (syntax-rules ()
     ((_ key node) (%string=? key (binding-node-key node)))))

 (define-syntax binding-node-key-node-lt
   (syntax-rules () ((_ key node) (%string<? key (binding-node-key node)))))

 (define-syntax binding-node-node-node-=?
   (syntax-rules ()
     ((_ node1 node2) (%string=? (binding-node-key node1) (binding-node-key node2)))))

 (define-syntax binding-node-node-node-lt
   (syntax-rules ()
     ((_ node1 node2) (%string<? (binding-node-key node1) (binding-node-key node2)))))

 (define-llrbtree/positional
   (ordered pure)
   binding-set-update-node
   binding-node-init!		 ;; defined
   binding-node-lookup		 ;; defined
   binding-node-min		 ;; defined
   binding-node-fold		 ;; defined
   binding-node-for-each	 ;; defined
   binding-node-insert		 ;; defined
   binding-node-delete		 ;; defined
   binding-node-delete-min	 ;; defined
   binding-node-empty?		 ;; defined
   binding-node-key-node-eq?
   binding-node-node-node-=?
   binding-node-key-node-lt
   binding-node-node-node-lt     ;; before? node order
   binding-node-left
   binding-node-right
   binding-node-color
   )

 (define (binding-set-empty? x)
   (ensure binding-node? x)
   (binding-node-empty? x))

 ;; Constructors

 ;; 0X0

 (define (%make-new-binding-node k v)	; internal/unclean
   (make-binding-node #f #f #f k v))

 (define empty-binding-set
   (let ((empty-node (binding-node-init! (make-binding-node #f #f #f #f #f))))
     (lambda () empty-node)))		; export

 ;; 0Xpairs

 (: make-binding-set (&rest -> (struct <string-binding-node>)))
 (define (make-binding-set . lst)	; export
   (if (null? lst)
       (empty-binding-set)
       (do ((lst lst (cdr lst))
	    (set (empty-binding-set)
		 (let* ((x (car lst)) (k (car x)))
		   (binding-node-insert set k #f (%make-new-binding-node k (cdr x)) #f))))
	   ((null? lst) set))))

 (define (%binding-set-ref/thunk envt k thunk success) ; internal
   (let ((entry (binding-node-lookup envt k)))
     (if entry (if success (success (binding-node-value entry)) (binding-node-value entry)) (thunk))))

 (define (%binding-set-ref/default envt k default) ; internal
   (let ((entry (binding-node-lookup envt k)))
     (if entry (binding-node-value entry) default)))

 (: binding-set-ref/default ((struct <string-binding-node>) string * --> *))
 (define (binding-set-ref/default envt k default) ; export
   (checkbinding-node envt 'binding-set-ref/default)
   (%binding-set-ref/default envt k default))

 (: binding-set-ref ((struct <string-binding-node>) string &optional (procedure () . *) (procedure (*) . *) --> *))
 (define (binding-set-ref envt k . thunk+success) ; export
   (checkbinding-node envt 'binding-set-ref)
   (%binding-set-ref/thunk
    envt k
    (if (pair? thunk+success) (car thunk+success)
	(lambda ()
	  (error "binding-set-ref unbound key" k)))
    (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success))))

 (: binding-set-delete (string (struct <string-binding-node>) --> (struct <string-binding-node>)))
 (define (binding-set-delete k envt)
   (checkbinding-node envt 'binding-set-delete)
   (ensure string? k)
   (binding-node-delete envt k))

 ;; setXkeyXvalue

 (: binding-set-insert ((struct <string-binding-node>) string * --> (struct <string-binding-node>)))
 (define (binding-set-insert nodeset k v) ; export
   (checkbinding-node nodeset 'binding-set-insert)
   (binding-node-insert nodeset k #f (%make-new-binding-node k v) #f))

 (: binding-set-update ((struct <string-binding-node>)
			string (procedure (*) *) (procedure () *)
			--> (struct <string-binding-node>)))
 (define (binding-set-update nodeset k update dflt) ; export
   (checkbinding-node nodeset 'binding-set-update)
   (ensure procedure? update)
   (ensure procedure? dflt)
   (binding-node-insert
    nodeset k #f
    (let ((update (or update identity)))
      (lambda (n)
	(let ((v (update (binding-node-value n))))
	  (make-binding-node #f #f #f (binding-node-key n) v))))
    (lambda () (%make-new-binding-node k (dflt)))))

 ;; srfi-1::alist-cons compatible
 (: binding-set-cons (string * (struct <string-binding-node>) --> (struct <string-binding-node>)))
 (define (binding-set-cons k v nodeset) ; export
   (binding-set-insert nodeset k v))

 (: binding-set-fold ((procedure (string * *) . *) * (struct <string-binding-node>)-> *))
 (define (binding-set-fold kvcons nil nodeset)
   (checkbinding-node nodeset 'binding-set-fold)
   (ensure procedure? kvcons)
   (binding-node-fold (lambda (e i) (kvcons (binding-node-key e) (binding-node-value e) i)) nil nodeset))

 ;; setXset

 (: binding-set-union ((struct <string-binding-node>) (struct <string-binding-node>) --> (struct <string-binding-node>)))
 (define (binding-set-union inner outer) ; export
   (checkbinding-node inner 'binding-set-union)
   (checkbinding-node outer 'binding-set-union)
   (binding-node-fold
    (lambda (node init) (binding-node-insert init (binding-node-key node) #f node #f))
    outer inner))

 (define-type :table: (struct <llrb-string-table>))
 (define-record-type <llrb-string-table>
   (%make-string-table root)
   table?
   (root root root-set!))

 (define-syntax check-table
   (syntax-rules ()
     ((_ obj loc) (typecheckp obj table? loc))))

 (define (make-table)
   (%make-string-table (empty-binding-set)))

 (: table-copy (:table: --> :table:))
 (define (table-copy table)
   (check-table table 'string-table-copy)
   (%make-string-table (root table)))

 (: table-empty? (:table: --> boolean))
 (define (table-empty? table)
   (check-table table 'string-table-empty?)
   (binding-node-empty? (root table)))

 (: table-delete! (:table: string -> *))
 (define (table-delete! table key)
   (check-table table 'string-table-delete!)
   (root-set! table (binding-node-delete (root table) key)))

 (: table-set! (:table: string * -> *))
 (define (table-set! table key value)
   (check-table table 'string-table-set!)
   (root-set! table (binding-node-insert (root table) key #f (%make-new-binding-node key value) #f)))
 
 (: table-ref/default (:table: string * --> *))
 (define (table-ref/default table key default)
   (check-table table 'string-table-ref/default)
   (%binding-set-ref/default (root table) key default))

 (: table-ref (:table: string &optional (procedure () *) (procedure (*) *) -> *))
 (define (table-ref table key . thunk+success)
   (check-table table 'string-table-ref)
   (%binding-set-ref/thunk
    (root table) key
    (if (pair? thunk+success) (car thunk+success)
	(lambda ()
	  (error "string-table-ref unbound key" key)))
    (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success))))

 (: table-update! (:table: string (or false procedure) &rest procedure -> *))
 (define (table-update! table key update . default)
   (check-table table 'string-table-update!)
   (or (eq? update #f) (ensure procedure? update))
   (let loop ((result #f) (old (root table)))
     (let ((new
	    (binding-node-insert
	     old
	     key
	     #f
	     (if update
		 (lambda (n)
		   (let ((v (update (binding-node-value n))))
		     (set! result v)
		     (make-binding-node #f #f #f (binding-node-key n) v)))
		 (lambda (n) (set! result (binding-node-value n)) n))
	     (lambda ()
	       (if (and (pair? default) (procedure? (car default)))
		   (let ((thunk (car default))) (%make-new-binding-node key (thunk)))
		   (error "string-table-update! no default" default))))))
       (if (eq? old (root table))
	   (begin
	     (root-set! table new)
	     result)
	   (loop #f (root table))))))

 (: table-fold (:table: (procedure (string * :table:) *) * -> *))
 (define (table-fold table proc init)
   (check-table table 'string-table-fold)
   (ensure procedure? proc)
   (binding-node-fold
    (lambda (node init) (proc (binding-node-key node) (binding-node-value node) init))
    init (root table)))

 (: table-for-each (:table: (procedure (string *) *) -> *))
 (define (table-for-each table proc)
   (check-table table 'string-table-for-each)
   (ensure procedure? proc)
   (binding-node-for-each
    (lambda (node) (proc (binding-node-key node) (binding-node-value node)))
    (root table))
   #f)

 (: table-min (:table: (procedure () * *) --> * *))
 (define (table-min table default)
   (check-table table 'string-table-min)
   (let ((node (binding-node-min (root table))))
     (if node (values (binding-node-key node) (binding-node-value node))
	 (begin
	   (ensure procedure? default)
	   (default)))))

 (: table-delete-min! (:table: -> * *))
 (define (table-delete-min! table)
   (binding-node-delete-min
    (root table)
    (lambda (root node)
      (root-set! table root)
      (if node
	  (values (binding-node-key node) (binding-node-value node))
	  (values #f #f)))))

 (define (wrap-one-string-arg proc)
   (let ((set (empty-binding-set))
	 (not-found '(not-found)))
     (lambda (s)
       (##sys#check-string s 'string-cached)
       (let ((hit (%binding-set-ref/default set s not-found)))
	 (if (eq? hit not-found)
	     (let ((value (proc s)))
	       (set! set (binding-node-insert set s #f (%make-new-binding-node s value) #f))
	       value)
	     hit)))))

 (define str2sym
   (let ((set (empty-binding-set))
	 (not-found '(not-found)))
     (lambda (s)
       (##sys#check-string s 'string-cached)
       (let ((entry (binding-node-lookup set s)))
	 (if entry (binding-node-value entry)
	     (let ((value (string->symbol s)))
	       (set! set (binding-node-insert set s #f (%make-new-binding-node s value) #f))
	       value))))))

 )
