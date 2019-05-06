(include "common.scm")
(module
 llrb-fixnum-table
 (
  make-table
  table?
  table-empty?
  table-copy
  table-ref
  table-ref/default
  table-set!
  table-delete!
  table-for-each
  table-fold
  table-update!
  table-min table-delete-min!
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
     ((_ obj loc) (typecheck obj '<fixnum-binding-node> loc))))

 (define-inline (make-binding-node color left right key value)
   (make-struct '<fixnum-binding-node> color left right key value))
 (define (binding-node? obj)
   (##sys#structure? obj '<fixnum-binding-node>))
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

 (define-type |:table:| (struct <llrb-fixnum-table>))
 (define-record-type <llrb-fixnum-table>
   (%make-fixnum-table root)
   table?
   (root root root-set!))

 (define-syntax check-table
   (syntax-rules ()
     ((_ obj loc) (typecheckp obj table? loc))))

 (define-syntax binding-node-update
   (syntax-rules (left: right: color:)
     ((_ 1 n l r c ())
      (make-binding-node c l r (binding-node-key n) (binding-node-value n)))
     ((_ 1 n l r c (left: v . more))
      (binding-node-update 1 n v r c more))
     ((_ 1 n l r c (right: v . more))
      (binding-node-update 1 n l v c more))
     ((_ 1 n l r c (color: v . more))
      (binding-node-update 1 n l r v more))
     ((_ n . more)
      (binding-node-update
       1 n (binding-node-left n) (binding-node-right n) (binding-node-color n) more))))

 (define-syntax binding-node-key-node-eq?
   (syntax-rules ()
     ((_ key node) (eq? key (binding-node-key node)))))

 (define-syntax binding-node-key-node-lt
   (syntax-rules () ((_ key node) (fx< key (binding-node-key node)))))

 (define-syntax binding-node-node-node-=?
   (syntax-rules ()
     ((_ node1 node2) (eq? (binding-node-key node1) (binding-node-key node2)))))

 (define-syntax binding-node-node-node-lt
   (syntax-rules ()
     ((_ node1 node2) (fx< (binding-node-key node1) (binding-node-key node2)))))

 (define-llrbtree/positional
   (ordered pure)
   binding-node-update
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

 (: make-table (--> |:table:|))

 (define make-table
   (let ((n0 (binding-node-init! (make-binding-node #f #f #f  #f #f))))
     (lambda () (%make-fixnum-table n0))))

 (: table-copy (|:table:| --> |:table:|))
 (define (table-copy table)
   (check-table table 'fixnum-table-copy)
   (%make-fixnum-table (root table)))

 (: table-empty? (|:table:| --> boolean))
 (define (table-empty? table)
   (ensure table? table)
   (binding-node-empty? (root table)))

 (: table-delete! (|:table:| fixnum -> *))
 (define (table-delete! table key)
   (check-table table 'fixnum-table-delete!)
   (ensure fixnum? key)
   (root-set! table (binding-node-delete (root table) key)))

 (: table-set! (|:table:| fixnum * -> boolean))
 (define (table-set! table key value)
   (check-table table 'fixnum-table-set!)
   (ensure fixnum? key)
   (root-set! table (binding-node-insert (root table) key #f (make-binding-node #f #f #f key value) #f)))

 (: table-ref/default (|:table:| fixnum * --> *))
 (define (table-ref/default table key default)
   (check-table table 'fixnum-table-ref/default)
   (ensure fixnum? key)
   (let ((node (binding-node-lookup (root table) key)))
     (if node (binding-node-value node) default)))

 (: table-ref (|:table:| fixnum &optional (procedure () *) (procedure (*) *) -> *))
 (define (table-ref table key . thunk+success)
   (check-table table 'fixnum-table-ref)
   (ensure fixnum? key)
   (let ((node (binding-node-lookup (root table) key)))
     (if node (if (and (pair? thunk+success) (pair? (cdr thunk+success)))
		  ((cadr thunk+success) (binding-node-value node))
		  (binding-node-value node))
	 (if (pair? thunk+success) ((car thunk+success))
	     (error "fixnum-table-ref no key" key)))))

 (: table-update! (|:table:| fixnum procedure &rest procedure -> *))
 (define (table-update! table key update . default)
   (check-table table 'fixnum-table-update!)
   (ensure fixnum? key)
   (ensure procedure? update)
   (let loop ((result #f)
	      (old (root table)))
     (let ((new
	    (binding-node-insert
	     (root table) key #f
	     (let ((update (or update identity)))
	       (lambda (n)
		 (let ((v (update (binding-node-value n))))
		   (set! result v)
		   (make-binding-node #f #f #f (binding-node-key n) v))))
	     (if (and (pair? default) (procedure? (car default)))
		 (let ((thunk (car default)))
		   (lambda () (make-binding-node #f #f #f key (thunk))))
		 (lambda () (error "fixnum-table-update! no default" default))))))
       (if (eq? old (root table))
	   (begin
	     (root-set! table new)
	     result)
	   (loop #f (root table))))))

 (: table-fold (|:table:| (procedure (fixnum * *) *) * -> *))
 (define (table-fold table proc init)
   (check-table table 'fixnum-table-fold)
   (ensure procedure? proc)
   (binding-node-fold
    (lambda (node init) (proc (binding-node-key node) (binding-node-value node) init))
    init (root table)))

 (: table-for-each (|:table:| (procedure (fixnum *) *) -> *))
 (define (table-for-each table proc)
   (check-table table 'fixnum-table-for-each)
   (ensure procedure? proc)
   (binding-node-for-each
    (lambda (node) (proc (binding-node-key node) (binding-node-value node)))
    (root table))
   #f)

 (: table-min (|:table:| (procedure () * *) --> * *))
 (define (table-min table default)
   (check-table table 'fixnum-table-min)
   (let ((node (binding-node-min (root table))))
     (if node (values (binding-node-key node) (binding-node-value node))
	 (begin
	   (ensure procedure? default)
	   (default)))))

 (: table-delete-min! (|:table:| -> * *))
 (define (table-delete-min! table)
   (binding-node-delete-min
    (root table)
    (lambda (root node)
      (root-set! table root)
      (if node
	  (values (binding-node-key node) (binding-node-value node))
	  (values #f #f)))))

 
 )
