(include "common.scm")
(module
 llrb-generic-tree
 ;; FIXME: This should NOT be part of the same compilation unit as the
 ;; other (tuned) modules.  Here we REALLY should check procedure,
 ;; arguments etc.
 (
  make-llrb-treetype
  llrb-treetype?
  make-binding-set
  empty-binding-set
  binding-set-empty?
  binding-set-ref/default
  binding-set-ref
  binding-set-insert
  binding-set-delete
  binding-set-update
  binding-set-cons
  binding-set-fold
  binding-set-union
  ;;
  make-table
  table?
  table-copy
  table-empty?
  table-delete!
  table-set!
  table-ref/default
  table-ref
  table-update!
  table-fold
  table-for-each
  table-min
  table-delete-min!
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
  (chicken-5 (import srfi-128))
  (else (use comparators)))

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
       ((_ obj typepred loc) (begin))))
   (define-inline (check-keytype type k) #t)
   )
  (else
   (define-syntax typecheck
     (syntax-rules ()
       ((_ obj typetag loc) (##sys#check-structure obj typetag loc))))
   (define-syntax typecheckp
     (syntax-rules ()
       ((_ obj typepred loc) (typepred obj))))
   (define-inline (check-keytype type k)
     (and-let*
      ((p (llrb-tree-type-key-type? type)))
      (or (p k)
	  (error "invalid key" p k))))
   ))

 (define-syntax checkbinding-node
   (syntax-rules ()
     ((_ obj loc) (typecheck obj '<binding-node> loc))))

 (define-inline (make-binding-node color left right key value)
   (make-struct '<binding-node> color left right key value))
 (define (binding-node? obj)
   (##sys#structure? obj '<binding-node>))
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

 (define-syntax binding-node-type
   (syntax-rules ()
     ((_ n) (binding-node-color n))))

 (define-inline (binding-node-type-set+ n t)
   (##sys#setslot n 1 t) n)

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

 (define-type |:table-type:| (struct llrb-tree-type))
 (define-record llrb-tree-type
   key-type?
   lookup
   min
   fold
   for-each
   insert
   delete
   delete-min
   )

 (define llrb-treetype? llrb-tree-type?)

 (define binding-node-init! #f)
 (define binding-node-empty? #f)

 (: make-llrb-treetype* ((or (procedure (*) boolean) false) (procedure (* *) boolean) (procedure (* *) *) --> |:table-type:|))
 (define (make-llrb-treetype* key-type? equal less)

   (define-syntax generic-k-n-eq?
     (syntax-rules () ((_ k n) (equal k (binding-node-key n)))))

   (define-syntax generic-n-n-eq?
     (syntax-rules () ((_ n1 n2) (equal (binding-node-key n1) (binding-node-key n2)))))

   (define-syntax generic-k-n-lt
     (syntax-rules () ((_ k n) (less k (binding-node-key n)))))

   (define-syntax generic-n-n-lt
     (syntax-rules () ((_ node1 node2) (less (binding-node-key node1)
					     (binding-node-key node2)))))

   (define-llrbtree/positional
     (ordered pure)
     binding-set-update-node
     %binding-node-init!     ;; defined
     binding-node-lookup     ;; defined
     binding-node-min	     ;; min defined
     binding-node-fold	     ;; defined
     binding-node-for-each   ;; defined
     binding-node-insert     ;; defined
     binding-node-delete     ;; defined
     binding-node-delete-min ;; defined
     %binding-node-empty?    ;; defined
     generic-k-n-eq?
     generic-n-n-eq?
     generic-k-n-lt
     generic-n-n-lt
     binding-node-left
     binding-node-right
     binding-node-color
     )

   (if (not binding-node-init!)
       (begin
	 (set! binding-node-init! %binding-node-init!)
	 (set! binding-node-empty? %binding-node-empty?)))

   (make-llrb-tree-type
    key-type?
    binding-node-lookup
    binding-node-min
    binding-node-fold
    binding-node-for-each
    binding-node-insert
    binding-node-delete
    binding-node-delete-min
    ))

 (define-type |:mk-tt-1:| (--> |:table-type:|))
 (define-type |:mk-tt-2:| ((struct comparator) --> |:table-type:|))
 (define-type |:mk-tt-3:|
   ((or (procedure (*) boolean) false) (procedure (* *) boolean) (procedure (* *) *)
    --> |:table-type:|))
 (: make-llrb-treetype (or |:mk-tt-1:| |:mk-tt-2:| |:mk-tt-3:|))
 
 (define (make-llrb-treetype . args)
   (cond
    ((null? args)
     (let ((t (make-default-comparator)))
       (make-llrb-treetype* (comparator-type-test-predicate t) (comparator-equality-predicate t) (comparator-ordering-predicate t))))
    ((comparator? (car args))
     (let ((t (car args)))
       (make-llrb-treetype* (comparator-type-test-predicate t) (comparator-equality-predicate t) (comparator-ordering-predicate t))))
    (else (make-llrb-treetype* (car args) (cadr args) (caddr args)))))

 (define (binding-set-empty? x)
   (ensure binding-node? x)
   (binding-node-empty? x)) 

 ;; Constructors

 ;; 0X0

 (define (%make-new-binding-node k v)	; internal/unclean
   (make-binding-node #f #f #f k v))

 (define (empty-binding-set type)
   (binding-node-type-set+ (binding-node-init! (make-binding-node #f #f #f #f #f)) type))

 ;; 0Xpairs

 (: make-binding-set (|:table-type:| &rest -> (struct <binding-node>)))
 (define (make-binding-set type . lst)	; export
   (if (null? lst)
       (empty-binding-set type)
       (let ((insert (llrb-tree-type-insert type))
	     (key-type? (llrb-tree-type-key-type? type)))
	 (do ((lst lst (cdr lst))
	      (set (empty-binding-set type)
		   (let* ((x (car lst)) (k (car x)))
		     (and key-type? (ensure key-type? k))
		     (insert set k #f (%make-new-binding-node k (cdr x)) #f))))
	   ((null? lst) (binding-node-type-set+ set type))))))

 (define (%binding-set-ref/thunk type envt k thunk success) ; internal
   (check-keytype type k)
   (let ((entry ((llrb-tree-type-lookup type) envt k)))
       (if entry (if success (success (binding-node-value entry)) (binding-node-value entry)) (thunk))))

 (define (%binding-set-ref/default type envt k default) ; internal
   (check-keytype type k)
   (let ((entry ((llrb-tree-type-lookup type) envt k)))
     (if entry (binding-node-value entry) default)))

 (: binding-set-ref/default ((struct <binding-node>) * * --> *))
 (define (binding-set-ref/default envt k default) ; export
   (checkbinding-node envt 'binding-set-ref/default)
   (let ((type (binding-node-type envt)))
     (%binding-set-ref/default type envt k default)))

 (: binding-set-ref ((struct <binding-node>) * &optional (procedure () . *) (procedure (*) . *) --> *))
 (define (binding-set-ref envt k . thunk+success) ; export
   (checkbinding-node envt 'binding-set-ref)
   (let ((type (binding-node-type envt)))
     (%binding-set-ref/thunk
      type envt k
      (if (pair? thunk+success) (car thunk+success)
	  (lambda ()
	    (error "binding-set-ref unbound key" k)))
      (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success)))))

 (: binding-set-delete (* (struct <binding-node>) --> (struct <binding-node>)))
 (define (binding-set-delete k envt)
   (checkbinding-node envt 'binding-set-delete)
   (let ((type (binding-node-type envt)))
     (check-keytype type k)
     (binding-node-type-set+ ((llrb-tree-type-delete type) envt k) type)))

 ;; setXkeyXvalue

 (: binding-set-insert ((struct <binding-node>) * * --> (struct <binding-node>)))
 (define (binding-set-insert nodeset k v) ; export
   (checkbinding-node nodeset 'binding-set-insert)
   (let ((type (binding-node-type nodeset)))
     (check-keytype type k)
     (binding-node-type-set+
      ((llrb-tree-type-insert type) nodeset k #f (%make-new-binding-node k v) #f)
      type)))

 (: binding-set-update ((struct <binding-node>)
			* (procedure (*) *) (procedure () *)
			--> (struct <binding-node>)))
 (define (binding-set-update nodeset k update dflt) ; export
   (checkbinding-node nodeset 'binding-set-update)
   (ensure procedure? update)
   (ensure procedure? dflt)
   (let ((type (binding-node-type nodeset)))
     (check-keytype type k)
     (binding-node-type-set+
      ((llrb-tree-type-insert type)
       nodeset k #f
       (let ((update (or update identity)))
	 (lambda (n)
	   (let ((v (update (binding-node-value n))))
	     (make-binding-node #f #f #f (binding-node-key n) v))))
       (lambda () (%make-new-binding-node k (dflt))))
      type)))

 ;; srfi-1::alist-cons compatible
 (: binding-set-cons (* * (struct <binding-node>) --> (struct <binding-node>)))
 (define (binding-set-cons k v nodeset) ; export
   (binding-set-insert nodeset k v))

 (: binding-set-fold ((procedure (* *) . *) * (struct <binding-node>) -> *))
 (define (binding-set-fold kvcons nil nodeset)
   (checkbinding-node nodeset 'binding-set-fold)
   (ensure procedure? kvcons)
   (let ((type (binding-node-type nodeset)))
     ((llrb-tree-type-fold type)
      (lambda (e i) (kvcons (binding-node-key e) (binding-node-value e) i))
      nil nodeset)))

 ;; setXset

 (: binding-set-union
    ((struct <binding-node>) (struct <binding-node>) --> (struct <binding-node>)))
 (define (binding-set-union inner outer) ; export
   (checkbinding-node inner 'binding-set-union)
   (checkbinding-node outer 'binding-set-union)
   (let ((ti (binding-node-type inner))
	 (to (binding-node-type outer)))
     (binding-node-type-set+
      ((llrb-tree-type-fold ti)
       (let ((insert (llrb-tree-type-insert to)))
	 (lambda (node init) (insert init (binding-node-key node) #f node #f)))
       outer inner)
      to)))

 (define-type |:table:| (struct <llrb-generic-table>))
 (define-record-type <llrb-generic-table>
   (%make-generic-table type root)
   table?
   (type llrb-type)
   (root root root-set!))

 (define-syntax retry-alter
   (syntax-rules ()
     ((_ cell var ref set expr)
      (let loop ((var (ref cell)))
	(let ((new expr))
	  (if (eq? var (ref cell))
	      (set cell new)
	      (loop ref)))))))

 (define-syntax check-table
   (syntax-rules ()
     ((_ obj loc) (typecheckp obj table? loc))))

 (: make-table (|:table-type:| --> |:table:|))
 (define (make-table type)
   (ensure llrb-tree-type? type)
   (%make-generic-table type (empty-binding-set type)))

 (: table-copy (|:table:| --> |:table:|))
 (define (table-copy table)
   (check-table table 'generic-table-copy)
   (%make-generic-table (llrb-type table) (root table)))

 (: table-empty? (|:table:| --> boolean))
 (define (table-empty? table)
   (check-table table 'generic-table-empty?)
   (binding-node-empty? (root table)))

 (: table-delete! (|:table:| * -> *))
 (define (table-delete! table key)
   (check-table table 'generic-table-delete!)
   (retry-alter table r root root-set! ((llrb-tree-type-delete (llrb-type table)) r key)))

 (: table-set! (|:table:| * * -> *))
 (define (table-set! table key value)
   (check-table table 'generic-table-set!)
   (let ((nn (%make-new-binding-node key value)))
     (retry-alter table r root root-set! ((llrb-tree-type-insert (llrb-type table)) r key #f nn #f))))
 
 (: table-ref/default (|:table:| * * --> *))
 (define (table-ref/default table key default)
   (check-table table 'generic-table-ref/default)
   (%binding-set-ref/default (llrb-type table) (root table) key default))

 (: table-ref (|:table:| * &optional (procedure () *) (procedure (*) *) -> *))
 (define (table-ref table key . thunk+success)
   (check-table table 'generic-table-ref)
   (%binding-set-ref/thunk
    (llrb-type table) (root table) key
    (if (pair? thunk+success) (car thunk+success)
	(lambda ()
	  (error "generic-table-ref unbound key" key)))
    (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success))))

 (: table-update! (|:table:| * (or false procedure) &rest procedure -> *))
 (define (table-update! table key update . default)
   (check-table table 'generic-table-update!)
   (or (eq? update #f) (ensure procedure? update))
   (let loop ((result #f)
	      (old (root table)))
     (let ((new
	    ((llrb-tree-type-insert (llrb-type table))
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
		   (error "generic-table-update! no default" default))))))
       (if (eq? old (root table))
	   (begin
	     (root-set! table new)
	     result)
	   (loop #f (root table))))))

 (: table-fold (|:table:| (procedure (* * |:table:|) *) * -> *))
 (define (table-fold table proc init)
   (check-table table 'generic-table-fold)
   (ensure procedure? proc)
   ((llrb-tree-type-fold (llrb-type table))
    (lambda (node init) (proc (binding-node-key node) (binding-node-value node) init))
    init (root table)))

 (: table-for-each (|:table:| (procedure (* *) *) -> *))
 (define (table-for-each table proc)
   (check-table table 'generic-table-for-each)
   (ensure procedure? proc)
   ((llrb-tree-type-for-each (llrb-type table))
    (lambda (node) (proc (binding-node-key node) (binding-node-value node)))
    (root table))
   #f)

 (: table-min (|:table:| (procedure () * *) --> * *))
 (define (table-min table default)
   (check-table table 'generic-table-min)
   (let ((node ((llrb-tree-type-min (llrb-type table)) (root table))))
     (if node (values (binding-node-key node) (binding-node-value node))
	 (begin
	   (ensure procedure? default)
	   (default)))))

 (: table-delete-min! (|:table:| -> * *))
 (define (table-delete-min! table)
   ((llrb-tree-type-delete-min (llrb-type table))
    (root table)
    (lambda (root node)
      (root-set! table root)
      (if node
	  (values (binding-node-key node) (binding-node-value node))
	  (values #f #f)))))

 )
