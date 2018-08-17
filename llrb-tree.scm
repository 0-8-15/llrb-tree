(declare
 ;; This wants to be an alternative to hash tables.  Thus our updates
 ;; must share the atomicity properties.
 (disable-interrupts)

 (no-argc-checks)
 (no-bound-checks)
 (no-procedure-checks)
 (safe-globals)
 (specialize)
 (foreign-declare
 #<<EOF
C_inline void C_ccall C_make_structureX(C_word c, C_word *av)
{
  C_word av2[2], *v=av+1;
  av2[0] = av[1];
  *v = C_STRUCTURE_TYPE | (c-2);
  av2[1] = (C_word) v;
  C_do_apply(2, av2);
}

EOF
)

 )

(require-library comparators)

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
 (import scheme chicken foreign)
 (import (only data-structures identity))
 (import (only lolevel mutate-procedure!))
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
       ((_ obj typetag loc) (begin)))))
  (else
   (define-syntax typecheck
     (syntax-rules ()
       ((_ obj typetag loc) (##sys#check-structure obj typetag loc))))))

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
     ((_ obj loc) (typecheck obj '<llrb-string-table> loc))))

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
 (import scheme chicken foreign)
 (import (only data-structures identity))
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
       ((_ obj typetag loc) (begin)))))
  (else
   (define-syntax typecheck
     (syntax-rules ()
       ((_ obj typetag loc) (##sys#check-structure obj typetag loc))))))

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

 (define-type :table: (struct <llrb-fixnum-table>))
 (define-record-type <llrb-fixnum-table>
   (%make-fixnum-table root)
   table?
   (root root root-set!))

 (define-syntax check-table
   (syntax-rules ()
     ((_ obj loc) (typecheck obj '<llrb-fixnum-table> loc))))

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

 (: make-table (--> :table:))

 (define make-table
   (let ((n0 (binding-node-init! (make-binding-node #f #f #f  #f #f))))
     (lambda () (%make-fixnum-table n0))))

 (: table-copy (:table: --> :table:))
 (define (table-copy table)
   (check-table table 'fixnum-table-copy)
   (%make-fixnum-table (root table)))

 (: table-empty? (:table: --> boolean))
 (define (table-empty? table)
   (ensure table? table)
   (binding-node-empty? (root table)))

 (: table-delete! (:table: fixnum -> *))
 (define (table-delete! table key)
   (check-table table 'fixnum-table-delete!)
   (ensure fixnum? key)
   (root-set! table (binding-node-delete (root table) key)))

 (: table-set! (:table: fixnum * -> boolean))
 (define (table-set! table key value)
   (check-table table 'fixnum-table-set!)
   (ensure fixnum? key)
   (root-set! table (binding-node-insert (root table) key #f (make-binding-node #f #f #f key value) #f)))

 (: table-ref/default (:table: fixnum * --> *))
 (define (table-ref/default table key default)
   (check-table table 'fixnum-table-ref/default)
   (ensure fixnum? key)
   (let ((node (binding-node-lookup (root table) key)))
     (if node (binding-node-value node) default)))

 (: table-ref (:table: fixnum &optional (procedure () *) (procedure (*) *) -> *))
 (define (table-ref table key . thunk+success)
   (check-table table 'fixnum-table-ref)
   (ensure fixnum? key)
   (let ((node (binding-node-lookup (root table) key)))
     (if node (if (and (pair? thunk+success) (pair? (cdr thunk+success)))
		  ((cadr thunk+success) (binding-node-value node))
		  (binding-node-value node))
	 (if (pair? thunk+success) ((car thunk+success))
	     (error "fixnum-table-ref no key" key)))))

 (: table-update! (:table: fixnum procedure &rest procedure -> *))
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

 (: table-fold (:table: (procedure (fixnum * *) *) * -> *))
 (define (table-fold table proc init)
   (check-table table 'fixnum-table-fold)
   (ensure procedure? proc)
   (binding-node-fold
    (lambda (node init) (proc (binding-node-key node) (binding-node-value node) init))
    init (root table)))

 (: table-for-each (:table: (procedure (fixnum *) *) -> *))
 (define (table-for-each table proc)
   (check-table table 'fixnum-table-for-each)
   (ensure procedure? proc)
   (binding-node-for-each
    (lambda (node) (proc (binding-node-key node) (binding-node-value node)))
    (root table))
   #f)

 (: table-min (:table: (procedure () * *) --> * *))
 (define (table-min table default)
   (check-table table 'fixnum-table-min)
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

 
 )

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
 (import scheme chicken foreign)
 (import (only data-structures identity))
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
       ((_ obj typetag loc) (begin)))))
  (else
   (define-syntax typecheck
     (syntax-rules ()
       ((_ obj typetag loc) (##sys#check-structure obj typetag loc))))))

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

 (define-type :table: (struct <llrb-symbol-table>))
 (define-record-type <llrb-symbol-table>
   (%make-symbol-table root)
   table?
   (root root root-set!))

 (define-syntax check-table
   (syntax-rules ()
     ((_ obj loc) (typecheck obj '<llrb-symbol-table> loc))))

 (: make-table ( --> :table:))
 (define (make-table)
   (%make-symbol-table (empty-binding-set)))

 (: table-copy (:table: --> :table:))
 (define (table-copy table)
   (check-table table 'symbol-table-copy)
   (%make-symbol-table (root table)))

 (: table-delete! (:table: symbol -> *))
 (define (table-delete! table key)
   (check-table table 'symbol-table-delete!)
   (ensure symbol? key)
   (root-set! table (binding-node-delete (root table) (%symbol->string key))))

 (: table-set! (:table: symbol * -> *))
 (define (table-set! table key value)
   (check-table table 'symbol-table-set!)
   (ensure symbol? key)
   (let ((key (%symbol->string key)))
     (root-set! table (%binding-set-insert (root table) key #f (%make-new-binding-node key value) #f))))

 (: table-ref/default (:table: symbol * --> *))
 (define (table-ref/default table key default)
   (check-table table 'symbol-table-ref/default)
   (ensure symbol? key)
   (%binding-set-ref/default (root table) (%symbol->string key) default))

 (: table-ref (:table: symbol &optional (procedure () *) (procedure (*) *) -> *))
 (define (table-ref table key . thunk+success)
   (check-table table 'symbol-table-ref)
   (ensure symbol? key)
   (%binding-set-ref/thunk
    (root table) (%symbol->string key)
    (if (pair? thunk+success) (car thunk+success)
	(lambda ()
	  (error "symbol-table-ref unbound key" key)))
    (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success))))

 (: table-update! (:table: symbol procedure &rest procedure -> *))
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
 (import scheme chicken foreign)
 (import (only data-structures identity))
 (import llrb-syntax)
 ;;(include "llrbsyn.scm")
 (import comparators)

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
   (define-inline (check-keytype type k) #t)
   )
  (else
   (define-syntax typecheck
     (syntax-rules ()
       ((_ obj typetag loc) (##sys#check-structure obj typetag loc))))
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

 (define-type :table-type: (struct llrb-tree-type))
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

 (: make-llrb-treetype* ((or (procedure (*) boolean) false) (procedure (* *) boolean) (procedure (* *) *) --> :table-type:))
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

 (define-type :mk-tt-1: (--> :table-type:))
 (define-type :mk-tt-2: ((struct comparator) --> :table-type:))
 (define-type :mk-tt-3:
   ((or (procedure (*) boolean) false) (procedure (* *) boolean) (procedure (* *) *)
    --> :table-type:))
 (: make-llrb-treetype (or :mk-tt-1: :mk-tt-2: :mk-tt-3:))
 
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

 (: make-binding-set (:table-type: &rest -> (struct <binding-node>)))
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

 (define-type :table: (struct <llrb-generic-table>))
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
     ((_ obj loc) (typecheck obj '<llrb-generic-table> loc))))

 (: make-table (:table-type: --> :table:))
 (define (make-table type)
   (ensure llrb-tree-type? type)
   (%make-generic-table type (empty-binding-set type)))

 (: table-copy (:table: --> :table:))
 (define (table-copy table)
   (check-table table 'generic-table-copy)
   (%make-generic-table (llrb-type table) (root table)))

 (: table-empty? (:table: --> boolean))
 (define (table-empty? table)
   (check-table table 'generic-table-empty?)
   (binding-node-empty? (root table)))

 (: table-delete! (:table: * -> *))
 (define (table-delete! table key)
   (check-table table 'generic-table-delete!)
   (retry-alter table r root root-set! ((llrb-tree-type-delete (llrb-type table)) r key)))

 (: table-set! (:table: * * -> *))
 (define (table-set! table key value)
   (check-table table 'generic-table-set!)
   (let ((nn (%make-new-binding-node key value)))
     (retry-alter table r root root-set! ((llrb-tree-type-insert (llrb-type table)) r key #f nn #f))))
 
 (: table-ref/default (:table: * * --> *))
 (define (table-ref/default table key default)
   (check-table table 'generic-table-ref/default)
   (%binding-set-ref/default (llrb-type table) (root table) key default))

 (: table-ref (:table: * &optional (procedure () *) (procedure (*) *) -> *))
 (define (table-ref table key . thunk+success)
   (check-table table 'generic-table-ref)
   (%binding-set-ref/thunk
    (llrb-type table) (root table) key
    (if (pair? thunk+success) (car thunk+success)
	(lambda ()
	  (error "generic-table-ref unbound key" key)))
    (and (pair? thunk+success) (pair? (cdr thunk+success)) (cadr thunk+success))))

 (: table-update! (:table: * (or false procedure) &rest procedure -> *))
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

 (: table-fold (:table: (procedure (* * :table:) *) * -> *))
 (define (table-fold table proc init)
   (check-table table 'generic-table-fold)
   (ensure procedure? proc)
   ((llrb-tree-type-fold (llrb-type table))
    (lambda (node init) (proc (binding-node-key node) (binding-node-value node) init))
    init (root table)))

 (: table-for-each (:table: (procedure (* *) *) -> *))
 (define (table-for-each table proc)
   (check-table table 'generic-table-for-each)
   (ensure procedure? proc)
   ((llrb-tree-type-for-each (llrb-type table))
    (lambda (node) (proc (binding-node-key node) (binding-node-value node)))
    (root table))
   #f)

 (: table-min (:table: (procedure () * *) --> * *))
 (define (table-min table default)
   (check-table table 'generic-table-min)
   (let ((node ((llrb-tree-type-min (llrb-type table)) (root table))))
     (if node (values (binding-node-key node) (binding-node-value node))
	 (begin
	   (ensure procedure? default)
	   (default)))))

 (: table-delete-min! (:table: -> * *))
 (define (table-delete-min! table)
   ((llrb-tree-type-delete-min (llrb-type table))
    (root table)
    (lambda (root node)
      (root-set! table root)
      (if node
	  (values (binding-node-key node) (binding-node-value node))
	  (values #f #f)))))

 )

;; This mutating version will go away.  It is left here for now to
;; show how bad a difference the mutations are.
(include "fixnum-table-mutating.scm")

(module
 llrb-tree
 *
 (import scheme chicken)
 (import (prefix llrb-m-fixnum-table mu:))
 ;; This is a bit unfortune as it doubles several prefixes.
 ;;
 ;; At the other hand removing those in the source makes it harder to
 ;; maintain.  Therfore one should better import those specialized
 ;; versions directly if used. 
 (reexport (prefix llrb-string-table string-)
	   (prefix llrb-fixnum-table fixnum-)
	   (prefix llrb-symbol-tree symbol-)
	   llrb-generic-tree)
 (define str2sym string-str2sym)
 )
