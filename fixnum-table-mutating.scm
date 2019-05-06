(module
 llrb-m-fixnum-table
 (
  make-fixnum-table
  fixnum-table-ref
  fixnum-table-ref/default
  fixnum-table-set!
  fixnum-table-update!
  fixnum-table-update!1
  fixnum-table-delete!
  fixnum-table-for-each
  fixnum-table-fold
  fixnum-table/min fixnum-table/delete-min!
  )
 (import scheme chicken llrb-syntax)
 (import (only data-structures identity))

 ;; ordered allocation free, impure fixnum llrb tree

 (cond-expand
  (never
   (define-type :llrb-node: (struct <llrb-node>))
   (define-record-type <llrb-node>
     (make-llrb-node color left right key value)
     llrb-node?
     (color llrb-node-color llrb-node-color-set!)
     (left llrb-node-left llrb-node-left-set!)
     (right llrb-node-right llrb-node-right-set!)
     (key llrb-node-key llrb-node-key-set!)
     (value llrb-node-value llrb-node-value-set!)))
  (else
   (define-type :llrb-node: (vector * * * * *))
   (define-inline (make-llrb-node color left right key value)
     (##sys#vector color left right key value))
   (define-inline (llrb-node-color n) (##sys#slot n 0))
   (define-inline (llrb-node-color-set! n v) (##sys#setislot n 0 v))
   (define-inline (llrb-node-left n) (##sys#slot n 1))
   (define-inline (llrb-node-left-set! n v) (##sys#setslot n 1 v))
   (define-inline (llrb-node-right n) (##sys#slot n 2))
   (define-inline (llrb-node-right-set! n v) (##sys#setslot n 2 v))
   (define-inline (llrb-node-key n) (##sys#slot n 3))
   (define-inline (llrb-node-key-set! n v) (##sys#setislot n 3 v))
   (define-inline (llrb-node-value n) (##sys#slot n 4))
   (define-inline (llrb-node-value-set! n v) (##sys#setslot n 4 v))))


 (define-type |:table:| (struct <llrb-fixnum-table>))
 (define-record-type <llrb-fixnum-table>
   (%make-fixnum-table root)
   fixnum-table?
   (root root))

 (define-syntax llrb-node-update
   (syntax-rules (left: right: color:)
     ((_ 1 n l r c ())
      (make-llrb-node c l r (llrb-node-key n) (llrb-node-value n)))
     ((_ 1 n l r c (left: v . more))
      (llrb-node-update 1 n v r c more))
     ((_ 1 n l r c (right: v . more))
      (llrb-node-update 1 n l v c more))
     ((_ 1 n l r c (color: v . more))
      (llrb-node-update 1 n l r v more))
     ((_ n . more)
      (llrb-node-update
       1 n (llrb-node-left n) (llrb-node-right n) (llrb-node-color n) more))
     ))

 (define-syntax llrb-node-update!
   (syntax-rules (left: right: color:)
     ((_ n) n)
     ((_ n left: v . more)
      (begin
	;;(llrb-node-left-set! n v)
	(if (not (eq? (llrb-node-left n) v)) (llrb-node-left-set! n v))
	(llrb-node-update n . more)))
     ((_ n right: v . more)
      (begin
	;;(llrb-node-right-set! n v)
	(if (not (eq? (llrb-node-right n) v)) (llrb-node-right-set! n v))
	(llrb-node-update n . more)))
     ((_ n color: v . more)
      (begin
	;;(llrb-node-color-set! n v)
	(if (not (eq? (llrb-node-color n) v)) (llrb-node-color-set! n v))
	(llrb-node-update n . more)))
     ))

 (define-syntax llrb-node-key-node-eq?
   (syntax-rules ()
     ((_ key node) (fx= key (llrb-node-key node)))))

 (define-syntax llrb-node-key-node-lt
   (syntax-rules () ((_ key node) (fx< key (llrb-node-key node)))))

 (define-syntax llrb-node-node-node-=?
   (syntax-rules ()
     ((_ node1 node2) (fx= (llrb-node-key node1) (llrb-node-key node2)))))

 (define-syntax llrb-node-node-node-lt
   (syntax-rules ()
     ((_ node1 node2) (fx< (llrb-node-key node1) (llrb-node-key node2)))))

 (define-llrbtree/positional
   (ordered)
   llrb-node-update!
   llrb-node-init!	      ;; defined
   llrb-node-node-lookup      ;; defined
   llrb-node-min	      ;; defined
   llrb-node-node-fold	      ;; defined
   llrb-node-node-for-each    ;; defined
   llrb-node-node-insert!     ;; defined
   llrb-node-delete!	      ;; defined
   llrb-node-node-delete-min! ;; defined
   llrb-node-empty?	      ;; defined
   llrb-node-key-node-eq?
   llrb-node-node-node-=?
   llrb-node-key-node-lt
   llrb-node-node-node-lt     ;; before? node order
   llrb-node-left
   llrb-node-right
   llrb-node-color
   )

 (: make-fixnum-table (--> |:table:|))

 (define (make-fixnum-table)
   (%make-fixnum-table (llrb-node-init! (make-llrb-node #f #f #f  #f #f))))

 (: fixnum-table-empty? (|:table:| --> boolean))
 (define (fixnum-table-empty? table)
   (ensure fixnum-table? table)
   (llrb-node-empty? (root table)))

 (: fixnum-table-delete! (|:table:| fixnum -> *))
 (define (fixnum-table-delete! table key)
   (ensure fixnum? key)
   (ensure fixnum-table? table)
   (llrb-node-delete! (root table) key))

 (: fixnum-table-set! (|:table:| fixnum * -> boolean))
 (define (fixnum-table-set! table key value)
   (ensure fixnum? key)
   (ensure fixnum-table? table)
   #;(let ((node (llrb-node-node-lookup (root table) key)))
     (if node
	 (llrb-node-value-set! node value)
	 (llrb-node-node-insert! (root table) key #f (make-llrb-node #f #f #f key value) #f)))
   (llrb-node-node-insert! (root table) key #f (make-llrb-node #f #f #f key value) #f)
   #t)

 (: fixnum-table-update! (|:table:| fixnum procedure &rest procedure -> *))
 (define (fixnum-table-update!1 table key update . default)
   (ensure fixnum? key)
   (ensure fixnum-table? table)
   (ensure procedure? update)
   (let ((node (llrb-node-node-lookup (root table) key)))
     (if node
	 (let ((result (update (llrb-node-value node))))
	   (llrb-node-value-set! node result)
	   result)
	 (let ((result (if (and (pair? default) (procedure? (car default)))
			   (update ((car default)))
			   (error "fixnum-table-update! no default" default))))
	   (llrb-node-node-insert! (root table) key #f (make-llrb-node #f #f #f key result) #f)
	   result))))

 (define (fixnum-table-update! table key update . default)
   (ensure fixnum? key)
   (ensure fixnum-table? table)
   (ensure procedure? update)
   (let ((result #f))
     (llrb-node-node-insert!
      (root table) key #f
      (let ((update (or update identity)))
	(lambda (n)
	  (let ((v (update (llrb-node-value n))))
	    (set! result v)
	    (make-llrb-node #f #f #f (llrb-node-key n) v))))
      (if (and (pair? default) (procedure? (car default)))
	  (let ((thunk (car default)))
	    (lambda () (make-llrb-node #f #f #f key (thunk))))
	  (lambda () (error "fixnum-table-update! no default" default))))
     result))

 (: fixnum-table-ref/default (|:table:| fixnum * --> *))
 (define (fixnum-table-ref/default table key default)
   (ensure fixnum? key)
   (ensure fixnum-table? table)
   (let ((node (llrb-node-node-lookup (root table) key)))
     (if node (llrb-node-value node) default)))

 (: fixnum-table-ref (|:table:| fixnum (procedure () *) -> *))
 (define (fixnum-table-ref table key default)
   (ensure fixnum? key)
   (ensure fixnum-table? table)
   (let ((node (llrb-node-node-lookup (root table) key)))
     (if node (llrb-node-value node) (default))))

 (: fixnum-table-fold ((procedure (fixnum * *) *) * |:table:| -> *))
 (define (fixnum-table-fold proc init table)
   (ensure fixnum-table? table)
   (llrb-node-node-fold
    (lambda (node init) (proc (llrb-node-key node) (llrb-node-value node) init))
    init (root table)))

 (: fixnum-table-for-each ((procedure (fixnum *) *) |:table:| -> *))
 (define (fixnum-table-for-each proc table)
   (ensure fixnum-table? table)
   (llrb-node-node-for-each
    (lambda (node) (proc (llrb-node-key node) (llrb-node-value node)))
    (root table))
   #f)

 (: fixnum-table/min (|:table:| (procedure () * *) --> * *))
 (define (fixnum-table/min table default)
   (ensure fixnum-table? table)
   (let ((node (llrb-node-min (root table))))
     (if node (values (llrb-node-key node) (llrb-node-value node)) (default))))

 (: fixnum-table/delete-min! (|:table:| (procedure () fixnum *) -> * *))
 (define (fixnum-table/delete-min! table default)
   (ensure fixnum-table? table)
   (let ((node (llrb-node-node-delete-min! (root table))))
     (if node (values (llrb-node-key node) (llrb-node-value node)) (default))))

 
 )
