(module
 llrb-tree
 *
 (import scheme)
 (cond-expand
  (chicken-4 (import chicken))
  (else (import (chicken module) (chicken base))))
 (require-library llrb-generic-tree llrb-symbol-tree llrb-fixnum-table llrb-string-table)
 ;; (import (prefix llrb-m-fixnum-table mu:))
 ;; This is a bit unfortune as it doubles several prefixes.
 ;;
 ;; Now that the file is split, I'm actually usure why we should have the toplevel module at all.
 (reexport (prefix llrb-string-table string-)
	   (prefix llrb-fixnum-table fixnum-)
	   (prefix llrb-symbol-tree symbol-)
	   llrb-generic-tree)
 (define str2sym string-str2sym)
 )
