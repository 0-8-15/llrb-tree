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
static
void C_ccall C_make_structureX(C_word c, C_word *av)
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
