/*
 * Implementation of peephole patterns to optimize A- joos compilers
 *
 * Modified by Group-a: David Thibodeau, Ioannis Fytilis, Sherry Shanshan Ruan
 */

/*
 * JOOS is Copyright (C) 1997 Laurie Hendren & Michael I. Schwartzbach
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will the authors be liable for any damages resulting from
 * use of this software.
 *
 * email: hendren@cs.mcgill.ca, mis@brics.dk
 */

/* iload x        iload x        iload x
 * ldc 0          ldc 1          ldc 2
 * imul           imul           imul
 * ------>        ------>        ------>
 * ldc 0          iload x        iload x
 *                               dup
 *                               iadd
 */

int simplify_multiplication_right(CODE **c)
{ int x,k;
  if (is_iload(*c,&x) && 
      is_ldc_int(next(*c),&k) && 
      is_imul(next(next(*c)))) {
     if (k==0) return replace(c,3,makeCODEldc_int(0,NULL));
     else if (k==1) return replace(c,3,makeCODEiload(x,NULL));
     else if (k==2) return replace(c,3,makeCODEiload(x,
                                       makeCODEdup(
                                       makeCODEiadd(NULL))));
     return 0;
  }
  return 0;
}

/* dup
 * astore x
 * pop
 * -------->
 * astore x
 */
int simplify_astore(CODE **c)
{ int x;
  if (is_dup(*c) &&
      is_astore(next(*c),&x) &&
      is_pop(next(next(*c)))) {
     return replace(c,3,makeCODEastore(x,NULL));
  }
  return 0;
}

/* iload x
 * ldc k   (0<=k<=127)
 * iadd
 * istore x
 * --------->
 * iinc x k
 */ 
int positive_increment(CODE **c)
{ int x,y,k;
  if (is_iload(*c,&x) &&
      is_ldc_int(next(*c),&k) &&
      is_iadd(next(next(*c))) &&
      is_istore(next(next(next(*c))),&y) &&
      x==y && 0<=k && k<=127) {
     return replace(c,4,makeCODEiinc(x,k,NULL));
  }
  return 0;
}

/* goto L1
 * ...
 * L1:
 * goto L2
 * ...
 * L2:
 * --------->
 * goto L2
 * ...
 * L1:    (reference count reduced by 1)
 * goto L2
 * ...
 * L2:    (reference count increased by 1)  
 */
int simplify_goto_goto(CODE **c)
{ int l1,l2;
  if (is_goto(*c,&l1) && is_goto(next(destination(l1)),&l2) && l1>l2) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEgoto(l2,NULL));
  }
  return 0;
}

/**************** The following patterns are added by Group-a ****************/

/* iload x
 * ldc k   (0<=k<=127)
 * isub
 * istore x
 * --------->
 * iinc x -k
 */ 
int positive_decrement(CODE **c)
{ int x,y,k;
  if (is_iload(*c,&x) &&
      is_ldc_int(next(*c),&k) &&
      is_isub(next(next(*c))) &&
      is_istore(next(next(next(*c))),&y) &&
      x==y && 0<=k && k<=127) {
     return replace(c,4,makeCODEiinc(x,(-1)*k,NULL));
  }
  return 0;
}

/* dup 
 * istore x
 * pop 
 * --------->
 * istore x
 */
int simplify_istore(CODE **c)
{ int x;
  if (is_dup(*c) &&
      is_istore(next(*c),&x) &&
      is_pop(next(next(*c)))) {
     return replace(c,3,makeCODEistore(x,NULL));
  }
  return 0;
} 

/* ldc_int x
 * ldc_int y
 * imul 
 * --------->
 * ldc_int x*y
 */
int simplify_mul(CODE **c)
{ int x,y;
  if (is_ldc_int(*c, &x) &&
      is_ldc_int(next(*c), &y) &&
      is_imul(next(next(*c)))) {
     return replace(c,3,makeCODEldc_int(x*y,NULL));
  }
  return 0;
}


/* ldc_string x (so the top of the stack cannot be null)
 * dup 
 * ifnull L1
 * goto L2
 * L1:
 * pop
 * ldc_string "null" (in fact we don't care about this line)
 * L2:
 * ...
 * --------->
 * ldc_string x
 * L2:
 * ...
 */
int remove_ifnull_check(CODE **c)
{ int l1,l2,l11,l22;
  char *s;
  if (is_ldc_string(*c, &s) &&
      is_dup(next(*c)) &&
      is_ifnull(next(next(*c)), &l1) &&
      is_goto(next(next(next(*c))), &l2) &&
      is_label(next(next(next(next(*c)))), &l11) && l11==l1 && uniquelabel(l1) &&
      is_pop(next(next(next(next(next(*c)))))) &&
      is_label(next(next(next(next(next(next(next(*c))))))), &l22) && l22==l2) {
    droplabel(l1);
    return replace(c,8,makeCODEldc_string(s,makeCODElabel(l2,NULL)));
  }
  return 0;
}

/* dup 
 * aload x
 * swap 
 * putfield *arg
 * pop 
 * --------->
 * aload x
 * swap 
 * putfield *arg
 */
int simplify_dup_swap_pop(CODE **c)
{ int x;
  char *arg;
  if (is_dup(*c) &&
      is_aload(next(*c),&x) &&
      is_swap(next(next(*c))) &&
      is_putfield(next(next(next(*c))),&arg) &&
      is_pop(next(next(next(next(*c)))))) {
     return replace (c,5,makeCODEaload(x,makeCODEswap(makeCODEputfield(arg,NULL))));
  }
  return 0;
}

/* nop
 * --------->
 * (Null)
 */
int remove_nop(CODE **c)
{ if (is_nop(*c)) {
     kill_line(c);
  }
  return 0;
}

/* aload x
 * aload y
 * swap
 * --------->
 * aload y
 * aload x
 */
int simplify_swap_aa(CODE **c)
{ int x,y;
  if (is_aload(*c,&x) &&
      is_aload(next(*c),&y) &&
      is_swap(next(next(*c)))) {
     return replace (c,3,makeCODEaload(y,makeCODEaload(x,NULL)));
  }
  return 0;
}

/* iload x
 * aload y
 * swap
 * --------->
 * aload y
 * iload x
 */
int simplify_swap_ia(CODE **c)
{ int x,y;
  if (is_iload(*c,&x) &&
      is_aload(next(*c),&y) &&
      is_swap(next(next(*c)))) {
     return replace (c,3,makeCODEaload(y,makeCODEiload(x,NULL)));
  }
  return 0;
}

/* ldc_int x
 * aload y
 * swap
 * --------->
 * aload y
 * ldc_int x
 */
int simplify_swap_inta(CODE **c)
{ int x,y;
  if (is_ldc_int(*c,&x) &&
      is_aload(next(*c),&y) &&
      is_swap(next(next(*c)))) {
     return replace (c,3,makeCODEaload(y,makeCODEldc_int(x,NULL)));
  }
  return 0;
}

/* ldc_string x
 * aload y
 * swap
 * --------->
 * aload y
 * ldc_string x
 */
int simplify_swap_stringa(CODE **c)
{ int y;
  char *x;
  if (is_ldc_string(*c,&x) &&
      is_aload(next(*c),&y) &&
      is_swap(next(next(*c)))) {
     return replace (c,3,makeCODEaload(y,makeCODEldc_string(x,NULL)));
  }
  return 0;
}

/* aconst_null
 * aload y
 * swap
 * --------->
 * aload y
 * aconst_null
 */
int simplify_swap_nulla(CODE **c)
{ int y;
  if (is_aconst_null(*c) &&
      is_aload(next(*c),&y) &&
      is_swap(next(next(*c)))) {
     return replace (c,3,makeCODEaload(y,makeCODEaconst_null(NULL)));
  }
  return 0;
}

/* istore x
 * iload x
 * ireturn
 * --------->
 * ireturn
 */
int simplify_istore_iload(CODE **c)
{ int x,y;
  if (is_istore(*c,&x) &&
      is_iload(next(*c), &y) &&
      x==y &&
      is_areturn(next(next(*c)))) {
    return replace(c,2,NULL);
  }
  return 0;
}

/* astore x
 * aload x
 * areturn
 * --------->
 * areturn
 */
int simplify_astore_aload(CODE **c)
{ int x,y;
  if (is_astore(*c,&x) &&
      is_aload(next(*c), &y) &&
      x==y &&
      is_areturn(next(next(*c)))) {
    return replace(c,2,NULL);
  }
  return 0;
}

/* ifnull L1
 * goto L2
 * L1:
 * ... stuff 1 ...
 * L2:
 * ... stuff 2 ...
 * --------->
 * ifnonnull L2 
 * L1:              (reference count decreased by 1)
 * ... stuff 1 ...
 * L2:              (reference count unchanged)
 * ... stuff 2 ...
 */
int simplify_ifnull(CODE **c)
{ int l1,l2,l11;
  if (is_ifnull(*c,&l1) &&
      is_goto(next(*c),&l2) &&
      is_label(next(next(*c)),&l11) && l11==l1) {
     droplabel(l1);
     return replace (c,2,makeCODEifnonnull(l2,NULL));
  }
  return 0;
}

/* To deal with labels, we reconnect them and reduce the count of unecessary labels */
/* We don't remove labels here  
 * We only remove them systematically in remove_deadlabels pattern */

/* if_icmplt L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ......
 * L3:
 * --------->
 * if_icmpge L3 
 * L1:                  (reference count decreased by one)
 * L2:                  (reference count decreased by one)
  ......
 * L3:                  (reference count unchanged)
 */
int simplify_if_icmplt(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_icmplt(*c, &l1) &&
      is_ldc_int(next(*c), &x) && x==0 &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1 && uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y) && y==1 &&
      is_label(next(next(next(next(next(*c))))), &l22) &&  l22==l2 && uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_icmpge(l3,makeCODElabel(l1,(makeCODElabel(l2,NULL)))));
  }
  return 0;
}

/* if_icmple L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ......
 * L3:
 * --------->
 * if_icmpgt L3 
 * ......
 * L3:
 */
int simplify_if_icmple(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_icmple(*c, &l1) &&
      is_ldc_int(next(*c), &x) && x==0 &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1 && uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y) && y==1 &&
      is_label(next(next(next(next(next(*c))))), &l22) && l22==l2 && uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_icmpgt(l3,makeCODElabel(l1,(makeCODElabel(l2,NULL)))));
  }
  return 0;
}

/* if_icmpgt L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ...
 * L3:
 * --------->
 * if_icmple L3 
 * ...
 * L3:
 */
int simplify_if_icmpgt(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_icmpgt(*c, &l1) &&
      is_ldc_int(next(*c), &x) && x==0 &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1 && uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y) && y==1 &&
      is_label(next(next(next(next(next(*c))))), &l22) && l2==l22 && uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_icmple(l3,makeCODElabel(l1,(makeCODElabel(l2,NULL)))));
  }
  return 0;
}

/* if_icmpge L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ...
 * L3:
 * --------->
 * if_icmplt L3 
 * ...
 * L3:
 */
int simplify_if_icmpge(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_icmpge(*c, &l1) &&
      is_ldc_int(next(*c), &x) && x==0 &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1 &&  uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y) && y==1 &&
      is_label(next(next(next(next(next(*c))))), &l22) &&  l22==l2 && uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_icmplt(l3,makeCODElabel(l1,(makeCODElabel(l2,NULL)))));
  }
  return 0;
}

/* if_icmpeq L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ...
 * L3:
 * --------->
 * if_icmpne L3 
 * ...
 * L3:
 */
int simplify_if_icmpeq(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_icmpeq(*c, &l1) &&
      is_ldc_int(next(*c), &x) && x==0 &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1  && uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y) && y==1 &&
      is_label(next(next(next(next(next(*c))))), &l22) && l22==l2 && uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_icmpne(l3,makeCODElabel(l1,makeCODElabel(l2,NULL))));
  }
  return 0;
}

/* if_icmpne L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ...
 * L3:
 * --------->
 * if_icmpeq L3 
 * ...
 * L3:
 */
int simplify_if_icmpne(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_icmpne(*c, &l1) &&
      is_ldc_int(next(*c), &x) && x==0 &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1 && uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y) && y==1 &&
      is_label(next(next(next(next(next(*c))))), &l22) && l22==l2 && uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_icmpeq(l3,makeCODElabel(l1,(makeCODElabel(l2,NULL)))));
  }
  return 0;
}

/* if_acmpeq L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ...
 * L3:
 * --------->
 * if_acmpne L3 
 * ...
 * L3:
 */
int simplify_if_acmpeq(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_acmpeq(*c, &l1) &&
      is_ldc_int(next(*c), &x) && x==0 &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1 && uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y) && y==1 &&
      is_label(next(next(next(next(next(*c))))), &l22) && l2==l22 && uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_acmpne(l3,makeCODElabel(l1,(makeCODElabel(l2,NULL)))));
  }
  return 0;
}

/* if_acmpne L1
 * ldc_int 0 
 * goto L2
 * L1:
 * ldc_int 1 
 * L2:
 * ifeq L3
 * ...
 * L3:
 * --------->
 * if_acmpeq L3 
 * ...
 * L3:
 */
int simplify_if_acmpne(CODE **c)
{ int l1,l11,l2,l22,l3;
  int x,y;
  if (is_if_acmpne(*c, &l1) &&
      is_ldc_int(next(*c), &x) &&
      is_goto(next(next(*c)), &l2) &&
      is_label(next(next(next(*c))),&l11) && l11==l1 && uniquelabel(l1) &&
      is_ldc_int(next(next(next(next(*c)))), &y)  &&
      is_label(next(next(next(next(next(*c))))), &l22) && l2==l22 &&uniquelabel(l2) &&
      is_ifeq(next(next(next(next(next(next(*c)))))), &l3)) {
     droplabel(l1);
     droplabel(l2);
     return replace(c,7,makeCODEif_acmpeq(l3,makeCODElabel(l1,(makeCODElabel(l2,NULL)))));
  }
  return 0;
}

/* ifeq L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * ifeq L2:
 * ...
 */
int simplify_ifeq_goto(CODE **c)
{ int l1,l2;
  if (is_ifeq(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEifeq(l2,NULL));
  }
  return 0;
}

/* ifne L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * ifne L2:
 * ...
 */
int simplify_ifne_goto(CODE **c)
{ int l1,l2;
  if (is_ifne(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEifne(l2,NULL));
  }
  return 0;
}

/* ifnull L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * ifnull L2:
 * ...
 */
int simplify_ifnull_goto(CODE **c)
{ int l1,l2;
  if (is_ifnull(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEifnull(l2,NULL));
  }
  return 0;
}

/* ifnonnull L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * ifnonnull L2:
 * ...
 */
int simplify_ifnonnull_goto(CODE **c)
{ int l1,l2;
  if (is_ifnonnull(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEifnonnull(l2,NULL));
  }
  return 0;
}

/* if_acmpeq L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_acmpeq L2:
 * ...
 */
int simplify_if_acmpeq_goto(CODE **c)
{ int l1,l2;
  if (is_if_acmpeq(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_acmpeq(l2,NULL));
  }
  return 0;
}

/* if_acmpne L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_acmpne L2:
 * ...
 */
int simplify_if_acmpne_goto(CODE **c)
{ int l1,l2;
  if (is_if_acmpne(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_acmpne(l2,NULL));
  }
  return 0;
}

/* if_icmpeq L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_icmpeq L2:
 * ...
 */
int simplify_if_icmpeq_goto(CODE **c)
{ int l1,l2;
  if (is_if_icmpeq(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_icmpeq(l2,NULL));
  }
  return 0;
}

/* if_icmpne L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_icmpne L2:
 * ...
 */
int simplify_if_icmpne_goto(CODE **c)
{ int l1,l2;
  if (is_if_icmpne(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_icmpne(l2,NULL));
  }
  return 0;
}

/* if_icmple L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_icmple L2:
 * ...
 */
int simplify_if_icmple_goto(CODE **c)
{ int l1,l2;
  if (is_if_icmple(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_icmple(l2,NULL));
  }
  return 0;
}

/* if_icmpge L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_icmpge L2:
 * ...
 */
int simplify_if_icmpge_goto(CODE **c)
{ int l1,l2;
  if (is_if_icmpge(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_icmpge(l2,NULL));
  }
  return 0;
}

/* if_icmplt L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_icmplt L2:
 * ...
 */
int simplify_if_icmplt_goto(CODE **c)
{ int l1,l2;
  if (is_if_icmplt(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_icmplt(l2,NULL));
  }
  return 0;
}

/* if_icmpgt L1:
 * ...
 * L1:
 * goto L2:
 * --------->
 * if_icmpgt L2:
 * ...
 */
int simplify_if_icmpgt_goto(CODE **c)
{ int l1,l2;
  if (is_if_icmpgt(*c, &l1) &&
      is_goto(next(destination(l1)), &l2)) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEif_icmpgt(l2,NULL));
  }
  return 0;
}

/* goto L1:
 * L1:
 * --------->
 * L1:
 */
int remove_redundant_goto(CODE **c)
{ int l1,l2;
  if (is_goto(*c, &l1) &&
      is_label(next(*c), &l2) && l1==l2)  {
     droplabel(l1);
     return replace(c,1,NULL);
  }
  return 0;
}

/* areturn
 * goto L
 * --------->
 * areturn
 */
int remove_goto_after_areturn(CODE **c)
{ int l;
  if (is_areturn(*c) &&
      is_goto(next(*c),&l)) {
      droplabel(l);
      return replace(c,2,makeCODEareturn(NULL));
  }
  return 0;
}

/* ireturn
 * goto L
 * --------->
 * areturn
 */
int remove_goto_after_ireturn(CODE **c)
{ int l;
  if (is_ireturn(*c) &&
      is_goto(next(*c),&l)) {
      droplabel(l);
      return replace(c,2,makeCODEireturn(NULL));
  }
  return 0;
}

/* return
 * goto L
 * --------->
 * return
 */
int remove_goto_after_return(CODE **c)
{ int l;
  if (is_return(*c) &&
      is_goto(next(*c),&l)) {
      droplabel(l);
      return replace(c,2,makeCODEreturn(NULL));
  }
  return 0;
}

/* goto L
 * ...
 * L:
 * ireturn
 * --------->
 * ireturn
 * ...
 * L:
 * ireturn
 */
int remove_ireturn_label(CODE **c)
{ int l;
  if (is_goto(*c,&l) &&
      is_ireturn(next(destination(l)))){
    droplabel(l);
    return replace(c,1,makeCODEireturn(NULL));
  }
  return 0;
}

/* goto L
 * ...
 * L:
 * areturn
 * --------->
 * areturn
 * ...
 * L:
 * areturn
 */
int remove_areturn_label(CODE **c)
{ int l;
  if (is_goto(*c,&l) &&
      is_areturn(next(destination(l)))){
    droplabel(l);
    return replace(c,1,makeCODEareturn(NULL));
  }
  return 0;
}

/* goto L
 * ...
 * L:
 * return
 * --------->
 * return
 * ...
 * L:
 * return
 */
int remove_return_label(CODE **c)
{ int l;
  if (is_goto(*c,&l) &&
      is_return(next(destination(l)))){
    droplabel(l);
    return replace(c,1,makeCODEreturn(NULL));
  }
  return 0;
}

/* helper: suppress labels
 * goto L1:
 * ...
 * L1:
 * L2:
 * --------->
 * goto L2:
 * ...
 * L1:
 * L2:
 */
int suppress_goto_label(CODE **c)
{ int l1, l2;
  if (is_goto(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEgoto(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * ifeq L1:
 * ...
 * L1:
 * L2:
 * --------->
 * ifeq L2:
 * ...
 * L1:
 * L2:
 */
int suppress_ifeq_label(CODE **c)
{ int l1, l2;
  if (is_ifeq(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEifeq(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * ifne L1:
 * ...
 * L1:
 * L2:
 * --------->
 * ifne L2:
 * ...
 * L1:
 * L2:
 */
int suppress_ifne_label(CODE **c)
{ int l1, l2;
  if (is_ifne(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEifne(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * ifnull L1:
 * ...
 * L1:
 * L2:
 * --------->
 * ifnull L2:
 * ...
 * L1:
 * L2:
 */
int suppress_ifnull_label(CODE **c)
{ int l1, l2;
  if (is_ifnull(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEifnull(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * ifnonnull L1:
 * ...
 * L1:
 * L2:
 * --------->
 * ifnonnull L2:
 * ...
 * L1:
 * L2:
 */
int suppress_ifnonnull_label(CODE **c)
{ int l1, l2;
  if (is_ifnonnull(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEifnonnull(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_icmplt L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_icmplt L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_icmplt_label(CODE **c)
{ int l1, l2;
  if (is_if_icmplt(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_icmplt(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_icmple L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_icmple L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_icmple_label(CODE **c)
{ int l1, l2;
  if (is_if_icmple(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_icmple(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_icmpgt L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_icmpgt L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_icmpgt_label(CODE **c)
{ int l1, l2;
  if (is_if_icmpgt(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_icmpgt(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_icmpge L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_icmpge L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_icmpge_label(CODE **c)
{ int l1, l2;
  if (is_if_icmpge(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_icmpge(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_icmpeq L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_icmpeq L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_icmpeq_label(CODE **c)
{ int l1, l2;
  if (is_if_icmpeq(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_icmpeq(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_icmpne L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_icmpne L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_icmpne_label(CODE **c)
{ int l1, l2;
  if (is_if_icmpne(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_icmpne(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_acmpeq L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_acmpeq L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_acmpeq_label(CODE **c)
{ int l1, l2;
  if (is_if_acmpeq(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_acmpeq(l2,NULL));
  }
  return 0;
}

/* helper: suppress labels
 * if_acmpne L1:
 * ...
 * L1:
 * L2:
 * --------->
 * if_acmpne L2:
 * ...
 * L1:
 * L2:
 */
int suppress_if_acmpne_label(CODE **c)
{ int l1, l2;
  if (is_if_acmpne(*c,&l1) &&
      is_label(next(destination(l1)), &l2)) {
    droplabel(l1);
    copylabel(l2);
    return replace(c,1,makeCODEif_acmpne(l2,NULL));
  }
  return 0;
}

/* L: (deadlabel)
 * --------->
 */
int remove_deadlabel(CODE **c)
{ int l;
  if (is_label(*c, &l) &&
      deadlabel(l)) {
    return replace (c,1,NULL);
  }
  return 0;
}

/* The above patterns can achieve the same optimization as a+ compiler */

/*************** further optimization ***************/

/* This pattern is observed by comparing joosa- and javac */
/* dup
 * ifnonnull L1
 * pop
 * ldc "null"
 * L1:
 * --------->
 * L1:
 */
int remove_ifnonnull_check(CODE **c)
{ int l1,l11;
  char *s;
  if (is_dup(*c) &&
      is_ifnonnull(next(*c),&l1) &&
      is_pop(next(next(*c))) &&
      is_ldc_string(next(next(next(*c))), &s) &&
      (strcmp(s,"null")==0) &&
      is_label(next(next(next(next(*c)))), &l11) &&
      l11==l1) {
    droplabel(l1);
    return replace (c,4,NULL);
  }
return 0;
}

/******  Old style - still works, but better to use new style. 
#define OPTS 4

OPTI optimization[OPTS] = {simplify_multiplication_right,
                           simplify_astore,
                           positive_increment,
                           simplify_goto_goto};
********/

/* new style for giving patterns */

int init_patterns()
  { ADD_PATTERN(simplify_multiplication_right);
    ADD_PATTERN(simplify_astore);
    ADD_PATTERN(positive_increment);
    ADD_PATTERN(simplify_goto_goto);

/* The following patterns are added by Group-a */
    ADD_PATTERN(positive_decrement);
    ADD_PATTERN(simplify_istore);
    ADD_PATTERN(simplify_mul);
    ADD_PATTERN(simplify_dup_swap_pop);
    ADD_PATTERN(remove_nop);
    ADD_PATTERN(remove_ifnull_check);
    ADD_PATTERN(simplify_istore_iload);
    ADD_PATTERN(simplify_astore_aload);
    /* simplify swap */
    ADD_PATTERN(simplify_swap_aa);
    ADD_PATTERN(simplify_swap_ia);
    ADD_PATTERN(simplify_swap_inta);
    ADD_PATTERN(simplify_swap_stringa);
    ADD_PATTERN(simplify_swap_nulla);
    /* simplify if */
    ADD_PATTERN(simplify_ifnull);
    ADD_PATTERN(simplify_if_icmplt);
    ADD_PATTERN(simplify_if_icmple);
    ADD_PATTERN(simplify_if_icmpgt);
    ADD_PATTERN(simplify_if_icmpge);
    ADD_PATTERN(simplify_if_icmpeq);
    ADD_PATTERN(simplify_if_icmpne);
    ADD_PATTERN(simplify_if_acmpeq);
    ADD_PATTERN(simplify_if_acmpne);
    /* simplify if and goto */
    ADD_PATTERN(simplify_ifeq_goto);
    ADD_PATTERN(simplify_ifne_goto);
    ADD_PATTERN(simplify_if_acmpeq_goto);
    ADD_PATTERN(simplify_if_acmpne_goto);
    ADD_PATTERN(simplify_if_icmpeq_goto);
    ADD_PATTERN(simplify_if_icmpne_goto);
    ADD_PATTERN(simplify_if_icmplt_goto);
    ADD_PATTERN(simplify_if_icmpgt_goto);
    ADD_PATTERN(simplify_if_icmple_goto);
    ADD_PATTERN(simplify_if_icmpge_goto);
    ADD_PATTERN(simplify_ifnull_goto);
    ADD_PATTERN(simplify_ifnonnull_goto);
    /* simplify label */
    ADD_PATTERN(suppress_goto_label);
    ADD_PATTERN(suppress_ifeq_label);
    ADD_PATTERN(suppress_ifne_label);
    ADD_PATTERN(suppress_ifnull_label);
    ADD_PATTERN(suppress_ifnonnull_label);
    ADD_PATTERN(suppress_if_icmplt_label);
    ADD_PATTERN(suppress_if_icmple_label);
    ADD_PATTERN(suppress_if_icmpgt_label);
    ADD_PATTERN(suppress_if_icmpge_label);
    ADD_PATTERN(suppress_if_icmpeq_label);
    ADD_PATTERN(suppress_if_icmpne_label);
    ADD_PATTERN(suppress_if_acmpeq_label);
    ADD_PATTERN(suppress_if_acmpne_label);
    /* remove unnecessary goto */
    ADD_PATTERN(remove_redundant_goto);
    /* remove deadlabels systematically */
    ADD_PATTERN(remove_deadlabel);
    /* simplify return */
    ADD_PATTERN(remove_goto_after_ireturn);
    ADD_PATTERN(remove_goto_after_areturn);
    ADD_PATTERN(remove_goto_after_return);
    ADD_PATTERN(remove_ireturn_label);
    ADD_PATTERN(remove_areturn_label);
    ADD_PATTERN(remove_return_label);
    /* The above patterns give the same result as a+ compiler */

    /*************** Further optimization ***************/
    /* this pattern is detected when comparing with javac compiler */
    ADD_PATTERN(remove_ifnonnull_check);
    
    return 1;
  }
