#include <stdio.h>
#include <stdlib.h>
#include "eval.h"
#include "math.h"

int tmp;

EXP* evalEXP(EXP *e)
{
    EXP *e1;
    EXP *e2;

    switch (e->kind) {
    case absK:
        e1 = evalEXP(e->val.absE.arg);

        if (e1->kind == intconstK)
            return makeEXPintconst(abs(e1->val.intconstE));
        else 
            return makeEXPabs(e1);
    case powK:
        e1 = evalEXP(e->val.powE.left);
        e2 = evalEXP(e->val.powE.right);

        if (e1->kind == intconstK && e1->val.intconstE == 0 && e2->kind == intconstK && e2->val.intconstE == 0) /* 0^0 = 1 */
            return makeEXPintconst(1);
        else if (e1->kind == intconstK && e1->val.intconstE == 0) /* 0^x = 0 */
            return makeEXPintconst(0);
        else if (e2->kind == intconstK && e2->val.intconstE == 0) /* x^0 = 1 */
            return makeEXPintconst(1); 
        else if (e2->kind == intconstK && e2->val.intconstE == 1) /* x^1 = x */
            return e1;
        else if (e1->kind == intconstK && e1->val.intconstE == 1) /* 1^x = 1 */
            return makeEXPintconst(1); 
        if (e1->kind == intconstK && e2->kind == intconstK)
            return makeEXPintconst(pow(e1->val.intconstE, e2->val.intconstE));
        else 
            return makeEXPpow(e1, e2);
    case timesK:
        e1 = evalEXP(e->val.timesE.left);
        e2 = evalEXP(e->val.timesE.right);

        if (e1->kind == intconstK && e1->val.intconstE == 0) /* 0*x = 0 */
            return makeEXPintconst(0);
        else if (e2->kind == intconstK && e2->val.intconstE == 0) /* x*0 = 0 */
            return makeEXPintconst(0);
        else if (e1->kind == intconstK && e1->val.intconstE == 1) /* 1*x = x */
            return e2;
        else if (e2->kind == intconstK && e2->val.intconstE == 1) /* x*1 = x */
            return e1;
        else if (e1->kind == intconstK && e2->kind == intconstK)
            return makeEXPintconst(e1->val.intconstE * e2->val.intconstE);
        else 
            return makeEXPtimes(e1, e2);
    case divK:
        e1 = evalEXP(e->val.divE.left);
        e2 = evalEXP(e->val.divE.right);

        if (e2->kind == intconstK && e2->val.intconstE == 0) { /* x/0 undefined */
            printf("Exception: cannot divide by zero\n");
            exit(EXIT_FAILURE);
        }
        else if (e1->kind == intconstK && e1->val.intconstE == 0) /* 0/x = 0 */
            return makeEXPintconst(0);
        else if (e2->kind == intconstK && e2->val.intconstE == 1) /* x/1 = x */
            return e1;
        else if (e1->kind == intconstK && e2->kind == intconstK)
            return makeEXPintconst(e1->val.intconstE / e2->val.intconstE);
        else 
            return makeEXPdiv(e1, e2);
    case modK: 
        e1 = evalEXP(e->val.modE.left);
        e2 = evalEXP(e->val.modE.right);

        if (e2->kind == intconstK && e2->val.intconstE == 0) { /* x%0 undefined */
            printf("Exception: invalid modulus\n");
            exit(EXIT_FAILURE);
        }
        else if (e1->kind == intconstK && e1->val.intconstE == 0) /* 0%x = 0 */
            return makeEXPintconst(0);
        else if (e1->kind == intconstK && e2->kind == intconstK)
            return makeEXPintconst(e1->val.intconstE % e2->val.intconstE);
        else 
            return makeEXPmod(e1, e2);
    case plusK:

        e1 = evalEXP(e->val.plusE.left);
        e2 = evalEXP(e->val.plusE.right);

        if (e1->kind == intconstK && e2->kind == intconstK)
            return makeEXPintconst(e1->val.intconstE + e2->val.intconstE);
        else if (e1->kind == intconstK && e1->val.intconstE == 0) /* 0+x = x */
            return e2;
        else if (e2->kind == intconstK && e2->val.intconstE == 0) /* x+0 = x */
            return e1;
        else if (e2->kind == intconstK && e2->val.intconstE < 0) /* x+-i = x-i */
            return makeEXPminus(e1, makeEXPintconst(-(e2->val.intconstE)));
        else 
            return makeEXPplus(e1, e2);
    case minusK:

        e1 = evalEXP(e->val.minusE.left);
        e2 = evalEXP(e->val.minusE.right);

        if (e1->kind == intconstK && e2->kind == intconstK)
            return makeEXPintconst(e1->val.intconstE - e2->val.intconstE);
        else if (e2->kind == intconstK && e2->val.intconstE == 0) /* x-0 = x */
            return e1;
        else 
            return makeEXPminus(e1, e2);
    default:
        return e;
    }
}