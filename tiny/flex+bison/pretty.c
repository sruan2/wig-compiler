#include <stdio.h>
#include "pretty.h"
 
void prettyEXP(EXP *e)
{ 
    prettyPrint(e, FALSE);
}

/* This is where I wish I had inner functions in C */
void prettyPrint(EXP *e, BOOL isWrap)
{ 
    if (isWrap) printf("(");
    switch (e->kind) {
    case idK:
        printf("%s",e->val.idE);
        break;
    case intconstK:
        printf("%i",e->val.intconstE);
        break;
    case timesK:
        prettyPrint(e->val.timesE.left, isHigher(e, e->val.timesE.left) && e->val.timesE.left->kind != timesK);
        printf("*");
        prettyPrint(e->val.timesE.right, !isHigher(e->val.timesE.right, e) && e->val.timesE.right->kind != timesK);
        break;
    case divK:
        prettyPrint(e->val.divE.left, isHigher(e, e->val.divE.left));
        printf("/");
        prettyPrint(e->val.divE.right, !isHigher(e->val.divE.right, e));
        break;
    case modK:
        prettyPrint(e->val.modE.left, isHigher(e, e->val.modE.left));
        printf("%%");
        prettyPrint(e->val.modE.right, !isHigher(e->val.modE.right, e));
        break;
    case plusK:
        prettyPrint(e->val.plusE.left, isHigher(e, e->val.plusE.left) && e->val.plusE.left->kind != plusK);
        printf("+");
        prettyPrint(e->val.plusE.right, !isHigher(e->val.plusE.right, e) && e->val.plusE.right->kind != plusK);
        break;
    case minusK:
        prettyPrint(e->val.minusE.left, isHigher(e, e->val.minusE.left));
        printf("-");
        prettyPrint(e->val.minusE.right, !isHigher(e->val.minusE.right, e));
        break;
    case powK:
        prettyPrint(e->val.powE.left, !isHigher(e->val.powE.left, e));
        printf("**");
        prettyPrint(e->val.powE.right, isHigher(e, e->val.powE.right));
        break;
    case absK:
        printf("abs");
        prettyPrint(e->val.absE.arg, TRUE);
        break;
    }
    if (isWrap) printf(")");
}

BOOL isHigher(EXP *e1, EXP *e2) 
{
    int e1prec = getPrecLevel(e1->kind);
    int e2prec = getPrecLevel(e2->kind);
    return e1prec > e2prec;
}

int getPrecLevel(opType kind) 
{
    switch(kind) 
    {
        case plusK: 
        case minusK: 
            return 0;
            break;
        case timesK:
        case divK:
        case modK:
            return 1;
            break;
        case powK:
            return 2;
            break;
        case absK:
        case intconstK:
        case idK:
            return 3;
            break;
    }
}

