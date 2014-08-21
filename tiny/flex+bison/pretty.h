#ifndef PRETTY_H
#define PRETTY_H

#define BOOL int
#define FALSE 0
#define TRUE 1

#include "tree.h"

void prettyEXP(EXP *e);

/* Helper functions */

void prettyPrint(EXP* e, BOOL isWrap);
BOOL isHigher(EXP *e1, EXP *e2);
int getPrecLevel(opType kind);

#endif /* !PRETTY_H */
