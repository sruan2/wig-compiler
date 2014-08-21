#ifndef TREE_H
#define TREE_H
 
typedef enum e_opType {idK,intconstK,timesK,divK,plusK,minusK, modK, powK, absK} opType;

typedef struct EXP {
  int lineno;
  opType kind;
  union {
    char *idE;
    int intconstE;
    struct {struct EXP *left; struct EXP *right;} timesE;
    struct {struct EXP *left; struct EXP *right;} divE;
    struct {struct EXP *left; struct EXP *right;} plusE;
    struct {struct EXP *left; struct EXP *right;} minusE;
    struct {struct EXP *left; struct EXP *right;} modE;
    struct {struct EXP *left; struct EXP *right;} powE;
    struct {struct EXP *arg;} absE;
  } val;
} EXP;
 
EXP *makeEXPid(char *id);

EXP *makeEXPintconst(int intconst);

EXP *makeEXPtimes(EXP *left, EXP *right);

EXP *makeEXPdiv(EXP *left, EXP *right);

EXP *makeEXPplus(EXP *left, EXP *right);

EXP *makeEXPminus(EXP *left, EXP *right);

EXP *makeEXPmod(EXP *left, EXP *right);

EXP *makeEXPpow(EXP *left, EXP *right);

EXP *makeEXPabs(EXP *arg);

#endif /* !TREE_H */
