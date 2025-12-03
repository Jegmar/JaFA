#ifndef AST_H
#define AST_H

#include <stdlib.h>
#include <stdio.h>

/* --- Expression kinds --- */
typedef enum {
    EX_CONST,
    EX_VAR,
    EX_ADD,
    EX_SUB,
    EX_MUL,
    EX_DIV
} ExprKind;

typedef enum {
    EXPR_INT,
    EXPR_CHAR
} ExprType;

/* --- Expression AST node --- */
typedef struct Expr {
    ExprKind kind;
    struct Expr *left;      // left child (for binary operators)
    struct Expr *right;     // right child (for binary operators)
    union {
        int value;          // for EX_CONST
        char *name;         // for EX_VAR
    };
    ExprType expr_type;
} Expr;


/* --- Constructors --- */
Expr* mk_const(int value);
Expr* mk_var(const char *name);
Expr* mk_bin(ExprKind kind, Expr *left, Expr *right);

/* --- Destructor --- */
void free_expr(Expr *expr);

#endif
