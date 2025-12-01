#include "ast.h"
#include <string.h>

/* --- Create constant node --- */
Expr* mk_const(int value) {
    Expr *e = (Expr*)malloc(sizeof(Expr));
    e->kind = EX_CONST;
    e->value = value;
    e->left = e->right = NULL;
    return e;
}

/* --- Create variable node --- */
Expr* mk_var(const char *name) {
    Expr *e = (Expr*)malloc(sizeof(Expr));
    e->kind = EX_VAR;
    e->name = strdup(name);
    e->left = e->right = NULL;
    return e;
}

/* --- Create binary operator node --- */
Expr* mk_bin(ExprKind kind, Expr *left, Expr *right) {
    Expr *e = (Expr*)malloc(sizeof(Expr));
    e->kind = kind;
    e->left = left;
    e->right = right;
    return e;
}

/* --- Free AST recursively --- */
void free_expr(Expr *expr) {
    if(!expr) return;

    if(expr->kind == EX_VAR && expr->name) {
        free(expr->name);
    }
    if(expr->left) free_expr(expr->left);
    if(expr->right) free_expr(expr->right);

    free(expr);
}
