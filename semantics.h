#ifndef SEMANTICS_H
#define SEMANTICS_H

#include <stdio.h>
#include "ast.h"

/* ---------------- Variable Definition ---------------- */
#define TYPE_INT 0
#define TYPE_CHAR 1

typedef struct {
    char *name;
    int value;   // store numeric or char value
    int type;    // TYPE_INT or TYPE_CHAR
} Variable;
/* ---------------- Symbol Table ---------------- */
extern Variable sym_table[];
extern int var_count;

/* ---------------- Symbol Table Functions ---------------- */
int find_var(const char *name);
int add_var(const char *name, int type);

/* ---------------- Semantic Checks ---------------- */
void check_undefined(char *name, int line_no);
void check_division(int divisor, int line_no);
void check_non_negative(int value, char *var_name, int line_no);
void check_assignment_type(int var_type, ExprType expr_type, const char *name, int line_no);
void check_expr_type(Expr *expr, int expected_type);


#endif
