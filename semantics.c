#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "semantics.h"
#include "ast.h"

#define MAX_VARS 100

/* Bison line number tracking */
extern int yylineno;

/* ---------------- Symbol Table ---------------- */
Variable sym_table[MAX_VARS];
int var_count = 0;

/* ---------------- Symbol Table Functions ---------------- */
int find_var(const char *name) {
    for (int i = 0; i < var_count; i++) {
        if (strcmp(sym_table[i].name, name) == 0)
            return i;
    }
    return -1;
}

int add_var(const char *name, int type) {
    if (var_count >= MAX_VARS) {
        fprintf(stderr, "\nError: semantic error.\n Too many variables.\n");
        exit(1);
    }
    sym_table[var_count].name = strdup(name);
    sym_table[var_count].value = 0;
    sym_table[var_count].type = type;
    return var_count++;
}

/* ---------------- Semantic Checks ---------------- */
void check_undefined(char *name, int line_no) {
    if (find_var(name) == -1) {
        fprintf(stderr, "\nError at line %d: semantic error.\nUndefined variable '%s'.\n", line_no, name);
        exit(1);
    }
}

void check_division(int divisor, int line_no) {
    if (divisor == 0) {
        fprintf(stderr, "\nError at line %d: semantic error.\nDivision by zero.\n", line_no);
        exit(1);
    }
}

void check_non_negative(int value, char *var_name, int line_no) {
    if (value < 0) {
        fprintf(stderr, "\nError at line %d: semantic error.\nVariable '%s' cannot be negative.\n", line_no, var_name);
        exit(1);
    }
}

void check_assignment_type(int var_type, ExprType expr_type,const char *name, int line_no) {
    if(var_type == TYPE_CHAR && expr_type != EXPR_CHAR) {
        fprintf(stderr,
            "\nERROR: cannot assign a value of type '%s' to variable '%s' of type 'char' (line %d)\n"
            "NOTE: only character literals (e.g., 'A') are allowed for 'char' variables\n",
            expr_type == EXPR_INT ? "int" : "unknown", name, line_no);
        exit(1);
    }
    if(var_type == TYPE_INT && expr_type == EXPR_CHAR) {
        fprintf(stderr,
            "\nERROR: cannot assign a value of type 'char' to variable '%s' of type 'int' (line %d)\n"
            "NOTE: consider converting the character to an integer explicitly if intended\n",
            name, line_no);
        exit(1);
    }
}

void check_expr_type(Expr *expr, int expected_type) {
    if (!expr) return;

    if (expr->kind == EX_VAR) {
        int idx = find_var(expr->name);
        if (idx != -1) {
            int var_type = sym_table[idx].type;
            if (var_type != expected_type) {
                fprintf(stderr,
                    "\nError at line %d: semantic error.\nVariable '%s' of type '%s' cannot be used in a %s expression.\n"
                    "Hint: variables of type '%s' cannot be mixed in expressions of type '%s'.\n\n",
                    yylineno,
                    expr->name,
                    var_type == TYPE_INT ? "int" : "char",
                    expected_type == TYPE_INT ? "integer" : "char",
                    var_type == TYPE_INT ? "int" : "char",
                    expected_type == TYPE_INT ? "integer" : "char");

                exit(1);
            }
        }
    }

    check_expr_type(expr->left, expected_type);
    check_expr_type(expr->right, expected_type);
}
