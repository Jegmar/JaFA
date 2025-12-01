#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "semantics.h"

#define MAX_VARS 100

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
        fprintf(stderr, "Semantic error: Too many variables\n");
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
        fprintf(stderr, "Semantic error at line %d: Undefined variable '%s'\n", line_no, name);
        exit(1);
    }
}

void check_division(int divisor, int line_no) {
    if (divisor == 0) {
        fprintf(stderr, "Semantic error at line %d: Division by zero\n", line_no);
        exit(1);
    }
}

void check_non_negative(int value, char *var_name, int line_no) {
    if (value < 0) {
        fprintf(stderr, "Semantic error at line %d: Variable '%s' cannot be negative\n", line_no, var_name);
        exit(1);
    }
}
