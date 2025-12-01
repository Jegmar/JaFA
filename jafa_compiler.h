#ifndef JAFA_COMPILER_H
#define JAFA_COMPILER_H

#include "ast.h"

// Generate next register
int next_reg();

// Generate assembly + machine code for entero (int) variable or expression
void generate_entero(const char* name, int value);
void generate_entero_with_expr(const char* name, Expr *expr);

// Generate assembly + machine code for letra (char) variable or expression
void generate_letra(const char* name, char c);
void generate_letra_with_expr(const char* name, Expr *expr);

// Declare variables directly (with optional initial value)
void declare_entero(const char* name, int init_val);
void declare_letra(const char* name, int init_val);

// Append string to .data, .code, machine buffers
void append_data(const char *s);
void append_asm(const char *s);
void append_mach(const char *s);
void append_to_data(const char *s);   // <<--- THIS WAS MISSING

// Initialize buffers
void init_buffers();

// Buffers to store generated assembly and machine code
extern char data_buffer[65536];
extern char assembly_buffer[65536];
extern char machine_buffer[65536];
extern int data_pos;
extern int asm_pos;
extern int mach_pos;

#endif
