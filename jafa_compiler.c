#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "jafa_compiler.h"
#include "ast.h"
#include "semantics.h"

int REG_COUNT = 1;

char data_buffer[65536];
char assembly_buffer[65536];
char machine_buffer[65536];
int data_pos = 0, asm_pos = 0, mach_pos = 0;

void init_buffers() {
    data_pos = asm_pos = mach_pos = 0;
    REG_COUNT = 1;
    data_buffer[0] = assembly_buffer[0] = machine_buffer[0] = '\0';
}

int next_reg() { return REG_COUNT++; }

/* Append helpers */
void append_data(const char* s) {
    int len = strlen(s);
    if(data_pos + len >= 65536) { fprintf(stderr, "Data buffer overflow\n"); exit(1); }
    strcpy(&data_buffer[data_pos], s);
    data_pos += len;
}

void append_asm(const char* s) {
    int len = strlen(s);
    if(asm_pos + len >= 65536) { fprintf(stderr, "Assembly buffer overflow\n"); exit(1); }
    strcpy(&assembly_buffer[asm_pos], s);
    asm_pos += len;
}

void append_mach(const char* s) {
    int len = strlen(s);
    if(mach_pos + len >= 65536) { fprintf(stderr, "Machine buffer overflow\n"); exit(1); }
    strcpy(&machine_buffer[mach_pos], s);
    mach_pos += len;
}

/* Append string directly to .data buffer */
void append_to_data(const char *s) {
    int len = strlen(s);
    if (data_pos + len >= 65536) { 
        fprintf(stderr,"Data buffer overflow\n"); 
        exit(1); 
    }
    strcpy(&data_buffer[data_pos], s);
    data_pos += len;
}

/* --- Machine code generation --- */
void print_machine_code(const char* instr, int r1, int r2, int r3, int imm) {
    unsigned int code = 0;
    if(strcmp(instr,"DADDIU")==0) code = 0x24000000 | ((r2&0x1F)<<21) | ((r1&0x1F)<<16) | (imm&0xFFFF);
    else if(strcmp(instr,"SD")==0) code = 0xF8000000 | ((r2&0x1F)<<21) | ((r1&0x1F)<<16) | (imm&0xFFFF);
    else if(strcmp(instr,"SB")==0) code = 0xA0000000 | ((r2&0x1F)<<21) | ((r1&0x1F)<<16) | (imm&0xFFFF);
    else if(strcmp(instr,"DADDU")==0) code = 0x00000021 | ((r2&0x1F)<<21) | ((r3&0x1F)<<16) | ((r1&0x1F)<<11);
    else if(strcmp(instr,"DSUBU")==0) code = 0x00000023 | ((r2&0x1F)<<21) | ((r3&0x1F)<<16) | ((r1&0x1F)<<11);
    else if(strcmp(instr,"DMULTU")==0) code = 0x00000019 | ((r2&0x1F)<<21) | ((r3&0x1F)<<16);
    else if(strcmp(instr,"DDIVU")==0) code = 0x0000001B | ((r2&0x1F)<<21) | ((r3&0x1F)<<16);
    else if(strcmp(instr,"MFLO")==0) code = 0x00001012 | ((r1&0x1F)<<11);
    else { char tmp[128]; sprintf(tmp,"// Unsupported instruction %s\n", instr); append_mach(tmp); return; }

    char line[128];
    for(int i=31;i>=0;i--) line[31-i] = ((code>>i)&1)+'0';
    line[32]='\0';
    char final[160];
    sprintf(final,"%s >> 0x%08X\n",line,code);
    append_mach(final);
}

/* --- Declare variables --- */
void declare_entero(const char* name, int init_val) {
    char line[64];
    sprintf(line,"%s: .dword 0x%08X\n", name, init_val);
    append_data(line);
}

void declare_letra(const char* name, int init_val) {
    char line[64];
    sprintf(line,"%s: .byte 0x%02X\n", name, init_val);
    append_data(line);
}

/* --- Recursive code generation for expressions --- */
int generate_expr_code(Expr *expr) {
    if(!expr) return 0;
    int r;
    char line[128];

    switch(expr->kind) {
        case EX_CONST:
            r = next_reg();
            sprintf(line,"DADDIU R%d, R0, #%d\n", r, expr->value);
            append_asm(line);
            print_machine_code("DADDIU", r, 0, 0, expr->value);
            return r;

        case EX_VAR:
            r = next_reg();
            sprintf(line,"LD R%d, %s(R0)\n", r, expr->name); // Load variable
            append_asm(line);
            append_mach("// LD instruction placeholder\n");
            return r;

        case EX_ADD:
        case EX_SUB:
        case EX_MUL:
        case EX_DIV: {
            int r1 = next_reg();
            int r2 = generate_expr_code(expr->left);
            int r3 = generate_expr_code(expr->right);
            switch(expr->kind) {
                case EX_ADD: sprintf(line,"DADDU R%d, R%d, R%d\n", r1,r2,r3); append_asm(line); print_machine_code("DADDU",r1,r2,r3,0); break;
                case EX_SUB: sprintf(line,"DSUBU R%d, R%d, R%d\n", r1,r2,r3); append_asm(line); print_machine_code("DSUBU",r1,r2,r3,0); break;
                case EX_MUL: 
                    sprintf(line,"DMULTU R%d, R%d\n", r2,r3); append_asm(line); print_machine_code("DMULTU",r2,r3,0,0);
                    sprintf(line,"MFLO R%d\n", r1); append_asm(line); print_machine_code("MFLO",r1,0,0,0); 
                    break;
                case EX_DIV:
                    sprintf(line,"DDIVU R%d, R%d\n", r2,r3); append_asm(line); print_machine_code("DDIVU",r2,r3,0,0);
                    sprintf(line,"MFLO R%d\n", r1); append_asm(line); print_machine_code("MFLO",r1,0,0,0); 
                    break;
            }
            return r1;
        }
    }
    return 0;
}

/* --- Evaluate expression to integer value --- */
int eval_expr(Expr *expr) {
    if(!expr) return 0;
    switch(expr->kind) {
        case EX_CONST: return expr->value;
        case EX_VAR: {
            int idx = find_var(expr->name);
            if(idx != -1) return sym_table[idx].value;
            return 0;
        }
        case EX_ADD: return eval_expr(expr->left) + eval_expr(expr->right);
        case EX_SUB: return eval_expr(expr->left) - eval_expr(expr->right);
        case EX_MUL: return eval_expr(expr->left) * eval_expr(expr->right);
        case EX_DIV: {
            int r = eval_expr(expr->right);
            return r != 0 ? eval_expr(expr->left)/r : 0;
        }
    }
    return 0;
}

/* --- Generate variable code with expression --- */
void generate_entero_with_expr(const char* name, Expr *expr) {
    int val = eval_expr(expr);
    declare_entero(name, val);
    int r = generate_expr_code(expr);
    char line[128];
    sprintf(line,"SD R%d, %s(R0)\n", r,name);
    append_asm(line);
    print_machine_code("SD", r, 0, 0, 0);

    int idx = find_var(name);
    if(idx != -1) sym_table[idx].value = val;
}

void generate_letra_with_expr(const char* name, Expr *expr) {
    int val = eval_expr(expr);
    declare_letra(name, val);
    int r = generate_expr_code(expr);
    char line[128];
    sprintf(line,"SB R%d, %s(R0)\n", r,name);
    append_asm(line);
    print_machine_code("SB", r, 0, 0, 0);

    int idx = find_var(name);
    if(idx != -1) sym_table[idx].value = val;
}
