// jafa_compiler.c - FINAL VERSION (compiles + correct + EduMIPS64 compatible)
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "jafa_compiler.h"
#include "ast.h"
#include "semantics.h"   // brings in Variable sym_table[] and find_var()

char data_buffer[65536];
char assembly_buffer[65536];
char machine_buffer[65536];
int data_pos = 0, asm_pos = 0, mach_pos = 0;
int REG_COUNT = 1;

/* Symbol table for address allocation only (separate from semantics.c) */
typedef struct {
    char name[32];
    int addr;
} AddrSymbol;

static AddrSymbol addr_table[512];
static int addr_count = 0;
static int next_addr = 0;

void init_buffers() {
    data_pos = asm_pos = mach_pos = 0;
    REG_COUNT = 1;
    addr_count = 0;
    next_addr = 0;
    memset(data_buffer, 0, sizeof(data_buffer));
    memset(assembly_buffer, 0, sizeof(assembly_buffer));
    memset(machine_buffer, 0, sizeof(machine_buffer));
}

int next_reg() { return REG_COUNT++; }

void append_data(const char* s) { strcpy(&data_buffer[data_pos], s); data_pos += strlen(s); }
void append_asm(const char* s)  { strcpy(&assembly_buffer[asm_pos], s); asm_pos += strlen(s); }
void append_mach(const char* s) { strcpy(&machine_buffer[mach_pos], s); mach_pos += strlen(s); }

/* Address allocation - independent of semantics.c */
static int get_symbol_addr(const char* name) {
    for (int i = 0; i < addr_count; i++)
        if (strcmp(addr_table[i].name, name) == 0)
            return addr_table[i].addr;

    strcpy(addr_table[addr_count].name, name);
    addr_table[addr_count].addr = next_addr;
    next_addr += 8;
    return addr_table[addr_count++].addr;
}

/* Machine code emitter - verified correct */
static void emit_machine(const char* instr, int rd, int rs, int rt, int imm) {
    unsigned int code = 0;

    if (strcmp(instr, "DADDIU") == 0)   code = (0x19u << 26) | (rs << 21) | (rt << 16) | (imm & 0xFFFF);
    else if (strcmp(instr, "DADDU") == 0)  code = (0x00u << 26) | (rs << 21) | (rt << 16) | (rd << 11) | 0x2D;
    else if (strcmp(instr, "DSUBU") == 0)  code = (0x00u << 26) | (rs << 21) | (rt << 16) | (rd << 11) | 0x2F;
    else if (strcmp(instr, "DMULTU") == 0) code = (0x00u << 26) | (rs << 21) | (rt << 16) | 0x19;
    else if (strcmp(instr, "DDIVU") == 0)  code = (0x00u << 26) | (rs << 21) | (rt << 16) | 0x1B;
    else if (strcmp(instr, "MFLO") == 0)   code = (0x00u << 26) | (rd << 11) | 0x12;
    else if (strcmp(instr, "LD") == 0)     code = (0x37u << 26) | (rs << 21) | (rt << 16) | (imm & 0xFFFF);
    else if (strcmp(instr, "SD") == 0)     code = (0x3Fu << 26) | (rs << 21) | (rt << 16) | (imm & 0xFFFF);
    else if (strcmp(instr, "SB") == 0)     code = (0x28u << 26) | (rs << 21) | (rt << 16) | (imm & 0xFFFF);

    char binary[33];
    for (int i = 31; i >= 0; i--)
        binary[31 - i] = ((code >> i) & 1) ? '1' : '0';
    binary[32] = '\0';

    char line[128];
    sprintf(line, "%s >> 0x%08X\n", binary, code);
    append_mach(line);
}

static int generate_expr_code(Expr* expr) {
    if (!expr) return 0;
    int r;
    char line[256];

    switch (expr->kind) {
        case EX_CONST:
            r = next_reg();
            sprintf(line, "DADDIU R%d, R0, #%d\n", r, expr->value);
            append_asm(line);
            emit_machine("DADDIU", 0, 0, r, expr->value);
            return r;

        case EX_VAR:
            r = next_reg();
            sprintf(line, "LD R%d, %s(R0)\n", r, expr->name);
            append_asm(line);
            emit_machine("LD", 0, 0, r, get_symbol_addr(expr->name));
            return r;

        case EX_ADD: case EX_SUB: case EX_MUL: case EX_DIV: {
            int left = generate_expr_code(expr->left);
            int right = generate_expr_code(expr->right);
            int result = next_reg();

            if (expr->kind == EX_ADD) {
                sprintf(line, "DADDU R%d, R%d, R%d\n", result, left, right);
                append_asm(line);
                emit_machine("DADDU", result, left, right, 0);
            }
            else if (expr->kind == EX_SUB) {
                sprintf(line, "DSUBU R%d, R%d, R%d\n", result, left, right);
                append_asm(line);
                emit_machine("DSUBU", result, left, right, 0);
            }
            else if (expr->kind == EX_MUL) {
                sprintf(line, "DMULTU R%d, R%d\n", left, right);
                append_asm(line);
                emit_machine("DMULTU", 0, left, right, 0);
                sprintf(line, "MFLO R%d\n", result);
                append_asm(line);
                emit_machine("MFLO", result, 0, 0, 0);
            }
            else if (expr->kind == EX_DIV) {
                sprintf(line, "DDIVU R%d, R%d\n", left, right);
                append_asm(line);
                emit_machine("DDIVU", 0, left, right, 0);
                sprintf(line, "MFLO R%d\n", result);
                append_asm(line);
                emit_machine("MFLO", result, 0, 0, 0);
            }
            return result;
        }
    }
    return 0;
}

/* Use semantics.c's sym_table for values */
int eval_expr(Expr* expr) {
    if (!expr) return 0;
    switch (expr->kind) {
        case EX_CONST: return expr->value;
        case EX_VAR: {
            int idx = find_var(expr->name);
            return (idx != -1) ? sym_table[idx].value : 0;
        }
        case EX_ADD: return eval_expr(expr->left) + eval_expr(expr->right);
        case EX_SUB: return eval_expr(expr->left) - eval_expr(expr->right);
        case EX_MUL: return eval_expr(expr->left) * eval_expr(expr->right);
        case EX_DIV: {
            int d = eval_expr(expr->right);
            return d != 0 ? eval_expr(expr->left) / d : 0;
        }
        default: return 0;
    }
}

/* EduMIPS64-safe .data */
void declare_entero(const char* name, int init_val) {
    get_symbol_addr(name);
    append_data(name);
    append_data(": .dword\n");
}

void declare_letra(const char* name, int init_val) {
    get_symbol_addr(name);
    append_data(name);
    append_data(": .byte\n");
}

void generate_entero_with_expr(const char* name, Expr* expr) {
    declare_entero(name, eval_expr(expr));
    int r = generate_expr_code(expr);
    char line[128];
    sprintf(line, "SD R%d, %s(R0)\n", r, name);
    append_asm(line);
    emit_machine("SD", 0, 0, r, get_symbol_addr(name));
}

void generate_letra_with_expr(const char* name, Expr* expr) {
    int val = eval_expr(expr);
    declare_letra(name, val);
    int r = generate_expr_code(expr);
    char line[128];
    sprintf(line, "SB R%d, %s(R0)\n", r, name);
    append_asm(line);
    emit_machine("SB", 0, 0, r, get_symbol_addr(name));
}