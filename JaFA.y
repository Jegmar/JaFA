%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "semantics.h"
#include "jafa_compiler.h"
#include "ast.h"   /* Make Expr visible */
#include <ctype.h>


int parse_success = 1; /* 1 = OK, 0 = error */
int current_type = TYPE_INT;
int last_token_is_operator = 0;

extern int yylex();
extern int yylineno;
void yyerror(const char *s);

/* ---------------- Output Buffer ---------------- */
#define OUTPUT_BUFFER_SIZE 65536
char output_buffer[OUTPUT_BUFFER_SIZE];
int buffer_pos = 0;

void append_output(const char *s) {
    int len = strlen(s);
    if(buffer_pos + len >= OUTPUT_BUFFER_SIZE) {
        fprintf(stderr, "Output buffer overflow\n");
        exit(1);
    }
    strcpy(&output_buffer[buffer_pos], s);
    buffer_pos += len;
}

void append_int(int n) {
    char tmp[32];
    sprintf(tmp, "%d", n);
    append_output(tmp);
}

/* external buffers provided by jafa_compiler.c */
extern char data_buffer[];
extern char assembly_buffer[];
extern char machine_buffer[];

/* --- Use eval_expr from jafa_compiler.c --- */
extern int eval_expr(Expr *e);

/* For syntax error mess */
extern char *yytext;

%}

/* THIS MAKES Expr VISIBLE TO %union */
%code requires {
    #include "ast.h"
}

%union {
    int num;
    char ch;
    char *str;
    char *id;
    int op;
    Expr *expr;   /* Expression AST node */
}


%token SUGOD HUMAN END BAD_TOKEN
%token INT_TYPE CHAR_TYPE
%token <id> NAME
%token <num> NUM_VAL
%token <ch> CHAR_VAL
%token <str> STRING_VAL
%token <op> SET ADD_SET SUB_SET MUL_SET DIV_SET
%token ADD SUB MUL DIV
%token L_PAREN R_PAREN COMMA DOT
%token SHOW

%type <expr> value combine factor base
%type <op> mode

%%

source : SUGOD flow HUMAN END
         { /* program successfully parsed */ }

flow : /* empty */
     | flow step
     

step : display DOT { if (parse_success) append_output("\n"); }
     | setup DOT
     | modify DOT
     | compute DOT
     ;


setup    : category batch
category : INT_TYPE  { current_type = TYPE_INT; }
         | CHAR_TYPE { current_type = TYPE_CHAR; }

batch : item
      | item COMMA batch

item : NAME
       { 
           if(!parse_success) YYABORT;

           int idx = find_var($1);
           if(idx == -1) idx = add_var($1, current_type);

           /* Default value = 0 */
           sym_table[idx].value = 0;

           /* Generate .data allocation */
           if(current_type == TYPE_INT) declare_entero($1, 0);
           else if(current_type == TYPE_CHAR) declare_letra($1, 0);

           free($1); 
       }
     | NAME SET value
        {
            if(!parse_success) YYABORT;

            /* Look up variable */
            int idx = find_var($1);

            if(idx == -1) {
                /* variable not declared, declare automatically */
                idx = add_var($1, current_type);
            }

            /* TYPE CHECKING */
            int var_type = sym_table[idx].type;
            ExprType expr_type = $3->expr_type;

            // Pass the expression type to the semantic checker
            check_assignment_type(sym_table[idx].type, $3->expr_type, $1, yylineno);


            /* Recursive type check for arithmetic expressions:
            - Ensure INT variables do not use CHAR variables in any part of the expression */
            if(var_type == TYPE_INT) {
                check_expr_type($3, TYPE_INT);   // Will abort if any CHAR variable is used
            }

            /* Evaluate expression */
            int val = eval_expr($3);
            sym_table[idx].value = val;

            /* Generate assembly */
            if (var_type == TYPE_INT)
                generate_entero_with_expr($1, $3);
            else
                generate_letra_with_expr($1, $3);

            free($1);
            free_expr($3);
        }

;


modify : NAME mode value
         {
             if(!parse_success) YYABORT;
             check_undefined($1, yylineno);
             int idx = find_var($1);
             int new_val = eval_expr($3);  /* evaluate expression */
             switch($2) {
                 case SET:     break; /* already stored */
                 case ADD_SET: new_val += sym_table[idx].value; break;
                 case SUB_SET: new_val = sym_table[idx].value - new_val; break;
                 case MUL_SET: new_val *= sym_table[idx].value; break;
                 case DIV_SET: check_division($3->value, yylineno); new_val = sym_table[idx].value / eval_expr($3); break;
             }
             sym_table[idx].value = new_val;
             free($1);
             free_expr($3);
         }

mode : SET     { $$ = SET; }
     | ADD_SET { $$ = ADD_SET; }
     | SUB_SET { $$ = SUB_SET; }
     | MUL_SET { $$ = MUL_SET; }
     | DIV_SET { $$ = DIV_SET; }

display : SHOW L_PAREN R_PAREN
        | SHOW L_PAREN arg_list R_PAREN
        ;
;

arg_list : arg
         | arg COMMA arg_list
         ;

arg : value
      {
        if(!parse_success) YYABORT;

        int val = eval_expr($1);

        if($1->kind == EX_CONST || $1->kind == EX_ADD || $1->kind == EX_SUB || $1->kind == EX_MUL || $1->kind == EX_DIV) {
            append_int(val);
        } else if($1->kind == EX_VAR) {
            int idx = find_var($1->name);
            if(idx == -1) append_output("0");
            else {
                if(sym_table[idx].type == TYPE_INT) append_int(sym_table[idx].value);
                else {
                    char tmp[2] = { (char)sym_table[idx].value, '\0' };
                    append_output(tmp);
                }
            }
        }

        free_expr($1);
      }
    | STRING_VAL
        { if(parse_success){
              char *s = $1;
              int len = strlen(s);
              if(s[0]=='"' && s[len-1]=='"') s[len-1]='\0';
              append_output(s+1);
          }
          free($1);
        }
    
;

compute : value { free_expr($1); }

value   : combine { $$ = $1; }

combine : factor { $$ = $1; }
        | combine ADD factor { $$ = mk_bin(EX_ADD, $1, $3); }
        | combine SUB factor { $$ = mk_bin(EX_SUB, $1, $3); }

factor  : base { $$ = $1; }
        | factor MUL base { $$ = mk_bin(EX_MUL, $1, $3); }
        | factor DIV base { $$ = mk_bin(EX_DIV, $1, $3); }

base : NUM_VAL
        {
            $$ = mk_const($1);
            $$->expr_type = EXPR_INT;
        }
     | CHAR_VAL    
        {
            $$ = mk_const($1);    
            $$->expr_type = EXPR_CHAR;
        }
     | NAME
         { check_undefined($1, yylineno); $$ = mk_var($1); free($1); }
     | L_PAREN value R_PAREN { $$ = $2; }
     | SUB base { Expr *zero = mk_const(0); $$ = mk_bin(EX_SUB, zero, $2); }

%%

void yyerror(const char *s) {
    extern int yylineno;
    extern char *yytext;

    /* Detect if offending token is an operator */
    if (yytext[0] == '+' || yytext[0] == '-' ||
        yytext[0] == '*' || yytext[0] == '/') 
    {
        last_token_is_operator = 1;
    }

    if (last_token_is_operator) {
        // Most specific: incomplete arithmetic
        fprintf(stderr,
            "Syntax Error at line %d: Incomplete arithmetic expression, operator '%s' has no right-hand operand.\n",
            yylineno, yytext);

        last_token_is_operator = 0;  // reset
        return;
    } 
    else if (isalpha(yytext[0])) {
        // After variable name, unexpected token
        fprintf(stderr,
            "Syntax Error at line %d: Expected assignment operator '=' or DOT after variable declaration, but found '%s'\n",
            yylineno, yytext);
        return;
    } 
    else if (strcmp(yytext, ".") != 0) {
        // DOT missing at end
        fprintf(stderr,
            "Syntax Error at line %d: Missing DOT at the end of the previous statement.\n",
            yylineno);
        return;
    } 
    else {
        // fallback generic syntax error
        fprintf(stderr,
            "Syntax Error at line %d: %s (near '%s')\n",
            yylineno, s, yytext);
        return;
    }
}



int main() {
    parse_success = 1;
    init_buffers();  /* reset assembly, machine, data buffers */
    int result = yyparse();

    if(parse_success && result == 0) {
        printf("%s", output_buffer);
        printf("\nProgram parsed successfully!\n\n");

        printf("Assembly code:\n");
        printf(".data\n%s", data_buffer);
        printf(".code\n%s\n", assembly_buffer);

        printf("Machine code:\n%s\n", machine_buffer);
    } else {
        fprintf(stderr, "Parsing terminated due to error.\n");
        return 1;
    }

    return 0;
}
//orig