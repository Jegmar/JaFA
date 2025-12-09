%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "JaFA.tab.h"
#include "ast.h"
%}

%option noyywrap
%option yylineno

%%

[ \t\r\n]+        ;  /* ignore whitespace */

"SUGOD"         { return SUGOD; }
"HUMAN"         { return HUMAN; }

"entero"        { return INT_TYPE; }
"letra"         { return CHAR_TYPE; }

"printa"        { return SHOW; }

"="             { return SET; }
"addset"        { return ADD_SET; }
"subset"        { return SUB_SET; }
"mulset"        { return MUL_SET; }
"divset"        { return DIV_SET; }

"+"             { return ADD; }
"-"             { return SUB; }
"*"             { return MUL; }
"/"             { return DIV; }

"("             { return L_PAREN; }
")"             { return R_PAREN; }
","             { return COMMA; }
"."             { return DOT; }

\"[^\"]*\"      { yylval.str = strdup(yytext); return STRING_VAL; }
\'[A-Za-z]\'    { yylval.ch = yytext[1]; return CHAR_VAL; }
[0-9]+          { yylval.num = atoi(yytext); return NUM_VAL; }

[A-Za-z][A-Za-z0-9]* { yylval.id = strdup(yytext); return NAME; }

"//".*          ;  /* ignore comments */

.               { printf("\nBad token at line %d: %s\n", yylineno, yytext); return BAD_TOKEN; }

%%
//orig