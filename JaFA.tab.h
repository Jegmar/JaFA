
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* "%code requires" blocks.  */

/* Line 1676 of yacc.c  */
#line 48 "JaFA.y"

    #include "ast.h"



/* Line 1676 of yacc.c  */
#line 46 "JaFA.tab.h"

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     SUGOD = 258,
     HUMAN = 259,
     END = 260,
     BAD_TOKEN = 261,
     INT_TYPE = 262,
     CHAR_TYPE = 263,
     NAME = 264,
     NUM_VAL = 265,
     CHAR_VAL = 266,
     STRING_VAL = 267,
     SET = 268,
     ADD_SET = 269,
     SUB_SET = 270,
     MUL_SET = 271,
     DIV_SET = 272,
     ADD = 273,
     SUB = 274,
     MUL = 275,
     DIV = 276,
     L_PAREN = 277,
     R_PAREN = 278,
     COMMA = 279,
     DOT = 280,
     SHOW = 281
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 52 "JaFA.y"

    int num;
    char ch;
    char *str;
    char *id;
    int op;
    Expr *expr;   /* Expression AST node */



/* Line 1676 of yacc.c  */
#line 100 "JaFA.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


