# JaFA
Custom Programmning Language

Run in terminal:

flex JaFA.l
bison -d JaFA.y
gcc lex.yy.c JaFA.tab.c semantics.c ast.c jafa_compiler.c -o jafa
gc JaFA.txt | .\jafa.exe