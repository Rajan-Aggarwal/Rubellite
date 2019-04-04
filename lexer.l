%{
#include <stdio.h>
#include <stdlib.h>
#include "parser.tab.h"

int yycolumn = 1;

#define YYERROR_VERBOSE 1
int yyerror(char const * s )
{
    printf("ERROR! \nMessage: %s\n Line number: %d\n Statement: %s\n", s, yylineno, yytext);
    return 1;
}

%}

%option yylineno

%x indent 
%s normal 
%x str qstr
%x comment
%x incl

dig     [0-9]
num1    [-+]?{dig}+\.([eE][-+]?{dig}+)?
num2    [-+]?{dig}*\.{dig}+([eE][-+]?{dig}+)?
number  {num1}|{num2}

%%

\r                      /* cr are ignored */
"if"                    return TIF;
"else"                  return TELSE;
"return"[ \t]*[\n]      return TRETURN_SIMPLE;
"return"                return TRETURN;
"not"                   return TNOT;
"and"                   return TAND;
"or"                    return TOR;
"def"                   return TDEF;
"var"                   return TVAR;
"while"                 return TWHILE;
"is"                    return IS;
"true"                  return TBOOL;
"false"                 return TBOOL;
"to"                    return TTO;
#.*                     /* comments one line til nl */
[ \t\n]                 /* ignore */;
[a-zA-Z_][a-zA-Z0-9_&%\$\?\-]*  return TIDENTIFIER;
-?[0-9]+                return TINTEGER;
{number}                return TDOUBLE;
"->"                    return TRANGE;
"::"                    return TRANGE;
"="                     return TEQUAL;
"=="                    return TCEQ;
"!="                    return TCNE;
"<<"                    return TLTLT;
"<"                     return TCLT;
"<="                    return TCLE;
">"                     return TCGT;
">="                    return TCGE;
"("                     return TLPAREN;
")"                     return TRPAREN;
"."                     return TDOT;
","                     return TCOMMA;
"+"                     return TPLUS;
"-"                     return TMINUS;
"*"                     return TMUL;
"/"                     return TDIV;
":"                     return TCOLON;
"["                     return TLBRACKET;
"]"                     return TRBRACKET;
.|\n                    {}

%%

int yywrap() { 
    return 1; 
}
