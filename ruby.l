%{

  #include <stdio.h>
  #include <string.h>
  #include "parse.tab.h"

  #define YY_DECL 			int yylex(YYSTYPE *yylval, parser_state *state)
  #define token(id) 		{return t##id;}
  #define key(id) 			{return keyword_##id;}

%}

%option noyywrap
%option yylineno

NUMBER [0-9](_[0-9]|[0-9])*

%%

/* comments */
#.*$ {}	

	
/* keywords */
"class"		key(class)
"module"	key(module)
"def"		key(def)
"undef"		key(undef)
"begin"		key(begin)
"rescue"	key(rescue)
"ensure"	key(ensure)
"end"		key(end)
"if"		key(if)
"unless"	key(unless)
"then"		key(then)
"elsif"		key(elsif)
"else"		key(else)
"case"		key(case)
"when"		key(when)
"while"		key(while)
"until"		key(until)
"for"		key(for)
"break"		key(break)
"next"		key(next)
"redo"		key(redo)
"retry"		key(retry)
"in"		key(int)
"do"		key(do)
// accounts for do_cond, do_LAMBDA, do_block 
"return"	key(return)
"yield"		key(yield)
"super"		key(super)
"self"		key(self)
"nil"		key(nil)
"true"		key(true)
"false"		key(false)
"and"		key(and)
"or"		key(or)
"not"		key(not)
"if"		key(if)
"alias"		key(alias)
"defined"	key(defined)
"BEGIN"		key(BEGIN)
"END"		key(END)


/* tokens */

// string
\"([^"]|\\.)*\"		token(STRING_CONTENT)
\'([^']|\\.)*\'		token(STRING_CONTENT)

// data types
{NUMBER}(\.{NUMBER}|(\.{NUMBER})?[eE][+-]?{NUMBER})		token(FLOAT)
{NUMBER}												token(INTEGER)
[a-zA-Z]												token(CHAR)

// var types
[a-z_][a-zA-Z0-9_]*			token(IDENTIFIER)
[A-Z][a-zA-Z0-9_]*			token(CONSTANT)
$[a-zA-Z_][a-zA-Z0-9_]*		token(GVAR)
@[a-zA-Z_][a-zA-Z0-9_]*		token(IVAR)
@@[a-zA-Z_][a-zA-Z0-9_]*	token(CVAR)

// special symbols
"**"	token(POW)
"<=>"	token(CMP)
"=="	token(EQ)
"==="	token(EQQ)
"!="	token(NEQ)
">="	token(GEQ)
"<="	token(LEQ)
"&&"	token(ANDOP)
"||"	token(OROP)
"=~"	token(MATCH)
"!~"	token(NMATCH)
".."	token(DOT2)
"..."	token(DOT3)
"[]"	token(AREF)
"[]="	token(ASET)
">>"	token(RSHFT)
"<<"	token(LSHFT)
"::"	token(COLON2)
[-+*/]=	token(OP_ASGN)
"=>"	token(ASSOC)
"->"	token(LAMBDA)


/* ignore white spaces */
[\t ] {}
\n {}


/*	unknown token */
. { fprintf(stderr, "Unknown token '%s'\n", yytext); }
*/

%%

void yyerror(char const *s) {
  fprintf(stderr, "%s. Unexpected \"%s\" on line %d\n", s, yytext, yylineno);
}
