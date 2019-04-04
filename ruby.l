%{

  #include <stdio.h>
  #include <string.h>
  #include "parse.tab.h"

  #define token(id) 		{return t##id;}
  #define key(id) 			{return keyword_##id;}

%}

%option noyywrap
%option yylineno

NUMBER [0-9](_[0-9]|[0-9])*

%%

#.*$ {}	


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
"in"		key(in)
"do"		key(do)

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
"alias"		key(alias)
"defined"	key(defined)
"BEGIN"		key(BEGIN)
"END"		key(END)


\"([^"]|\\.)*\"		token(STRING_CONTENT)
\'([^']|\\.)*\'		token(STRING_CONTENT)

[a-z_][a-zA-Z0-9_]*			token(IDENTIFIER)
[A-Z][a-zA-Z0-9_]*			token(CONSTANT)
$[a-zA-Z_][a-zA-Z0-9_]*		token(GVAR)
@[a-zA-Z_][a-zA-Z0-9_]*		token(IVAR)
@@[a-zA-Z_][a-zA-Z0-9_]*	token(CVAR)

{NUMBER}(\.{NUMBER}|(\.{NUMBER})?[eE][+-]?{NUMBER})		token(FLOAT)
{NUMBER}												token(INTEGER)

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

[\t ] {}

.|\n {}
%%
