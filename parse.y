%{
  #include <stdio.h>
  int yylex();
  int yyerror();
  extern FILE* yyin;
  extern int yylineno;
  extern char* yytext;
%}

%token do end undef alias if while unless until
%token return yield and or not super self
%token defined? elsif else end case in nil 
%token when rescue ensure class module then def
%token OP_ASSGN SYMBOL FNAME OPERATION VARNAME GLOBAL 
%token STRING2 HERE_DOC REGEXP IDENTIFIER

%start PROGRAM

%%

PROGRAM		 	: COMPSTMT {printf("Success! Parsing complete.\n")}

COMPSTMT	 	: STMT (TERM EXPR)* [TERM]

STMT		   	: CALL do [`|' [BLOCK_VAR] `|'] COMPSTMT end
           		| undef FNAME
		       	| alias FNAME FNAME
		       	| STMT if EXPR
		       	| STMT while EXPR
		       	| STMT unless EXPR
		       	| STMT until EXPR
           		| `BEGIN' `{' COMPSTMT `}'
           		| `END' `{' COMPSTMT `}'
           		| LHS `=' COMMAND [do [`|' [BLOCK_VAR] `|'] COMPSTMT end]
		       	| EXPR

EXPR		   	: MLHS `=' MRHS
		       	| return CALL_ARGS
		       	| yield CALL_ARGS
		       	| EXPR and EXPR
		       	| EXPR or EXPR
		       	| not EXPR
		       	| COMMAND
		       	| `!' COMMAND
		       	| ARG

CALL		   	: FUNCTION
           		| COMMAND

COMMAND		 	: OPERATION CALL_ARGS
		       	| PRIMARY `.' OPERATION CALL_ARGS
		       	| PRIMARY `::' OPERATION CALL_ARGS
		       	| super CALL_ARGS

FUNCTION   		: OPERATION [`(' [CALL_ARGS] `)']
		       	| PRIMARY `.' OPERATION `(' [CALL_ARGS] `)'
		       	| PRIMARY `::' OPERATION `(' [CALL_ARGS] `)'
		       	| PRIMARY `.' OPERATION
		       	| PRIMARY `::' OPERATION
		       	| super `(' [CALL_ARGS] `)'
		       	| super

ARG		     	: LHS `=' ARG
		       	| LHS OP_ASGN ARG
		       	| ARG `..' ARG
		       	| ARG `...' ARG
		       	| ARG `+' ARG
		       	| ARG `-' ARG
		       	| ARG `*' ARG
		       	| ARG `/' ARG
		       	| ARG `%' ARG
		       	| ARG `**' ARG
		       	| `+' ARG
		       	| `-' ARG
		       	| ARG `|' ARG
		       	| ARG `^' ARG
		       	| ARG `&' ARG
		       	| ARG `<=>' ARG
		       	| ARG `>' ARG
		       	| ARG `>=' ARG
		       	| ARG `<' ARG
		       	| ARG `<=' ARG
		       	| ARG `==' ARG
		       	| ARG `===' ARG
		       	| ARG `!=' ARG
		       	| ARG `=~' ARG
		       	| ARG `!~' ARG
		       	| `!' ARG
		       	| `~' ARG
		       	| ARG `<<' ARG
		       	| ARG `>>' ARG
		       	| ARG `&&' ARG
		       	| ARG `||' ARG
		       	| defined? ARG
		       	| PRIMARY

PRIMARY		 	: `(' COMPSTMT `)'
		       	| LITERAL
		       	| VARIABLE
		       	| PRIMARY `::' IDENTIFIER
		       	| `::' IDENTIFIER
		       	| PRIMARY `[' [ARGS] `]'
		       	| `[' [ARGS [`,']] `]'
		       	| `{' [(ARGS|ASSOCS) [`,']] `}'
		       	| return [`(' [CALL_ARGS] `)']
		       	| yield [`(' [CALL_ARGS] `)']
		       	| defined? `(' ARG `)'
           		| FUNCTION
		       	| FUNCTION `{' [`|' [BLOCK_VAR] `|'] COMPSTMT `}'
		       	| if EXPR THEN
		       	COMPSTMT
		       	(elsif EXPR THEN COMPSTMT)*
		       	[else COMPSTMT]
		         end
		       	| unless EXPR THEN
		         COMPSTMT
		         [else COMPSTMT]
		         end
		       	| while EXPR DO COMPSTMT end
		       	| until EXPR DO COMPSTMT end
		       	| case COMPSTMT
		         (when WHEN_ARGS THEN COMPSTMT)+
		         [else COMPSTMT]
		         end
		       	| for BLOCK_VAR in EXPR DO
		         COMPSTMT
		         end
		       	| begin
		         COMPSTMT
		         [rescue [ARGS] DO COMPSTMT]+
		         [else COMPSTMT]
		         [ensure COMPSTMT]
		         end
		       	| class IDENTIFIER [`<' IDENTIFIER]
		         COMPSTMT
		         end
		       	| module IDENTIFIER
		         COMPSTMT
		         end
		       	| def FNAME ARGDECL
		         COMPSTMT
		         end
				| def SINGLETON (`.'|`::') FNAME ARGDECL
		          COMPSTMT
		          end

WHEN_ARGS		: ARGS [`,' `*' ARG]
				| `*' ARG

THEN			: TERM
				| then
				| TERM then

DO				: TERM
				| do
				| TERM do

BLOCK_VAR	 	: LHS
				| MLHS

MLHS		   	: MLHS_ITEM `,' [MLHS_ITEM (`,' MLHS_ITEM)*] [`*' [LHS]]
				| `*' LHS

MLHS_ITEM	 	: LHS
				| '(' MLHS ')'

LHS			   	: VARIABLE
				| PRIMARY `[' [ARGS] `]'
				| PRIMARY `.' IDENTIFIER

MRHS		   	: ARGS [`,' `*' ARG]
		       	| `*' ARG

CALL_ARGS	 	: ARGS
				| ARGS [`,' ASSOCS] [`,' `*' ARG] [`,' `&' ARG]
		     	| ASSOCS [`,' `*' ARG] [`,' `&' ARG]
		     	| `*' ARG [`,' `&' ARG]
		     	| `&' ARG
				| COMMAND

ARGS 		 	: ARG (`,' ARG)*

ARGDECL		 	: `(' ARGLIST `)'
				| ARGLIST TERM

ARGLIST		 	: IDENTIFIER(`,'IDENTIFIER)*[`,'`*'[IDENTIFIER]][`,'`&'IDENTIFIER]
				| `*'IDENTIFIER[`,'`&'IDENTIFIER]
				| [`&'IDENTIFIER]

SINGLETON	 	: VARIABLE
				| `(' EXPR `)'

ASSOCS			: ASSOC (`,' ASSOC)*

ASSOC		    : ARG `=>' ARG

VARIABLE	 	: VARNAME
				| nil
		       	| self

LITERAL		 	: numeric
		       	| SYMBOL
		       	| STRING
		       	| STRING2
		       	| HERE_DOC
		       	| REGEXP

TERM		   	: `;'
		       	| `\n'

%%

int yyerror(const char *s) {
  printf("%s);
  return 1;
}