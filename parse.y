
%{
    #include <stdio.h>
    extern int yycolumn;
    extern int yylex();
    extern FILE *yyin;
    extern int yylineno;
    int yyerror(char const *s);
%}


/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token TIDENTIFIER TINTEGER TDOUBLE TSTR TBOOL
%token TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL TLTLT
%token TCOMMA TDOT TCOLON TRANGE
%token TLPAREN TRPAREN TLBRACKET TRBRACKET
%token TPLUS TMINUS TMUL TDIV
%token TNOT TAND TOR
%token TIF TELSE TWHILE TTO 
%token TSQUOTE TDEF TRETURN TRETURN_SIMPLE TVAR IS
%token INDENT UNINDENT 

/* Operator precedence for mathematical operators */
%left TPLUS TMINUS
%left TMUL TDIV
%left TAND TNOT

%start program
%debug 
%verbose 
%locations /* track locations: @n of component N; @$ of entire range */

%%

program : /* blank */ 
        | stmts
        ;

stmts : stmt 
      | stmts stmt 
      ;

stmt : var_decl
     | var_decl_deduce
     | func_decl
     | class_decl
     | conditional 
     | return
     | while
     | array_add_element
     | expr 
     ;


block : INDENT stmts UNINDENT 
      | INDENT UNINDENT
      ;

conditional : TIF expr block TELSE block
            | TIF expr block 
            ; 

while : TWHILE expr block TELSE block 
      | TWHILE expr block 
      ; 

var_decl : ident ident 
         | ident ident TEQUAL expr 
         ;

var_decl_deduce : TVAR ident TEQUAL expr 
         ;

func_decl : TDEF ident TLPAREN func_decl_args TRPAREN TCOLON ident block 
          | TDEF ident TLPAREN func_decl_args TRPAREN block 
          ;

func_decl_args : /*blank*/ 
          | var_decl 
          | func_decl_args TCOMMA var_decl 
          ;

class_decl: TDEF ident block 
          ;

ident : TIDENTIFIER 
      | TIDENTIFIER TDOT TIDENTIFIER 
      ;

literals : TINTEGER 
         | TDOUBLE 
         | TSTR 
         | TBOOL 
         ;


return : TRETURN expr 
       | TRETURN_SIMPLE 
       ;

expr : ident TEQUAL expr 
     | ident TLPAREN call_args TRPAREN 
     | ident 
     | literals
     | boolean_expr 
     | binop_expr
     | unaryop_expr
     | TLPAREN expr TRPAREN 
     | range_expr
     | array_expr
     | array_access
     ;
/* have to write it explicitly to have the right operator precedence */
binop_expr : expr TAND expr 
           | expr TOR expr 
           | expr TPLUS expr 
           | expr TMINUS expr 
           | expr TMUL expr 
           | expr TDIV expr 
           ;

unaryop_expr : TNOT expr 
             ;

boolean_expr : expr comparison expr 
             ;

call_args : /*blank*/  
          | expr 
          | call_args TCOMMA expr  
          ;
 
comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE
           ;
          
array_elemets_expr: /* blank */
                 | expr 
                 | array_elemets_expr TCOMMA expr 
                 ; 
                 
array_expr : TLBRACKET array_elemets_expr TRBRACKET 
          ;
          
array_add_element: ident TLTLT expr 
                ;
                
array_access: ident TLBRACKET TINTEGER TRBRACKET 
           | array_access TLBRACKET TINTEGER TRBRACKET 
           
range_expr : TLBRACKET expr TRANGE expr TRBRACKET 
            ;

%%

int main(int argc,char* argv[])
{
  if(argc>1) {
    yyin = fopen(argv[1],"r");
  } else {
    printf("Enter the Expression\n");
  }
  printf("hELLO");
  do
  {
    if(yyparse())
    {
      printf("\n Failure\n");
      exit(0);
    }
  } while(!feof(yyin));
  printf("\nSuccess\n");
  return 0;
}