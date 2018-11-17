/* Parser Generator for Quirk24 */
/* parser_generator.y */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yyerror(char *s);
int yylex(void);
int yyparse();
%}

%union{
  int		int_val;
  float		fval;
  char*		op_val;
}


%start	input 

%token	<int_val>	INTEGER_LITERAL
%type	<int_val>	exp
%left	PLUS
%left	MULT

%%

input:		/* empty */
		| exp	{ printf("Result: %d\n", $1); }
		;

exp:		INTEGER_LITERAL	{ $$ = $1; }
		| exp PLUS exp	{ $$ = $1 + $3; }
		| exp MULT exp	{ $$ = $1 * $3; }
		;

%%

int yyerror(char *s)
{
  extern int yylineno;	// defined and maintained in lex.c
  extern char *yytext;	// defined and maintained in lex.c
  
  fprintf(stderr, "ERROR: %s at symbol on line %d\n", yytext, yylineno);
  exit(1);
}

int main(int argc, char **argv)
{
  if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
  {
    fprintf(stderr, "argv[0]: File %s cannot be opened.\n", argv[1]) ;
    exit( 1 );
  }
  
  yyparse();

  return 0;
}


