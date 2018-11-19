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
	int	ival;
	float	fval;
	char*	sval;
}


%start	program 

%token SEMICOLON COMMA COLON LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET LANGLE RANGLE ARROW DOT VOID NULLVALUE TRUE FALSE FUNCTION PROTOCOL EXTENDS IMPLEMENTS FUN CLASS RETURN IF ELSEIF ELSE WHILE FOR HALT NEW LAMBDA THIS

%token	<ival>	INTLITERAL
%token	<fval>	FLOATLITERAL
%token	<sval>	ID TYPEVAR ASSIGNOP OROP ANDOP RELOP ADDOP MULOP UNOP PRIMTYPE CHARLITERAL STRINGLITERAL

%left	PLUS
%left	MULT

%%

program:	{printf("(illegal)")}
                ;

%%

/*%type	<ival>	exp*/

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


