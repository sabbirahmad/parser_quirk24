/* Lexical Analyer for Quirk24 */
/* lexical_analyser.l */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parser_generator.tab.h"
int yyerror(char *s);
//int yylineno = 1;
%}
%option noyywrap

digit		[0-9]
int_const	{digit}+

%%

{int_const}	{ yylval.int_val = atoi(yytext); return INTEGER_LITERAL; }
"+"		{ yylval.op_val = yytext; return PLUS; }
"*"		{ yylval.op_val = yytext; return MULT; }

[ \t]*		{}
[\n]		{ yylineno++;	}

.		{ fprintf(stderr, "SCANNER "); yyerror(""); exit(1);	}

