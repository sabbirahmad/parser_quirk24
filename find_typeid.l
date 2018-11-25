/* Lexical Analyzer for Quirk24 */
/* lexical_analyzer.l */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

%}
%option noyywrap


typeid          protocol[ \t]+[a-z][a-zA-Z0-9_]*

%%

{typeid}        { fprintf(stderr, "%s\n", strdup(yytext)); }

[\n]		{ yylineno++;	}

.               {}


%%



// to check if lexical analyzer works fine
int main(int argc, char **argv)
{
  if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
  {
    fprintf(stderr, "argv[0]: File %s cannot be opened.\n", argv[1]) ;
    exit( 1 );
  }
  while(yylex());
  printf("\nLines: %d\n", yylineno);

  return 0;
}
