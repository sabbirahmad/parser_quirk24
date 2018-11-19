/* Lexical Analyzer for Quirk24 */
/* lexical_analyzer.l */

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
intliteral	{digit}+
id		[a-z][a-zA-Z0-9_]*
tvar		[A-Z][a-zA-Z0-9_]*
floatliteral	[0-9]*\.?[0-9]+([e][-+]?[0-9]+)?
charliteral     \'[ a-zA-Z0-9_~#$%&()\*+\-\,\.\/:;<=>?@\[\]^\{\}~]\'
stringliteral   \"[ a-zA-Z0-9_~#$%&()\*+\-\,\.\/:;<=>?@\[\]^\{\}~]*\"

%%

"void"          { /*printf("%s\n", yytext);*/ return VOID; }
"bool"|"char"|"string"|"int"|"float"            { /*printf("primtype %s\n", yytext);*/ yylval.ival = atoi(yytext); return PRIMTYPE; }
"null"          { /*printf("%s\n", yytext);*/ return NULLVALUE; }
"true"          { /*printf("%s\n", yytext);*/ return TRUE; }
"false"         { /*printf("%s\n", yytext);*/ return FALSE; }

"function"      { /*printf("%s\n", yytext);*/ return FUNCTION; }
"protocol"      { /*printf("%s\n", yytext);*/ return PROTOCOL; }
"extends"       { /*printf("%s\n", yytext);*/ return EXTENDS; }
"implements"    { /*printf("%s\n", yytext);*/ return IMPLEMENTS; }
"fun"           { /*printf("%s\n", yytext);*/ return FUN; }
"class"         { /*printf("%s\n", yytext);*/ return CLASS; }
"return"        { /*printf("%s\n", yytext);*/ return RETURN; }
"if"            { /*printf("%s\n", yytext);*/ return IF; }
"else if"       { /*printf("%s\n", yytext);*/ return ELSEIF; }
"else"          { /*printf("%s\n", yytext);*/ return ELSE; }
"while"         { /*printf("%s\n", yytext);*/ return WHILE; }
"for"           { /*printf("%s\n", yytext);*/ return FOR; }
"halt"          { /*printf("%s\n", yytext);*/ return HALT; }
"new"           { /*printf("%s\n", yytext);*/ return NEW; }
"lambda"        { /*printf("%s\n", yytext);*/ return LAMBDA; }
"this"          { /*printf("%s\n", yytext);*/ return THIS; }

{intliteral}	{ /*printf("intliteral %d\n", atoi(yytext));*/ yylval.ival = atoi(yytext); return INTLITERAL; }
{floatliteral}	{ /*printf("floatliteral %f\n", atof(yytext));*/ yylval.fval = atof(yytext); return FLOATLITERAL; }
{id}		{ /*printf("id %s\n", yytext);*/ yylval.sval = yytext; return ID; }
{tvar}		{ /*printf("typevar %s\n", yytext);*/ yylval.sval = yytext; return TYPEVAR; }
"="		{ /*printf("assignop %s\n", yytext);*/ yylval.sval= yytext; return ASSIGNOP; }
"||"		{ /*printf("orop %s\n", yytext);*/ yylval.sval= yytext; return OROP; }
"&&"		{ /*printf("andop %s\n", yytext);*/ yylval.sval= yytext; return ANDOP; }
"=="|"!="|"<"|"<="|">"|">="		{ /*printf("relop %s\n", yytext);*/ yylval.sval= yytext; return RELOP; }
"+"|"-"		{ /*printf("addop %s\n", yytext);*/ yylval.sval= yytext; return ADDOP; }
"*"|"/"		{ /*printf("mulop %s\n", yytext);*/ yylval.sval= yytext; return MULOP; }
"!"|"-"		{ /*printf("unop %s[unop]\n", yytext);*/ yylval.sval= yytext; return UNOP; }
";"		{ /*printf("semicolon %s at Line %d\n", yytext, yylineno);*/ return SEMICOLON; }
":"		{ /*printf("colon %s\n", yytext);*/ return COLON; }
","		{ /*printf("comma %s\n", yytext);*/ return COMMA; }
"("		{ /*printf("%s\n", yytext);*/ return LPAR; }
")"		{ /*printf("%s\n", yytext);*/ return RPAR; }
"{"		{ /*printf("%s\n", yytext);*/ return LBRACE; }
"}"		{ /*printf("%s\n", yytext);*/ return RBRACE; }
"["		{ /*printf("%s\n", yytext);*/ return LBRACKET; }
"]"		{ /*printf("%s\n", yytext);*/ return RBRACKET; }
"<"		{ /*printf("%s\n", yytext);*/ return LANGLE; }
">"		{ /*printf("%s\n", yytext);*/ return RANGLE; }
"->"		{ /*printf("%s\n", yytext);*/ return ARROW; }
\.		{ /*printf("%s\n", yytext);*/ return DOT; }
\/\/.*		{ /*printf("COMMENT %s\n", yytext);*/ }
{charliteral}   { /*printf("charliteral %s\n", yytext); yylval.sval= yytext;*/ return CHARLITERAL; } 
{stringliteral} { /*printf("stringliteral %s\n", yytext); yylval.sval= yytext;*/ return STRINGLITERAL; } 
[ \t]*		{}
[\n]		{ yylineno++;	}

.		{ fprintf(stderr, "SCANNER "); /*printf("------UNKNOWN CHARACTER ENCOUNTERED!------\n---%s\n", yytext);*/ yyerror(""); /*printf("\nLines: %d", yylineno);*/ exit(1); }

%%


// to check if lexical analyzer works fine
/*int main(int argc, char **argv)
{
  if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
  {
    fprintf(stderr, "argv[0]: File %s cannot be opened.\n", argv[1]) ;
    exit( 1 );
  }
  while(yylex());
  printf("\nLines: %d", yylineno);

  return 0;
}*/


