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

"void"          { /*printf("%s\n", strdup(yytext));*/ return VOID; }
"bool"|"char"|"string"|"int"|"float"            { /*printf("primtype %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return PRIMTYPE; }
"null"          { /*printf("%s\n", strdup(yytext));*/ return NULLVALUE; }
"true"          { /*printf("%s\n", strdup(yytext));*/ return TRUE; }
"false"         { /*printf("%s\n", strdup(yytext));*/ return FALSE; }

"function"      { /*printf("%s\n", strdup(yytext));*/ return FUNCTION; }
"protocol"      { /*printf("%s\n", strdup(yytext));*/ return PROTOCOL; }
"extends"       { /*printf("%s\n", strdup(yytext));*/ return EXTENDS; }
"implements"    { /*printf("%s\n", strdup(yytext));*/ return IMPLEMENTS; }
"fun"           { /*printf("%s\n", strdup(yytext));*/ return FUN; }
"class"         { /*printf("%s\n", strdup(yytext));*/ return CLASS; }
"return"        { /*printf("%s\n", strdup(yytext));*/ return RETURN; }
"if"            { /*printf("%s\n", strdup(yytext));*/ return IF; }
"else"          { /*printf("%s\n", strdup(yytext));*/ return ELSE; }
"while"         { /*printf("%s\n", strdup(yytext));*/ return WHILE; }
"for"           { /*printf("%s\n", strdup(yytext));*/ return FOR; }
"halt"          { /*printf("%s\n", strdup(yytext));*/ return HALT; }
"new"           { /*printf("%s\n", strdup(yytext));*/ return NEW; }
"lambda"        { /*printf("%s\n", strdup(yytext));*/ return LAMBDA; }
"this"          { /*printf("%s\n", strdup(yytext));*/ return THIS; }

{intliteral}	{ /*printf("intliteral %d\n", atoi(strdup(yytext)));*/ yylval.sval = strdup(yytext); return INTLITERAL; }
{floatliteral}	{ /*printf("floatliteral %f\n", atof(strdup(yytext)));*/ yylval.sval = strdup(yytext); return FLOATLITERAL; }
{id}		{ /*fprintf(stderr, "id %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return ID; }
{tvar}		{ /*printf("typevar %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return TVAR; }
"->"		{ /*printf("%s\n", strdup(yytext));*/ return ARROW; }
"||"		{ /*printf("orop %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return OROP; }
"&&"		{ /*printf("andop %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return ANDOP; }
"=="|"!="|"<="|">="             { /*printf("relop %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return RELOP; }
"<"		{ /*printf("%s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return LANGLE; }
">"		{ /*printf("%s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return RANGLE; }
"="		{ /*printf("assignop %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return ASSIGNOP; }
"+"		{ /*fprintf(stderr, "addop %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return ADDOP; }
"*"|"/"		{ /*printf("mulop %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return MULOP; }
"!"		{ /*fprintf(stderr, "unop %s[unop]\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return UNOP; }
"-"		{ /*fprintf(stderr, "unop %s[unop]\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return MINUS; }
";"		{ /*printf("semicolon %s at Line %d\n", strdup(yytext), yylineno);*/ return SEMICOLON; }
":"		{ /*printf("colon %s\n", strdup(yytext));*/ return COLON; }
","		{ /*printf("comma %s\n", strdup(yytext));*/ return COMMA; }
"("		{ /*printf("%s\n", strdup(yytext));*/ return LPAR; }
")"		{ /*printf("%s\n", strdup(yytext));*/ return RPAR; }
"{"		{ /*printf("%s\n", strdup(yytext));*/ return LBRACE; }
"}"		{ /*printf("%s\n", strdup(yytext));*/ return RBRACE; }
"["		{ /*printf("%s\n", strdup(yytext));*/ return LBRACKET; }
"]"		{ /*printf("%s\n", strdup(yytext));*/ return RBRACKET; }
\.		{ /*printf("%s\n", strdup(yytext));*/ return DOT; }
\/\/.*		{ /*printf("COMMENT %s\n", strdup(yytext));*/ }
{charliteral}   { /*printf("charliteral %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return CHARLITERAL; } 
{stringliteral} { /*printf("stringliteral %s\n", strdup(yytext));*/ yylval.sval= strdup(yytext); return STRINGLITERAL; } 
[ \t]*		{}
[\n]		{ yylineno++;	}

.		{ fprintf(stderr, "SCANNER "); /*printf("------UNKNOWN CHARACTER ENCOUNTERED!------\n---%s\n", strdup(yytext));*/ yyerror(""); /*printf("\nLines: %d", yylineno);*/ exit(1); }

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


