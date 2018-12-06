/* Lexical Analyzer for Quirk24 */
/* lexical_analyzer.l */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parser_generator.tab.h"
int yyerror(char *s);
//int yylineno = 1;
int check_id_typeid(char *id);
int store_typeid(char *s);
int inittypeid();
char* extract_typeid_name(char *s);
char* manage_string(char *s);
char **typeid_array;
int initial_typeid_limit = 1;
int typeid_count = 0;  
%}
%option noyywrap

digit		[0-9]
intliteral	{digit}+
id		[a-z][a-zA-Z0-9_]*
tvar		[A-Z][a-zA-Z0-9_]*
floatliteral    ([0-9]+\.[0-9]*([e][-+]?[0-9]+)?)|([0-9]*\.[0-9]+([e][-+]?[0-9]+)?)
charliteral     \'[ a-zA-Z0-9_~#$%&!()\"\*+\-\,\.\/\\:;<=>?@\[\]^\{\}~]\'
/*stringliteral   \"[ a-zA-Z0-9_~#$%&()\*+\-\,\.\/:;<=>?@\[\]^\{\}~]*\" */
stringliteral   \"[ a-zA-Z0-9_~#$%&!()\n\r\'\*+\-\,\.\/\\:;<=>?@\[\]^\{\}~]*\"
typeid          protocol[ \t]+[a-z][a-zA-Z0-9_]*  

%%

{intliteral}	{ /*printf("intliteral %d\n", atoi(strdup(yytext)));*/ yylval.sval = strdup(yytext); return INTLITERAL; }
{floatliteral}	{ /*printf("floatliteral %f\n", atof(strdup(yytext)));*/ yylval.sval = strdup(yytext); return FLOATLITERAL; } 

{typeid}        { /*fprintf(stderr, "typeid %s\n", strdup(yytext));*/ char *prt = extract_typeid_name(strdup(yytext)); store_typeid(prt); yylval.sval = strdup(prt); return PROTOCOL; }
"void"          { /*printf("%s\n", strdup(yytext));*/ return VOID; }
"bool"|"char"|"string"|"int"|"float"            { /*printf("primtype %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return PRIMTYPE; }
"null"          { /*printf("%s\n", strdup(yytext));*/ return NULLVALUE; }
"true"          { /*printf("%s\n", strdup(yytext));*/ return TRUE; }
"false"         { /*printf("%s\n", strdup(yytext));*/ return FALSE; }

"constant"      { /*printf("%s\n", strdup(yytext));*/ return CONSTANT; }
"static"        { /*printf("%s\n", strdup(yytext));*/ return STATIC; }
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


{id}		{ /*fprintf(stderr, "id %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext);
                        if (check_id_typeid(strdup(yytext)) == 1)
                        {
                                //fprintf(stderr, "TYPEID %s\n", strdup(yytext));
                                return TYPEID;
                        }
                        else{
                               //fprintf(stderr, "ID %s\n", strdup(yytext)); 
                                return ID; 
                        }
                }

{tvar}		{ /*printf("typevar %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return TVAR; }
"->"		{ /*printf("%s\n", strdup(yytext));*/ return ARROW; }
"||"		{ /*printf("orop %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return OROP; }
"&&"		{ /*printf("andop %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return ANDOP; }
"=="            { /*printf("relop %s\n", strdup(yytext));*/ yylval.sval = strdup("="); return RELOP; }
"!="|"<="|">="  { /*printf("relop %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return RELOP; }
"<"		{ /*printf("%s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return LANGLE; }
">"		{ /*printf("%s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return RANGLE; }
"="		{ /*printf("assignop %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return ASSIGNOP; }
"+"		{ /*fprintf(stderr, "addop %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return ADDOP; }
"*"|"/"		{ /*printf("mulop %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return MULOP; }
"!"		{ /*fprintf(stderr, "unop %s[unop]\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return UNOP; }
"-"		{ /*fprintf(stderr, "unop %s[unop]\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return MINUS; }
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
{charliteral}   { /*printf("charliteral %s\n", strdup(yytext));*/ yylval.sval = strdup(yytext); return CHARLITERAL; } 
{stringliteral} { /*printf("stringliteral %s\n", strdup(yytext));*/ char *str = manage_string(strdup(yytext)); yylval.sval = strdup(str); return STRINGLITERAL; } 
[ \t]*		{ }
[\n]		{ yylineno++;	}

.		{ fprintf(stderr, "SCANNER "); /*printf("------UNKNOWN CHARACTER ENCOUNTERED!------\n---%s\n", strdup(yytext));*/ yyerror(""); /*printf("\nLines: %d", yylineno);*/ exit(1); }

%%

/*"this"          { printf("%s\n", strdup(yytext)); return THIS; }*/

char* manage_string(char *s)
{
        int l = strlen(s);
        int str_l = 2*l;
        char *str = (char*) malloc(str_l);
        int j = 0;
        for(int i=0; i<l; i++){
                if (j >= str_l){
                        str_l = str_l*2;
                        str = (char *) realloc (str, str_l);
                }
                if(s[i] == '\\'){
                        str[j++] = '\\';
                        str[j++] = '\\';
                }
                else if(s[i] == '\n'){
                        str[j++] = '\\';
                        str[j++] = 'n';
                        yylineno++;
                }
                else{
                        str[j++] = s[i];
                }
        }
        str[j] = '\0';

        return str;
}

char* extract_typeid_name(char *s)
{
        char *token;
        char *str = strdup(s);
        token = strtok(str, " ");
        token = strtok(NULL, " ");
        
        return token;
}

int inittypeid()
{
        typeid_array = (char **) malloc (sizeof(char *) * initial_typeid_limit);
        return 0;
}

int store_typeid(char *s)
{
        if (typeid_count >= initial_typeid_limit)
        {
                initial_typeid_limit = 2 * initial_typeid_limit;
                typeid_array = (char **) realloc (typeid_array, sizeof(char *) * initial_typeid_limit);
        }

        typeid_array[typeid_count] = strdup(s);
        typeid_count++;
        return 1;
}

int check_id_typeid(char *id)
{
        for (int i = 0; i < typeid_count; i++) 
        {
                if (!strcmp(id, typeid_array[i]))    //see if the id matches with any typeid
                {
                        return 1;   //matches with one of the typeid
                }
        }
        return 0;   //doesn't match with any typeid
} 

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


