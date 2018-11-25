/* Parser Generator for Quirk24 */
/* parser_generator.y */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yyerror(char *s);
int yylex(void);
int yyparse();
int inittypeid();
char * ccstr(char *s1, const char *s2);  // concat strings s2 to end of s1 and return s1
char * initstr(char *s);
char * extchar(char *s);

int call_no = 1;



%}

%union{
	char*	sval;
}


%start	program 

%token SEMICOLON COMMA COLON LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET ARROW DOT VOID NULLVALUE TRUE FALSE FUNCTION PROTOCOL EXTENDS IMPLEMENTS FUN CLASS RETURN IF ELSE WHILE FOR HALT NEW LAMBDA THIS

%token	<sval>  INTLITERAL FLOATLITERAL ID TVAR ASSIGNOP OROP ANDOP RELOP ADDOP MULOP UNOP PRIMTYPE CHARLITERAL STRINGLITERAL MINUS LANGLE RANGLE TYPEID

%type   <sval>  program protodecs classdecs stm stms exp lhs disjunct conjunct simple term factor factorrest literal block localdecs localdec vardec type types formal formals fundec fundecs typevars tvars rtype typesrest formalsrest typeapp typeapps typeappsrest

%left	ADDOP
%left	MULOP
%left   RELOP
%left   UNOP

%%

program:
        protodecs classdecs stm         { fprintf(stderr, "%d: program\n", call_no++); char *prg; prg = initstr(prg); prg = ccstr(prg, "(program "); prg = ccstr(prg, $1); prg = ccstr(prg, " "); prg = ccstr(prg, $2); prg = ccstr(prg, " "); prg = ccstr(prg, $3); $$ = strdup(ccstr(prg, ")")); /*free(prg);*/ puts($$); }
       |                                { printf("(illegal)"); }
       ;
protodecs:
                                        { fprintf(stderr, "%d: protodecs\n", call_no++); $$ = "()"; }
       ;
classdecs:
                                        { fprintf(stderr, "%d: classdecs\n", call_no++); $$ = "()"; }
       ;
typevars:
                                        { fprintf(stderr, "%d: typevars1\n", call_no++); $$ = ""; }
       |LANGLE tvars RANGLE             { fprintf(stderr, "%d: typevars2\n", call_no++); char *tvr; tvr = initstr(tvr); $$ = strdup(ccstr(tvr, $2));}
       ;
typeapp:
        TYPEID                          { fprintf(stderr, "%d: typeapp1\n", call_no++); char *tap; tap = initstr(tap); tap = ccstr(tap, "(typeApp "); tap = ccstr(tap, $1); tap = ccstr(tap, " ( "); $$ = strdup(ccstr(tap, " ))")); }
       |TYPEID LANGLE types RANGLE      { fprintf(stderr, "%d: typeapp2\n", call_no++); char *tap; tap = initstr(tap); tap = ccstr(tap, "(typeApp "); tap = ccstr(tap, $1); tap = ccstr(tap, " ( "); tap = ccstr(tap, $3); $$ = strdup(ccstr(tap, " ))")); }
       |TVAR                            { fprintf(stderr, "%d: typeapp3\n", call_no++); char *tap; tap = initstr(tap); tap = ccstr(tap, "(tVar "); tap = ccstr(tap, $1); $$ = strdup(ccstr(tap, ")")); }
       ;
formal:
        type ID                         { fprintf(stderr, "%d: formal\n", call_no++); char *fml; fml = initstr(fml); fml = ccstr(fml, "(formal "); fml = ccstr(fml, $1); fml = ccstr(fml, " "); fml = ccstr(fml, $2); $$ = strdup(ccstr(fml, ")")); }
       ;
fundec:
        FUN ID typevars LPAR formals RPAR block         { fprintf(stderr, "%d: fundec1\n", call_no++); char *fnd; fnd = initstr(fnd); fnd = ccstr(fnd, "(funDec "); fnd = ccstr(fnd, $2); fnd = ccstr(fnd, "("); fnd = ccstr(fnd, $3); fnd = ccstr(fnd, ") ("); fnd = ccstr(fnd, $5); fnd = ccstr(fnd, ") "); fnd = ccstr(fnd, "(void) "); fnd = ccstr(fnd, $7); $$ = strdup(ccstr(fnd, ")")); }
       |FUN ID typevars LPAR formals RPAR COLON rtype block   { fprintf(stderr, "%d: fundec2\n", call_no++); char *fnd; fnd = initstr(fnd); fnd = ccstr(fnd, "(funDec "); fnd = ccstr(fnd, $2); fnd = ccstr(fnd, "("); fnd = ccstr(fnd, $3); fnd = ccstr(fnd, ") ("); fnd = ccstr(fnd, $5); fnd = ccstr(fnd, ") "); fnd = ccstr(fnd, $8); fnd = ccstr(fnd, " "); fnd = ccstr(fnd, $9); $$ = strdup(ccstr(fnd, ")")); }
       ;
block:
        LBRACE localdecs stms RBRACE    { fprintf(stderr, "%d: block\n", call_no++); char *blk; blk = initstr(blk); blk = ccstr(blk, "(block ("); blk = ccstr(blk, $2); blk = ccstr(blk, ") ("); blk = ccstr(blk, $3); $$ = strdup(ccstr(blk, "))")); }
       ;
localdec:
        vardec                          { fprintf(stderr, "%d: localdec1\n", call_no++); $$ = strdup($1); }
       |fundec                          { fprintf(stderr, "%d: localdec2\n", call_no++); $$ = strdup($1); }
       ;
vardec:
        type ID ASSIGNOP exp SEMICOLON  { fprintf(stderr, "%d: vardec\n", call_no++); char *vrd; vrd = initstr(vrd); vrd = ccstr(vrd, "(varDec "); vrd = ccstr(vrd, $1); vrd = ccstr(vrd, " "); vrd = ccstr(vrd, $2); vrd = ccstr(vrd, " "); vrd = ccstr(vrd, $4); $$ = strdup(ccstr(vrd, ")")); }
       ;
type:
        PRIMTYPE                        { fprintf(stderr, "%d: type1\n", call_no++); char *typ; typ = initstr(typ); typ = ccstr(typ, "("); typ = ccstr(typ, $1); $$ = strdup(ccstr(typ, ")")); }
       |typeapp                         { fprintf(stderr, "%d: type2\n", call_no++); $$ = strdup($1); }
       |type LBRACKET RBRACKET          { fprintf(stderr, "%d: type3\n", call_no++); char *typ; typ = initstr(typ); typ = ccstr(typ, "(arrayOf "); typ = ccstr(typ, $1); $$ = strdup(ccstr(typ, ")")); }
       |FUNCTION LPAR LPAR types RPAR ARROW rtype RPAR          { fprintf(stderr, "%d: type4\n", call_no++); char *typ; typ = initstr(typ); typ = ccstr(typ, "(funType ("); typ = ccstr(typ, $4); typ = ccstr(typ, ") "); typ = ccstr(typ, $7); $$ = strdup(ccstr(typ, ")")); }
       ;
rtype:
        type                            { fprintf(stderr, "%d: rtype1\n", call_no++); $$ = strdup($1); }
       |VOID                            { fprintf(stderr, "%d: rtype2\n", call_no++); $$ = "(void)"; }
       ;
stm:    
        SEMICOLON                       { fprintf(stderr, "%d: stm1\n", call_no++); $$ = "(skip)";  /*fprintf(stderr, "-------SEMICOLON--------\n")*/ }
       |exp SEMICOLON                   { fprintf(stderr, "%d: stm2\n", call_no++); char *st; st = initstr(st); st = ccstr(st, "(expStm "); st = ccstr(st, $1); $$ = strdup(ccstr(st, ")")); /*fprintf(stderr, "STMEXP: %s----------\n", $$);*/ /*free(st);*/ }
       |IF LPAR exp RPAR stm ELSE stm   { fprintf(stderr, "%d: stm3\n", call_no++); char *st; st = initstr(st); st = ccstr(st, "(if "); st = ccstr(st, $3); st = ccstr(st, " "); st = ccstr(st, $5); st = ccstr(st, " "); st = ccstr(st, $7); $$ = strdup(ccstr(st, ")")); }
       |WHILE LPAR exp RPAR stm         { fprintf(stderr, "%d: stm4\n", call_no++); char *st; st = initstr(st); st = ccstr(st, "(while "); st = ccstr(st, $3); st = ccstr(st, " "); st = ccstr(st, $5); $$ = strdup(ccstr(st, ")")); }
       |FOR LPAR vardec exp SEMICOLON exp RPAR stm      { fprintf(stderr, "%d: stm5\n", call_no++); char *st; st = initstr(st); st = ccstr(st, "(block ("); st = ccstr(st, $3); st = ccstr(st, ") ("); st = ccstr(st, "(while "); st = ccstr(st, $4); st = ccstr(st, " "); st = ccstr(st, "(block () ("); st = ccstr(st, $8); st = ccstr(st, " (expStm "); st = ccstr(st, $6); $$ = strdup(ccstr(st, "))))))")); }
       |RETURN SEMICOLON                { fprintf(stderr, "%d: stm6\n", call_no++); $$ = "(return0)"; }
       |RETURN exp SEMICOLON            { fprintf(stderr, "%d: stm7\n", call_no++); char *st; st = initstr(st); st = ccstr(st, "(return "); st = ccstr(st, $2); $$ = strdup(ccstr(st, ")")); }
       |block                           { fprintf(stderr, "%d: stm8\n", call_no++); $$ = strdup($1); }
       |HALT LPAR exp RPAR SEMICOLON    { fprintf(stderr, "%d: stm9\n", call_no++); char *st; st = initstr(st); st = ccstr(st, "(halt "); st = ccstr(st, $3); $$ = strdup(ccstr(st, ")")); }
       ;
exp:
        lhs                             { fprintf(stderr, "%d: lhs1\n", call_no++); char *ex; ex = initstr(ex); $$ = strdup(ccstr(ex, $1)); /*fprintf(stderr, "EXPLHS: %s----------\n", $$);*/ /*free(ex);*/ }
       |lhs ASSIGNOP exp                { fprintf(stderr, "%d: lhs2\n", call_no++); char *ex; ex = initstr(ex); ex = ccstr(ex, "(assign "); ex = ccstr(ex, $1); ex = ccstr(ex, " "); ex = ccstr(ex, $3); $$ = strdup(ccstr(ex, ")")); }
       ;
lhs:   
        disjunct                        { fprintf(stderr, "%d: disjunct1\n", call_no++); char *lh; lh = initstr(lh); $$ = strdup(ccstr(lh, $1)); /*fprintf(stderr, "LHSDIS: %s----------\n", $$);*/ /*free(lh);*/ }
       |disjunct OROP lhs               { fprintf(stderr, "%d: disjunct2\n", call_no++); char *lh; lh = initstr(lh); lh = ccstr(lh, "(binOpn "); lh = ccstr(lh, "or "); lh = ccstr(lh, $1); lh = ccstr(lh, " "); lh = ccstr(lh, $3); $$ = strdup(ccstr(lh, ")")); }
       ;
disjunct:
        conjunct                        { char *dis; dis = initstr(dis); $$ = strdup(ccstr(dis, $1)); }
       |conjunct ANDOP disjunct         { char *dis; dis = initstr(dis); dis = ccstr(dis, "(binOpn "); dis = ccstr(dis, "and "); dis = ccstr(dis, $1); dis = ccstr(dis, " "); dis = ccstr(dis, $3); $$ = strdup(ccstr(dis, ")")); /*fprintf(stderr, "DISADD: %s----------\n", $$);*/ /*free(dis);*/ }
       ;
conjunct:
        simple                          { fprintf(stderr, "%d: conjunct1\n", call_no++); char *con; con = initstr(con); $$ = strdup(ccstr(con, $1)); }
       |simple RELOP simple             { fprintf(stderr, "%d: conjucnt2\n", call_no++); char *con; con = initstr(con); con = ccstr(con, "(binOpn "); con = ccstr(con, $2); con = ccstr(con, " "); con = ccstr(con, $1); con = ccstr(con, " "); con = ccstr(con, $3); $$ = strdup(ccstr(con, ")")); }
       |simple LANGLE simple            { fprintf(stderr, "%d: conjunct3\n", call_no++); char *con; con = initstr(con); con = ccstr(con, "(binOpn "); con = ccstr(con, $2); con = ccstr(con, " "); con = ccstr(con, $1); con = ccstr(con, " "); con = ccstr(con, $3); $$ = strdup(ccstr(con, ")")); }
       |simple RANGLE simple            { fprintf(stderr, "%d: conjucnt4\n", call_no++); char *con; con = initstr(con); con = ccstr(con, "(binOpn "); con = ccstr(con, $2); con = ccstr(con, " "); con = ccstr(con, $1); con = ccstr(con, " "); con = ccstr(con, $3); $$ = strdup(ccstr(con, ")")); }
       ;
simple:
        term                            { fprintf(stderr, "%d: simple1\n", call_no++); char *spl; spl = initstr(spl); $$ = strdup(ccstr(spl, $1)); }
       |simple ADDOP term               { fprintf(stderr, "%d: simple2\n", call_no++); char *spl; spl = initstr(spl); spl = ccstr(spl, "(binOpn "); spl = ccstr(spl, $2); spl = ccstr(spl, " "); spl = ccstr(spl, $1); spl = ccstr(spl, " "); spl = ccstr(spl, $3); $$ = strdup(ccstr(spl, ")")); }
       |simple MINUS term               { fprintf(stderr, "%d: simple3\n", call_no++); char *spl; spl = initstr(spl); spl = ccstr(spl, "(binOpn "); spl = ccstr(spl, $2); spl = ccstr(spl, " "); spl = ccstr(spl, $1); spl = ccstr(spl, " "); spl = ccstr(spl, $3); $$ = strdup(ccstr(spl, ")")); }
       ;
term: 
        factor                          { fprintf(stderr, "%d: term1\n", call_no++); char *trm; trm = initstr(trm); $$ = strdup(ccstr(trm, $1)); }
       |term MULOP factor               { fprintf(stderr, "%d: term2\n", call_no++); char *trm; trm = initstr(trm); trm = ccstr(trm, "(binOpn "); trm = ccstr(trm, $2); trm = ccstr(trm, " "); trm = ccstr(trm, $1); trm = ccstr(trm, " "); trm = ccstr(trm, $3); $$ = strdup(ccstr(trm, ")")); }
       ;
factor:
        UNOP factor                     { fprintf(stderr, "%d: factor1\n", call_no++); char *fac; fac = initstr(fac); fac = ccstr(fac, "(unOpn "); fac = ccstr(fac, $1); fac = ccstr(fac, " "); fac = ccstr(fac, $2); $$ = strdup(ccstr(fac, ")")); }
       |MINUS factor                    { fprintf(stderr, "%d: factor2\n", call_no++); char *fac; fac = initstr(fac); fac = ccstr(fac, "(unOpn "); fac = ccstr(fac, $1); fac = ccstr(fac, " "); fac = ccstr(fac, $2); $$ = strdup(ccstr(fac, ")")); }
       |literal factorrest              { fprintf(stderr, "%d: factor3\n", call_no++); char *fac; fac = initstr(fac); $$ = strdup(ccstr(fac, $1)); }
       |ID factorrest                   { fprintf(stderr, "%d: factor4\n", call_no++); char *fac; fac = initstr(fac); $$ = strdup(ccstr(fac, $1)); /*free(fac);*/ }
       ;
factorrest:
                                        { fprintf(stderr, "%d: factorrest\n", call_no++); $$ = ""; }
        ;
literal:
        NULLVALUE                       { fprintf(stderr, "%d: literal1\n", call_no++); char *lit; lit = initstr(lit); $$ = strdup(ccstr(lit, "(null)")); }
       |TRUE                            { fprintf(stderr, "%d: literal2\n", call_no++); char *lit; lit = initstr(lit); $$ = strdup(ccstr(lit, "(true)")); }
       |FALSE                           { fprintf(stderr, "%d: literal3\n", call_no++); char *lit; lit = initstr(lit); $$ = strdup(ccstr(lit, "(false)")); }
       |CHARLITERAL                     { fprintf(stderr, "%d: literal4\n", call_no++); char *lit; lit = initstr(lit); lit = ccstr(lit, "(charLiteral #\\"); char *ch = extchar($1); lit = ccstr(lit, ch); $$ = strdup(ccstr(lit, ")")); }
       |STRINGLITERAL                   { fprintf(stderr, "%d: literal5\n", call_no++); char *lit; lit = initstr(lit); lit = ccstr(lit, "(stringLiteral "); lit = ccstr(lit, $1); $$ = strdup(ccstr(lit, ")")); }
       |INTLITERAL                      { fprintf(stderr, "%d: literal6\n", call_no++); char *lit; lit = initstr(lit); lit = ccstr(lit, "(intLiteral "); lit = ccstr(lit, $1); $$ = strdup(ccstr(lit, ")")); }
       |FLOATLITERAL                    { fprintf(stderr, "%d: literal7\n", call_no++); char *lit; lit = initstr(lit); lit = ccstr(lit, "(floatLiteral "); lit = ccstr(lit, $1); $$ = strdup(ccstr(lit, ")")); }
       ;
tvars:
        TVAR                            { fprintf(stderr, "%d: tvars1\n", call_no++); $$ = strdup($1); }
       |TVAR COMMA tvars                { fprintf(stderr, "%d: tvars2\n", call_no++); char *tvr; tvr = initstr(tvr); tvr = ccstr(tvr, $1); $$ = strdup(ccstr(tvr, $3)); /* LOOK AT THIS RULE*/}
       ;
typeapps:
                                        { fprintf(stderr, "%d: typeapps1\n", call_no++); $$ = ""; }
       |typeapp typeappsrest            { fprintf(stderr, "%d: typeapps2\n", call_no++); char *typ; typ = initstr(typ); typ = ccstr(typ, $1); $$ = strdup(ccstr(typ, $2)); }        
       ;
typeappsrest:
                                        { fprintf(stderr, "%d: typeappsrest1\n", call_no++); $$ = ""; }
       |COMMA typeapp typeappsrest      { fprintf(stderr, "%d: typeappsrest2\n", call_no++); char *tpr; tpr = initstr(tpr); tpr = ccstr(tpr, $2); $$ = strdup(ccstr(tpr, $3)); }        
       ;
fundecs:
                                        { fprintf(stderr, "%d: fundecs1\n", call_no++); $$ = ""; }
       |fundec fundecs                  { fprintf(stderr, "%d: fundecs2\n", call_no++); char *fnd; fnd = initstr(fnd); fnd = ccstr(fnd, $1); $$ = strdup(ccstr(fnd, $2)); }        
       ;
types:
                                        { fprintf(stderr, "%d: types1\n", call_no++); $$ = ""; }
       |type typesrest                  { fprintf(stderr, "%d: types2\n", call_no++); char *typ; typ = initstr(typ); typ = ccstr(typ, $1); $$ = strdup(ccstr(typ, $2)); }        
       ;
typesrest:
                                        { fprintf(stderr, "%d: typesrest1\n", call_no++); $$ = ""; }
       |COMMA type typesrest           { fprintf(stderr, "%d: typesrest2\n", call_no++); char *tpr; tpr = initstr(tpr); tpr = ccstr(tpr, $2); $$ = strdup(ccstr(tpr, $3)); }        
       ;
localdecs:
                                        { fprintf(stderr, "%d: localdecs1\n", call_no++); $$ = ""; }
       |localdec localdecs              { fprintf(stderr, "%d: localdecs2\n", call_no++); char *lcd; lcd = initstr(lcd); lcd = ccstr(lcd, $1); $$ = strdup(ccstr(lcd, $2)); }        
       ;
stms:
                                        { fprintf(stderr, "%d: stms1\n", call_no++); $$ = ""; }
       |stm stms                        { fprintf(stderr, "%d: stms2\n", call_no++); char *sts; sts = initstr(sts); sts = ccstr(sts, $1); $$ = strdup(ccstr(sts, $2)); }        
       ;
formals:
                                        { fprintf(stderr, "%d: formals1\n", call_no++); $$ = ""; }
       |formal formalsrest              { fprintf(stderr, "%d: formals2\n", call_no++); char *fml; fml = initstr(fml); fml = ccstr(fml, $1); $$ = strdup(ccstr(fml, $2)); }        
       ;
formalsrest:
                                        { fprintf(stderr, "%d: formalrest1\n", call_no++); $$ = ""; }
       |COMMA formal formalsrest        { fprintf(stderr, "%d: formalrest2\n", call_no++); char *fmr; fmr = initstr(fmr); fmr = ccstr(fmr, $2); $$ = strdup(ccstr(fmr, $3)); }        
       ;


%%
/*
  // deleted rules
  typeid:
        ID                              { fprintf(stderr, "%d: typeid\n", call_no++); $$ = strdup($1); }
       ; 
*/

//fprintf(stderr, "STRING: %s----------\n", dis); 

char * extchar(char *s)
{
        //fprintf(stderr, "%c-----\n", s[1]);
        char *ch =  (char*) malloc(2);
        ch[0] = s[1];
        ch[1] = '\0';
        return ch;
}

char * initstr(char *s)
{
        s =  (char*) malloc(10);
        s[0] = '\0';
        return s;
}

char * ccstr(char *s1, const char *s2)
{       
        int l = strlen(s1) + strlen(s2) + 10;
        //fprintf(stderr, "CCSTR: l = %d, %s, %s\n", l, s1, s2); 
        s1 = (char *) realloc(s1, l);
        strcat(s1, s2);
        return s1;
}


int yyerror(char *s)
{
        extern int yylineno;	// defined and maintained in lex.c
        extern char *yytext;	// defined and maintained in lex.c
        fprintf(stderr, "ERROR: %s at symbol on line %d\n", yytext, yylineno);
        printf("(illegal)"); 
        exit(1);
}

int main(int argc, char **argv)
{
        if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
        {
                fprintf(stderr, "argv[0]: File %s cannot be opened.\n", argv[1]) ;
                exit( 1 );
        }
        inittypeid();
        
        yyparse();

        return 0;
}
