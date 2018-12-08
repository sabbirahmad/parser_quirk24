/* Parser Generator for Quirk24 */
/* parser_generator.y */

/* Sabbir Ahmad */
/* ahmad.sab@husky.neu.edu */ 

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yyerror(const char *s);
int yylex(void);
int yyparse();

char * ccstr(char *s1, const char *s2); // concat strings s2 to end of s1 and return s1
char * initstr(char *s);                // initialize a string to a character pointer
char * extchar(char *s);                // extract only character from quotes: a from 'a'
char* extparts(char *s, int pos);       // extract two parts (first part: pos=0, second part: pos =1) for aref, call, dot
char * error_string(char* s);           // error message parsing

extern int inittypeid();                 // initialize array to save typeid, defined in "lexical_analyzer.l"

int yydebug = 0;                         // show debug if set to 1

%}

%debug

%error-verbose

%union{
	char*	sval;
}

%start	program 

%token SEMICOLON COMMA COLON LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET ARROW DOT VOID NULLVALUE TRUE FALSE FUNCTION EXTENDS IMPLEMENTS FUN CLASS RETURN IF ELSE WHILE FOR HALT NEW LAMBDA CONSTANT STATIC

%token	<sval>  INTLITERAL FLOATLITERAL ID TVAR ASSIGNOP OROP ANDOP RELOP ADDOP MULOP UNOP PRIMTYPE CHARLITERAL STRINGLITERAL MINUS LANGLE RANGLE PROTOCOL TYPEID 

%type   <sval>  program protodecs classdecs stm stms exp lhs disjunct conjunct simple term factor factorrest literal block localdecs localdec vardec type types formal formals fundec typevars tvars rtype typesrest formalsrest typeapp typeapps typeappsrest actuals actualsrest protodec extends funproto classdec bodydec init classbody constdec globaldec fielddec funprotos bodydecs /*fundecs*/

%left	ADDOP
%left	MULOP
%left   RELOP
%left   UNOP

%%

program:
        protodecs classdecs stm         { char *prg; prg = initstr(prg); prg = ccstr(prg, "(program ("); prg = ccstr(prg, $1); prg = ccstr(prg, ") ("); prg = ccstr(prg, $2); prg = ccstr(prg, ") "); prg = ccstr(prg, $3); $$ = strdup(ccstr(prg, ")")); puts($$); /*return 1;*/ }
       ;
protodec:
        PROTOCOL typevars extends LBRACE funprotos RBRACE       { char *prt; prt = initstr(prt); prt = ccstr(prt, "(protoDec "); prt = ccstr(prt, $1); prt = ccstr(prt, " ("); prt = ccstr(prt, $2); prt = ccstr(prt, ") ("); prt = ccstr(prt, $3); prt = ccstr(prt, ") ("); prt = ccstr(prt, $5); $$ = strdup(ccstr(prt, "))")); }
       ;
typevars:
                                        { $$ = ""; }
       |LANGLE tvars RANGLE             { char *tvr; tvr = initstr(tvr); $$ = strdup(ccstr(tvr, $2)); }
       ;
extends:
                                        { $$ = ""; }
       |EXTENDS typeapps                { char *ext; ext = initstr(ext); $$ = strdup(ccstr(ext, $2)); }
       ;
typeapp:
        TYPEID                          { char *tap; tap = initstr(tap); tap = ccstr(tap, "(typeApp "); tap = ccstr(tap, $1); tap = ccstr(tap, " ( "); $$ = strdup(ccstr(tap, " ))")); }
       |TYPEID LANGLE types RANGLE      { char *tap; tap = initstr(tap); tap = ccstr(tap, "(typeApp "); tap = ccstr(tap, $1); tap = ccstr(tap, " ( "); tap = ccstr(tap, $3); $$ = strdup(ccstr(tap, " ))")); }
       |TVAR                            { char *tap; tap = initstr(tap); tap = ccstr(tap, "(tVar "); tap = ccstr(tap, $1); $$ = strdup(ccstr(tap, ")")); }
       ;
funproto:
        FUN ID typevars LPAR formals RPAR SEMICOLON             { char *fnp; fnp = initstr(fnp); fnp = ccstr(fnp, "(funProto "); fnp = ccstr(fnp, $2); fnp = ccstr(fnp, " ("); fnp = ccstr(fnp, $3); fnp = ccstr(fnp, ") ("); fnp = ccstr(fnp, $5); fnp = ccstr(fnp, ") "); $$ = strdup(ccstr(fnp, "(void))")); }
       |FUN ID typevars LPAR formals RPAR COLON rtype SEMICOLON { char *fnp; fnp = initstr(fnp); fnp = ccstr(fnp, "(funProto "); fnp = ccstr(fnp, $2); fnp = ccstr(fnp, " ("); fnp = ccstr(fnp, $3); fnp = ccstr(fnp, ") ("); fnp = ccstr(fnp, $5); fnp = ccstr(fnp, ") "); fnp = ccstr(fnp, $8); $$ = strdup(ccstr(fnp, ")")); }
       ;
classdec:
        CLASS ID typevars IMPLEMENTS typeapps classbody { char *cld; cld = initstr(cld); cld = ccstr(cld, "(classDec "); cld = ccstr(cld, $2); cld = ccstr(cld, " ("); cld = ccstr(cld, $3); cld = ccstr(cld, ") ("); cld = ccstr(cld, $5); cld = ccstr(cld, ") "); cld = ccstr(cld, $6); $$ = strdup(ccstr(cld, ")")); }
       ;
classbody:
        LBRACE init bodydecs RBRACE     { char *clb; clb = initstr(clb); clb = ccstr(clb, $2); clb = ccstr(clb, " ("); clb = ccstr(clb, $3); $$ = strdup(ccstr(clb, ")")); }
       ;
init:
        LPAR formals RPAR block         { char *ini; ini = initstr(ini); ini = ccstr(ini, "(init ("); ini = ccstr(ini, $2); ini = ccstr(ini, ")"); ini = ccstr(ini, $4); $$ = strdup(ccstr(ini, ")")); }
       ;
bodydec:
        constdec                        { $$ = strdup($1); }
       |globaldec                       { $$ = strdup($1); }
       |fielddec                        { $$ = strdup($1); }
       |fundec                          { $$ = strdup($1); }
       ;
constdec:
        CONSTANT PRIMTYPE ID ASSIGNOP literal SEMICOLON         { char *cnd; cnd = initstr(cnd); cnd = ccstr(cnd, "(constant "); cnd = ccstr(cnd, $2); cnd = ccstr(cnd, " "); cnd = ccstr(cnd, $3); cnd = ccstr(cnd, " "); cnd = ccstr(cnd, $5); $$ = strdup(ccstr(cnd, ")")); }
       ;
globaldec:
        STATIC PRIMTYPE ID ASSIGNOP literal SEMICOLON           { char *gld; gld = initstr(gld); gld = ccstr(gld, "(static "); gld = ccstr(gld, $2); gld = ccstr(gld, " "); gld = ccstr(gld, $3); gld = ccstr(gld, " "); gld = ccstr(gld, $5); $$ = strdup(ccstr(gld, ")")); }
       ;
fielddec:
        formal SEMICOLON                { char *fld; fld = initstr(fld); fld = ccstr(fld, "(fieldDec "); fld = ccstr(fld, $1); $$ = strdup(ccstr(fld, ")")); }
       ;
formal:
        type ID                         { char *fml; fml = initstr(fml); fml = ccstr(fml, "(formal "); fml = ccstr(fml, $1); fml = ccstr(fml, " "); fml = ccstr(fml, $2); $$ = strdup(ccstr(fml, ")")); }
       ;
fundec:
        FUN ID typevars LPAR formals RPAR block                 { char *fnd; fnd = initstr(fnd); fnd = ccstr(fnd, "(funDec "); fnd = ccstr(fnd, $2); fnd = ccstr(fnd, "("); fnd = ccstr(fnd, $3); fnd = ccstr(fnd, ") ("); fnd = ccstr(fnd, $5); fnd = ccstr(fnd, ") "); fnd = ccstr(fnd, "(void) "); fnd = ccstr(fnd, $7); $$ = strdup(ccstr(fnd, ")")); }
       |FUN ID typevars LPAR formals RPAR COLON rtype block     { char *fnd; fnd = initstr(fnd); fnd = ccstr(fnd, "(funDec "); fnd = ccstr(fnd, $2); fnd = ccstr(fnd, "("); fnd = ccstr(fnd, $3); fnd = ccstr(fnd, ") ("); fnd = ccstr(fnd, $5); fnd = ccstr(fnd, ") "); fnd = ccstr(fnd, $8); fnd = ccstr(fnd, " "); fnd = ccstr(fnd, $9); $$ = strdup(ccstr(fnd, ")")); }
       ;
block:
        LBRACE localdecs stms RBRACE    { char *blk; blk = initstr(blk); blk = ccstr(blk, "(block ("); blk = ccstr(blk, $2); blk = ccstr(blk, ") ("); blk = ccstr(blk, $3); $$ = strdup(ccstr(blk, "))")); }
      ;
localdec:
        vardec                          { $$ = strdup($1); }
       |fundec                          { $$ = strdup($1); }
       ;
vardec:
        type ID ASSIGNOP exp SEMICOLON  { char *vrd; vrd = initstr(vrd); vrd = ccstr(vrd, "(varDec "); vrd = ccstr(vrd, $1); vrd = ccstr(vrd, " "); vrd = ccstr(vrd, $2); vrd = ccstr(vrd, " "); vrd = ccstr(vrd, $4); $$ = strdup(ccstr(vrd, ")")); }
       ;
type:
        PRIMTYPE                                        { char *typ; typ = initstr(typ); typ = ccstr(typ, "("); typ = ccstr(typ, $1); $$ = strdup(ccstr(typ, ")")); }
       |typeapp                                         { $$ = strdup($1); }
       |type LBRACKET RBRACKET                          { char *typ; typ = initstr(typ); typ = ccstr(typ, "(arrayOf "); typ = ccstr(typ, $1); $$ = strdup(ccstr(typ, ")")); }
       |FUNCTION LPAR LPAR types RPAR ARROW rtype RPAR  { char *typ; typ = initstr(typ); typ = ccstr(typ, "(funType ("); typ = ccstr(typ, $4); typ = ccstr(typ, ") "); typ = ccstr(typ, $7); $$ = strdup(ccstr(typ, ")")); }
       ;
rtype:
        type                            { $$ = strdup($1); }
       |VOID                            { $$ = "(void)"; }
       ;
stm:    
        SEMICOLON                                       { $$ = "(skip)"; }
       |exp SEMICOLON                                   { char *st; st = initstr(st); st = ccstr(st, "(expStm "); st = ccstr(st, $1); $$ = strdup(ccstr(st, ")")); }
       |IF LPAR exp RPAR stm ELSE stm                   { char *st; st = initstr(st); st = ccstr(st, "(if "); st = ccstr(st, $3); st = ccstr(st, " "); st = ccstr(st, $5); st = ccstr(st, " "); st = ccstr(st, $7); $$ = strdup(ccstr(st, ")")); }
       |WHILE LPAR exp RPAR stm                         { char *st; st = initstr(st); st = ccstr(st, "(while "); st = ccstr(st, $3); st = ccstr(st, " "); st = ccstr(st, $5); $$ = strdup(ccstr(st, ")")); }
       |FOR LPAR vardec exp SEMICOLON exp RPAR stm      { char *st; st = initstr(st); st = ccstr(st, "(block ("); st = ccstr(st, $3); st = ccstr(st, ") ("); st = ccstr(st, "(while "); st = ccstr(st, $4); st = ccstr(st, " "); st = ccstr(st, "(block () ("); st = ccstr(st, $8); st = ccstr(st, " (expStm "); st = ccstr(st, $6); $$ = strdup(ccstr(st, "))))))")); }
       |RETURN SEMICOLON                                { $$ = "(return0)"; }
       |RETURN exp SEMICOLON                            { char *st; st = initstr(st); st = ccstr(st, "(return "); st = ccstr(st, $2); $$ = strdup(ccstr(st, ")")); }
       |block                                           { $$ = strdup($1); }
       |HALT LPAR exp RPAR SEMICOLON                    { char *st; st = initstr(st); st = ccstr(st, "(halt "); st = ccstr(st, $3); $$ = strdup(ccstr(st, ")")); }
       ;
exp:
        lhs                             { char *ex; ex = initstr(ex); $$ = strdup(ccstr(ex, $1)); }
       |lhs ASSIGNOP exp                { char *ex; ex = initstr(ex); ex = ccstr(ex, "(assign "); ex = ccstr(ex, $1); ex = ccstr(ex, " "); ex = ccstr(ex, $3); $$ = strdup(ccstr(ex, ")")); }
       ;
lhs:   
        disjunct                        { char *lh; lh = initstr(lh); $$ = strdup(ccstr(lh, $1)); }
       |disjunct OROP lhs               { char *lh; lh = initstr(lh); lh = ccstr(lh, "(binOpn "); lh = ccstr(lh, "or "); lh = ccstr(lh, $1); lh = ccstr(lh, " "); lh = ccstr(lh, $3); $$ = strdup(ccstr(lh, ")")); }
       ;
disjunct:
        conjunct                        { char *dis; dis = initstr(dis); $$ = strdup(ccstr(dis, $1)); }
       |conjunct ANDOP disjunct         { char *dis; dis = initstr(dis); dis = ccstr(dis, "(binOpn "); dis = ccstr(dis, "and "); dis = ccstr(dis, $1); dis = ccstr(dis, " "); dis = ccstr(dis, $3); $$ = strdup(ccstr(dis, ")")); }
       ;
conjunct:
        simple                          { char *con; con = initstr(con); $$ = strdup(ccstr(con, $1)); }
       |simple RELOP simple             { char *con; con = initstr(con); con = ccstr(con, "(binOpn "); con = ccstr(con, $2); con = ccstr(con, " "); con = ccstr(con, $1); con = ccstr(con, " "); con = ccstr(con, $3); $$ = strdup(ccstr(con, ")")); }
       |simple LANGLE simple            { char *con; con = initstr(con); con = ccstr(con, "(binOpn "); con = ccstr(con, $2); con = ccstr(con, " "); con = ccstr(con, $1); con = ccstr(con, " "); con = ccstr(con, $3); $$ = strdup(ccstr(con, ")")); }
       |simple RANGLE simple            { char *con; con = initstr(con); con = ccstr(con, "(binOpn "); con = ccstr(con, $2); con = ccstr(con, " "); con = ccstr(con, $1); con = ccstr(con, " "); con = ccstr(con, $3); $$ = strdup(ccstr(con, ")")); }
       ;
simple:
        term                            { char *spl; spl = initstr(spl); $$ = strdup(ccstr(spl, $1)); }
       |simple ADDOP term               { char *spl; spl = initstr(spl); spl = ccstr(spl, "(binOpn "); spl = ccstr(spl, $2); spl = ccstr(spl, " "); spl = ccstr(spl, $1); spl = ccstr(spl, " "); spl = ccstr(spl, $3); $$ = strdup(ccstr(spl, ")")); }
       |simple MINUS term               { char *spl; spl = initstr(spl); spl = ccstr(spl, "(binOpn "); spl = ccstr(spl, $2); spl = ccstr(spl, " "); spl = ccstr(spl, $1); spl = ccstr(spl, " "); spl = ccstr(spl, $3); $$ = strdup(ccstr(spl, ")")); }
       ;
term: 
        factor                          { char *trm; trm = initstr(trm); $$ = strdup(ccstr(trm, $1)); }
       |term MULOP factor               { char *trm; trm = initstr(trm); trm = ccstr(trm, "(binOpn "); trm = ccstr(trm, $2); trm = ccstr(trm, " "); trm = ccstr(trm, $1); trm = ccstr(trm, " "); trm = ccstr(trm, $3); $$ = strdup(ccstr(trm, ")")); }
       ;
factor:                                                                                                                                                                                                                                                 
        UNOP factor                                             { char *fac; fac = initstr(fac); fac = ccstr(fac, "(unOpn "); fac = ccstr(fac, $1); fac = ccstr(fac, " "); fac = ccstr(fac, $2); $$ = strdup(ccstr(fac, ")")); }
       |MINUS factor                                            { char *fac; fac = initstr(fac); fac = ccstr(fac, "(unOpn "); fac = ccstr(fac, $1); fac = ccstr(fac, " "); fac = ccstr(fac, $2); $$ = strdup(ccstr(fac, ")")); }
       |literal factorrest                                      { char *fac; fac = initstr(fac); char *p1 = extparts($2, 0); char *p2 = extparts($2, 1); fac = ccstr(fac, p1); fac = ccstr(fac, $1); fac = ccstr(fac, " "); $$ = strdup(ccstr(fac, p2)); }
       |NEW ID LPAR actuals RPAR factorrest                     { char *fac; fac = initstr(fac); char *p1 = extparts($6, 0); char *p2 = extparts($6, 1); fac = ccstr(fac, p1); fac = ccstr(fac, "(newObject "); fac = ccstr(fac, $2); fac = ccstr(fac, "("); fac = ccstr(fac, $4); fac = ccstr(fac, "))"); $$ = strdup(ccstr(fac, p2)); }
       |NEW ID LANGLE types RANGLE LPAR actuals RPAR factorrest { char *fac; fac = initstr(fac); char *p1 = extparts($9, 0); char *p2 = extparts($9, 1); fac = ccstr(fac, p1); fac = ccstr(fac, "(newObject (classApp "); fac = ccstr(fac, $2); fac = ccstr(fac, "("); fac = ccstr(fac, $4); fac = ccstr(fac, "))"); fac = ccstr(fac, "("); fac = ccstr(fac, $7); fac = ccstr(fac, "))"); $$ = strdup(ccstr(fac, p2)); }
       |NEW type LBRACKET exp RBRACKET factorrest               { char *fac; fac = initstr(fac); char *p1 = extparts($6, 0); char *p2 = extparts($6, 1); fac = ccstr(fac, p1); fac = ccstr(fac, "(newArray "); fac = ccstr(fac, $2); fac = ccstr(fac, " "); fac = ccstr(fac, $4); fac = ccstr(fac, ")"); $$ = strdup(ccstr(fac, p2)); }
       |LAMBDA LPAR formals RPAR block factorrest               { char *fac; fac = initstr(fac); char *p1 = extparts($6, 0); char *p2 = extparts($6, 1); fac = ccstr(fac, p1); fac = ccstr(fac, "(lambda ("); fac = ccstr(fac, $3); fac = ccstr(fac, ")"); fac = ccstr(fac, "(void)"); fac = ccstr(fac, $5); fac = ccstr(fac, ")"); $$ = strdup(ccstr(fac, p2)); }
       |LAMBDA LPAR formals RPAR COLON rtype block factorrest   { char *fac; fac = initstr(fac); char *p1 = extparts($8, 0); char *p2 = extparts($8, 1); fac = ccstr(fac, p1); fac = ccstr(fac, "(lambda ("); fac = ccstr(fac, $3); fac = ccstr(fac, ")"); fac = ccstr(fac, $6); fac = ccstr(fac, " "); fac = ccstr(fac, $7); fac = ccstr(fac, ")"); $$ = strdup(ccstr(fac, p2)); }
       |LPAR exp RPAR factorrest                                { char *fac; fac = initstr(fac); char *p1 = extparts($4, 0); char *p2 = extparts($4, 1); fac = ccstr(fac, p1); fac = ccstr(fac, $2); fac = ccstr(fac, " "); $$ = strdup(ccstr(fac, p2)); }
       |ID factorrest                                           { char *fac; fac = initstr(fac); char *p1 = extparts($2, 0); char *p2 = extparts($2, 1); fac = ccstr(fac, p1); fac = ccstr(fac, $1); fac = ccstr(fac, " "); $$ = strdup(ccstr(fac, p2)); }
       ;
factorrest:
                                        { $$ = ""; }
       |LPAR actuals RPAR factorrest    { char *frr; frr = initstr(frr); char *p1 = extparts($4, 0); char *p2 = extparts($4, 1); frr = ccstr(frr, p1); frr = ccstr(frr, "(call , ("); frr = ccstr(frr, $2); frr = ccstr(frr, ")"); frr = ccstr(frr, ")"); $$ = strdup(ccstr(frr, p2)); }
       |DOT ID factorrest               { char *frr; frr = initstr(frr); char *p1 = extparts($3, 0); char *p2 = extparts($3, 1); frr = ccstr(frr, p1); frr = ccstr(frr, "(dot , "); frr = ccstr(frr, $2); frr = ccstr(frr, ")"); $$ = strdup(ccstr(frr, p2)); }
       |LBRACKET exp RBRACKET factorrest{ char *frr; frr = initstr(frr); char *p1 = extparts($4, 0); char *p2 = extparts($4, 1); frr = ccstr(frr, p1); frr = ccstr(frr, "(aref , "); frr = ccstr(frr, $2); frr = ccstr(frr, ")"); $$ = strdup(ccstr(frr, p2)); }
       ;
literal:
        NULLVALUE                       { char *lit; lit = initstr(lit); $$ = strdup(ccstr(lit, "(null)")); }
       |TRUE                            { char *lit; lit = initstr(lit); $$ = strdup(ccstr(lit, "(true)")); }
       |FALSE                           { char *lit; lit = initstr(lit); $$ = strdup(ccstr(lit, "(false)")); }
       |CHARLITERAL                     { char *lit; lit = initstr(lit); lit = ccstr(lit, "(charLiteral #\\"); char *ch = extchar($1); lit = ccstr(lit, ch); $$ = strdup(ccstr(lit, ")")); }
       |STRINGLITERAL                   { char *lit; lit = initstr(lit); lit = ccstr(lit, "(stringLiteral "); lit = ccstr(lit, $1); $$ = strdup(ccstr(lit, ")")); }
       |INTLITERAL                      { char *lit; lit = initstr(lit); lit = ccstr(lit, "(intLiteral "); lit = ccstr(lit, $1); $$ = strdup(ccstr(lit, ")")); }
       |FLOATLITERAL                    { char *lit; lit = initstr(lit); lit = ccstr(lit, "(floatLiteral "); lit = ccstr(lit, $1); $$ = strdup(ccstr(lit, ")")); }
       ;
tvars:
        TVAR                            { $$ = strdup($1); }
       |TVAR COMMA tvars                { char *tvr; tvr = initstr(tvr); tvr = ccstr(tvr, $1); tvr = ccstr(tvr, " "); $$ = strdup(ccstr(tvr, $3)); }
       ;
protodecs:
                                        { $$ = ""; }
       |protodec protodecs              { char *prt; prt = initstr(prt); prt = ccstr(prt, $1); prt = ccstr(prt, " "); $$ = strdup(ccstr(prt, $2)); }                                       
       ;
typeapps:
                                        { $$ = ""; }
       |typeapp typeappsrest            { char *typ; typ = initstr(typ); typ = ccstr(typ, $1); typ = ccstr(typ, " "); $$ = strdup(ccstr(typ, $2)); }        
       ;
typeappsrest:
                                        { $$ = ""; }
       |COMMA typeapp typeappsrest      { char *tpr; tpr = initstr(tpr); tpr = ccstr(tpr, $2); tpr = ccstr(tpr, " "); $$ = strdup(ccstr(tpr, $3)); }        
       ;
funprotos:
                                        { $$ = ""; }
       |funproto funprotos              { char *fprt; fprt = initstr(fprt); fprt = ccstr(fprt, $1); fprt = ccstr(fprt, " "); $$ = strdup(ccstr(fprt, $2)); }                                       
       ;
classdecs:
                                        { $$ = ""; }
       |classdec classdecs              { char *cld; cld = initstr(cld); cld = ccstr(cld, $1); cld = ccstr(cld, " "); $$ = strdup(ccstr(cld, $2)); }                                        
       ; 
/*
//commented out, because no rule uses this
fundecs:
                                        { $$ = ""; }
       |fundec fundecs                  { char *fnd; fnd = initstr(fnd); fnd = ccstr(fnd, $1); $$ = strdup(ccstr(fnd, $2)); }        
       ;
*/
types:
                                        { $$ = ""; }
       |type typesrest                  { char *typ; typ = initstr(typ); typ = ccstr(typ, $1); typ = ccstr(typ, " "); $$ = strdup(ccstr(typ, $2)); }        
       ;
typesrest:
                                        { $$ = ""; }
       |COMMA type typesrest            { char *tpr; tpr = initstr(tpr); tpr = ccstr(tpr, $2); tpr = ccstr(tpr, " "); $$ = strdup(ccstr(tpr, $3)); }        
       ;
bodydecs:
                                        { $$ = ""; }
       |bodydec bodydecs                { char *bdd; bdd = initstr(bdd); bdd = ccstr(bdd, $1); bdd = ccstr(bdd, " "); $$ = strdup(ccstr(bdd, $2)); }        
       ;
localdecs:
                                        { $$ = ""; }
       |localdec localdecs              { char *lcd; lcd = initstr(lcd); lcd = ccstr(lcd, $1); lcd = ccstr(lcd, " "); $$ = strdup(ccstr(lcd, $2)); }        
       ;
stms:
                                        { $$ = ""; }
       |stm stms                        { char *sts; sts = initstr(sts); sts = ccstr(sts, $1); sts = ccstr(sts, " "); $$ = strdup(ccstr(sts, $2)); }        
       ;
formals:
                                        { $$ = ""; }
       |formal formalsrest              { char *fml; fml = initstr(fml); fml = ccstr(fml, $1); fml = ccstr(fml, " "); $$ = strdup(ccstr(fml, $2)); }        
       ;
formalsrest:
                                        { $$ = ""; }
       |COMMA formal formalsrest        { char *fmr; fmr = initstr(fmr); fmr = ccstr(fmr, $2); fmr = ccstr(fmr, " "); $$ = strdup(ccstr(fmr, $3)); }        
       ;
actuals:
                                        { $$ = ""; }
       |exp actualsrest                 { char *act; act = initstr(act); act = ccstr(act, $1); act = ccstr(act, " "); $$ = strdup(ccstr(act, $2)); }        
       ;
actualsrest:
                                        { $$ = ""; }
       |COMMA exp actualsrest           { char *acr; acr = initstr(acr); acr = ccstr(acr, $2); acr = ccstr(acr, " "); $$ = strdup(ccstr(acr, $3)); }        
       ;

%%

char* extparts(char *s, int pos)  // extract two parts (first part: pos=0, second part: pos =1) for aref, call, dot 
{
        char *token;
        char *str = strdup(s);
        if(!strcmp(str, "")){
                return "";
        }

        token = strtok(str, ",");
        if(pos){
                token = strtok(NULL, ",");
        }
        
        return token;
}

char * extchar(char *s)  // extract only character from quotes: a from 'a'
{
        char *ch =  (char*) malloc(2);
        ch[0] = s[1];
        ch[1] = '\0';
        return ch;
}

char * initstr(char *s)  // initialize a string to a character pointer  
{
        s =  (char*) malloc(10);
        s[0] = '\0';
        return s;
}

char * ccstr(char *s1, const char *s2)  // concat strings s2 to end of s1 and return s1 
{       
        int l = strlen(s1) + strlen(s2) + 10;
        s1 = (char *) realloc(s1, l);
        strcat(s1, s2);
        return s1;
}

char * error_string(char* s)
{       
        int l = strlen(s);
        int i, j=0;
        char *msg = (char*) malloc(l - 10);
        for (i=14;i<l;i++)
                msg[j++] = s[i];
                
        return msg;
}

int yyerror(const char *s)
{
        extern int yylineno;  // defined by flex in lexical analyzer  
        extern char *yytext;  // defined by flex in lexical analyzer
        
        fprintf(stderr, "Syntax Error: \n\t----> in line %d, at token \"%s\"\n", yylineno, yytext);
        //fprintf(stderr, "\t%s\n", s);
        char *err_s = error_string(strdup(s));
        fprintf(stderr, "\t----> %s\n", err_s);
        
        
        printf("(illegal)");  // syntax error, so write (illegal) in the abstract syntax tree
        
        exit(1);
}

int main(int argc, char **argv)
{
        if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
        {
                fprintf(stderr, "argv[0]: File %s cannot be opened.\n", argv[1]) ;
                exit(1);
        }
        inittypeid();  // initialize array to store typeid, array defined in "lexical_analyzer.l"
        
        yyparse();

        return 0;
}
