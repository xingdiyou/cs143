/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
 
%}

%option noyywrap
%option stack

%x COMMENT
%x STRING
%x ESCAPE
%x ILLEGAL

/*
 * Define names for regular expressions here.
 */

%%

"(*" {
    yy_push_state(COMMENT);
}

"*)" {
    yylval.error_msg = "Unmatched *)";
    return ERROR;
}

<COMMENT>"(*" {
    yy_push_state(COMMENT);
}

<COMMENT>"*)" {
    yy_pop_state();
}

<COMMENT><<EOF>> {
    BEGIN(INITIAL);
    yylval.error_msg = "EOF in comment";
    return ERROR;
}

<COMMENT>. {
}

<COMMENT>\n {
    curr_lineno++;
}

"=>" {
    return DARROW;
}

"<-" {
    return ASSIGN;
}

"<=" {
    return LE;
}

(?i:case) {
    return CASE;
}

(?i:class) {
    return CLASS;
}

(?i:else) {
    return ELSE;
}

(?i:esac) {
    return ESAC;
}

(?i:fi) {
    return FI;
}

(?i:if) {
    return IF;
}

(?i:in) {
    return IN;
}

(?i:inherits) {
    return INHERITS;
}

(?i:isvoid) {
    return ISVOID;
}

(?i:let) {
    return LET;
}

(?i:loop) {
    return LOOP;
}

(?i:new) {
    return NEW;
}

(?i:not) {
    return NOT;
}

(?i:of) {
    return OF;
}

(?i:pool) {
    return POOL;
}

(?i:then) {
    return THEN;
}

(?i:while) {
    return WHILE;
}

t(?i:rue) {
    yylval.boolean = 1;
    return BOOL_CONST;
}

f(?i:alse) {
    yylval.boolean = 0;
    return BOOL_CONST;
}

\" {
    BEGIN(STRING);
    string_buf_ptr = string_buf;
}

<STRING>\" {
    BEGIN(INITIAL);
    *string_buf_ptr = '\0';
    yylval.symbol = stringtable.add_string(string_buf);
    return STR_CONST;
}

<STRING>\\ {
    BEGIN(ESCAPE);
}

<STRING>\0 {
    BEGIN(ILLEGAL);
    yylval.error_msg = "String contains escaped null character";
    return ERROR;
}

<STRING>. {
    if (string_buf_ptr == &string_buf[MAX_STR_CONST - 1]) {
        BEGIN(ILLEGAL);
        yylval.error_msg = "String constant too long";
        return ERROR;
    }
    *string_buf_ptr++ = *yytext;
}

<STRING>\n {
    BEGIN(INITIAL);
    curr_lineno++;
    yylval.error_msg = "Unterminated string constant";
    return ERROR;
}

<STRING><<EOF>> {
    BEGIN(INITIAL);
    yylval.error_msg = "EOF in string constant";
    return ERROR;
}

<ESCAPE>[b] {
    BEGIN(STRING);
    *string_buf_ptr++ = '\b';
}

<ESCAPE>[f] {
    BEGIN(STRING);
    *string_buf_ptr++ = '\f';
}

<ESCAPE>[n] {
    if (string_buf_ptr == &string_buf[MAX_STR_CONST - 1]) {
        BEGIN(ILLEGAL);
        yylval.error_msg = "String constant too long";
        return ERROR;
    }
    BEGIN(STRING);
    *string_buf_ptr++ = '\n';
}

<ESCAPE>[t] {
    BEGIN(STRING);
    *string_buf_ptr++ = '\t';
}

<ESCAPE>[v] {
    BEGIN(STRING);
    *string_buf_ptr++ = '\v';
}

<ESCAPE>\0 {
    BEGIN(ILLEGAL);
    yylval.error_msg = "String contains escaped null character";
    return ERROR;
}

<ESCAPE>. {
    if (string_buf_ptr == &string_buf[MAX_STR_CONST - 1]) {
        BEGIN(ILLEGAL);
        yylval.error_msg = "String constant too long";
        return ERROR;
    }
    BEGIN(STRING);
    *string_buf_ptr++ = *yytext;
}

<ESCAPE>\n {
    curr_lineno++;
    if (string_buf_ptr == &string_buf[MAX_STR_CONST - 1]) {
        BEGIN(ILLEGAL);
        yylval.error_msg = "String constant too long";
        return ERROR;
    }
    BEGIN(STRING);
    *string_buf_ptr++ = '\n';
}

<ILLEGAL>\" {
    BEGIN(INITIAL);
}

<ILLEGAL>. {
}

<ILLEGAL>\n {
    BEGIN(INITIAL);
    curr_lineno++;
}

[0-9]+ {
    yylval.symbol = idtable.add_string(yytext);
    return INT_CONST;
}

[A-Z][a-zA-Z_0-9]* {
    yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
}

[a-z][a-zA-Z_0-9]* {
    yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
}

[{}():;.,+\-*/=~\<@] {
    return *yytext;
}

[ \f\r\t\v]+ {
}

--.* {
}

. {
    yylval.error_msg = yytext;
    return ERROR;
}

\n {
    curr_lineno++;
}

%%
