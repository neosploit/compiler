%{
#include <ctype.h>
#include <math.h>
#include "pasc600.h"
#include "error.h"
#include "ast.h"
#include "parser.tab.h"

char string[256], string_escaped[256];

static int strtoint(const char* str);
static float strtoreal(const char* str);
static char charesc(const char* str);
static void stresc(const char* str, char* str_esc);
%}

%option     yylineno case-insensitive noinput nounput noyywrap nodefault
%x          STRING COMMENT

id          _?[a-zA-Z](_?[a-zA-Z0-9])*
iconst      [1-9][0-9]*|0[xX][1-9a-fA-F][0-9a-fA-F]*|0[oO][1-7][0-7]*|0[bB]1[01]*|0
rconst      ([1-9][0-9]*|0)?\.([1-9][0-9]*|0*[1-9][0-9]*|0)([eE][-+]?([1-9][0-9]*|0))?|([1-9][0-9]*|0)[eE][-+]?([1-9][0-9]*|0)|0[xX]([1-9a-fA-F][0-9a-fA-F]*|0)?\.([1-9A-F][0-9A-F]*|0*[1-9A-F][0-9A-F]*|0)|0[oO]([1-7][0-7]*|0)?\.([1-7][0-7]*|0*[1-7][0-7]*|0)|0[bB](1[01]*|0)?\.(1[01]*|0*1[0-1]*|0)
bconst      true|false
cconst      \'(.|\\[nftrbv])\'
listfunc    C(AD*|D+)R

%%

program                 { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_PROGRAM; }
const                   { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_CONST; }
type                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_TYPE; }
array                   { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_ARRAY; }
list                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_LIST; }
record                  { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_RECORD; }
set                     { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_SET; }
of                      { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_OF; }
var                     { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_VAR; }
integer                 { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_INTEGER; }
real                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_REAL; }
boolean                 { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_BOOLEAN; }
char                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_CHAR; }
string                  { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_STRING; }
forward                 { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_FORWARD; }
function                { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_FUNCTION; }
procedure               { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_PROCEDURE; }
begin                   { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_BEGIN; }
end                     { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_END; }
if                      { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_IF; }
then                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_THEN; }
else                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_ELSE; }
while                   { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_WHILE; }
do                      { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_DO; }
for                     { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_FOR; }
downto                  { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_DOWNTO; }
to                      { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_TO; }
with                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_WITH; }
read                    { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_READ; }
write                   { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_WRITE; }
length                  { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_LENGTH; }
new                     { if (DEBUG) printf("\n[KEYWORD] %s", yytext);      return TOKEN_NEW; }

or                      { if (DEBUG) printf("\n[OROP] %s", yytext);         yylval.operator = KW_OR;            return TOKEN_OROP; }
div                     { if (DEBUG) printf("\n[MULDIVANDOP] %s", yytext);  yylval.operator = KW_DIV;           return TOKEN_MULDIVANDOP; }
mod                     { if (DEBUG) printf("\n[MULDIVANDOP] %s", yytext);  yylval.operator = KW_MOD;           return TOKEN_MULDIVANDOP; }
and                     { if (DEBUG) printf("\n[MULDIVANDOP] %s", yytext);  yylval.operator = KW_AND;           return TOKEN_MULDIVANDOP; }
not                     { if (DEBUG) printf("\n[NOTOP] %s", yytext);        yylval.operator = KW_NOT;           return TOKEN_NOTOP; }
in                      { if (DEBUG) printf("\n[INOP] %s", yytext);         yylval.operator = KW_IN;            return TOKEN_INOP; }
">"                     { if (DEBUG) printf("\n[RELOP] %s", yytext);        yylval.operator = GREATER;          return TOKEN_RELOP; }
">="                    { if (DEBUG) printf("\n[RELOP] %s", yytext);        yylval.operator = GREATER_EQUAL;    return TOKEN_RELOP; }
"<"                     { if (DEBUG) printf("\n[RELOP] %s", yytext);        yylval.operator = LESS;             return TOKEN_RELOP; }
"<="                    { if (DEBUG) printf("\n[RELOP] %s", yytext);        yylval.operator = LESS_EQUAL;       return TOKEN_RELOP; }
"<>"                    { if (DEBUG) printf("\n[RELOP] %s", yytext);        yylval.operator = NOT_EQUAL;        return TOKEN_RELOP; }
"+"                     { if (DEBUG) printf("\n[ADDOP] %s", yytext);        yylval.operator = PLUS;             return TOKEN_ADDOP; }
"-"                     { if (DEBUG) printf("\n[ADDOP] %s", yytext);        yylval.operator = MINUS;            return TOKEN_ADDOP; }
"*"                     { if (DEBUG) printf("\n[MULDIVANDOP] %s", yytext);  yylval.operator = MUL;              return TOKEN_MULDIVANDOP; }
"/"                     { if (DEBUG) printf("\n[MULDIVANDOP] %s", yytext);  yylval.operator = DIV;              return TOKEN_MULDIVANDOP; }

"("                     { if (DEBUG) printf("\n[LPAREN] %s", yytext);       return TOKEN_LPAREN; }
")"                     { if (DEBUG) printf("\n[RPAREN] %s", yytext);       return TOKEN_RPAREN; }
";"                     { if (DEBUG) printf("\n[SEMI] %s", yytext);         return TOKEN_SEMI; }
"."                     { if (DEBUG) printf("\n[DOT] %s", yytext);          return TOKEN_DOT; }
","                     { if (DEBUG) printf("\n[COMMA] %s", yytext);        return TOKEN_COMMA; }
"="                     { if (DEBUG) printf("\n[EQU] %s", yytext);          yylval.operator = EQUAL;            return TOKEN_EQU; }
":"                     { if (DEBUG) printf("\n[COLON] %s", yytext);        return TOKEN_COLON; }
"["                     { if (DEBUG) printf("\n[LBRACK] %s", yytext);       return TOKEN_LBRACK; }
"]"                     { if (DEBUG) printf("\n[RBRACK] %s", yytext);       return TOKEN_RBRACK; }
":="                    { if (DEBUG) printf("\n[ASSIGN] %s", yytext);       return TOKEN_ASSIGN; }
".."                    { if (DEBUG) printf("\n[DOTDOT] %s", yytext);       return TOKEN_DOTDOT; }

{bconst}                { if (DEBUG) printf("\n[BCONST] %s", yytext);       yylval.bconst = strdup(yytext);     return TOKEN_BCONST; }
{listfunc}              { if (DEBUG) printf("\n[LISTFUNC] %s", yytext);     yylval.listfunc = strdup(yytext);   return TOKEN_LISTFUNC; }
{id}                    { if (DEBUG) printf("\n[ID] %s", yytext);           yylval.identifier = strdup(yytext); return TOKEN_ID; }
{iconst}                { if (DEBUG) printf("\n[ICONST] %s", yytext);       yylval.iconst = strtoint(yytext);   return TOKEN_ICONST; }
{rconst}                { if (DEBUG) printf("\n[RCONST] %s", yytext);       yylval.rconst = strtoreal(yytext);  return TOKEN_RCONST; }
{cconst}                { if (DEBUG) printf("\n[CCONST] %s", yytext);       yylval.cconst = charesc(yytext);    return TOKEN_CCONST; }

\"                      { strcpy(string, ""); strcpy(string_escaped, ""); BEGIN(STRING); }
<STRING>\"              { if (DEBUG) printf("\n[SCONST] %s", string); stresc(string, string_escaped); yylval.sconst = strdup(string_escaped); BEGIN(INITIAL); return TOKEN_SCONST; }
<STRING>\n              { scanner_error(yylineno, SCANNER_LINE_FEED_IN_STRING, string); }
<STRING>(\\.|[^"\\])    { strcat(string, yytext); }
<STRING>(\\\n)+         { }
<STRING><<EOF>>         { scanner_error(yylineno, SCANNER_EOF_IN_STRING, string); }
<STRING>.               { scanner_error(yylineno, SCANNER_UNKNOWN_CHARACTER_IN_STRING, yytext[0], yytext[0], string); }

"{"                     { BEGIN(COMMENT); }
<COMMENT>"}"            { BEGIN(INITIAL); }
<COMMENT>\n             { }
<COMMENT>[^}\n]         { /* Comment content */ }
<COMMENT><<EOF>>        { scanner_error(yylineno, SCANNER_EOF_IN_COMMENT); }

[ \f\t\r\b\v]+          { /* space & escape sequences */ }
\n                      { if (DEBUG) printf("\n\n-> Line No. %u <-", yylineno); }
<<EOF>>                 { if (DEBUG) printf("\n[EOF] <<EOF>>"); return 0; }
.                       { scanner_error(yylineno, SCANNER_UNKNOWN_CHARACTER, yytext[0], yytext[0]); }

%%

static int strtoint(const char* str) {

    if (!strncmp(str, "0B", 2) || !strncmp(str, "0b", 2)) return strtol(&str[2], NULL, 2);
    else if (!strncmp(str, "0O", 2) || !strncmp(str, "0o", 2)) return strtol(&str[2], NULL, 8);
    else if (!strncmp(str, "0X", 2) || !strncmp(str, "0x", 2)) return strtol(&str[2], NULL, 16);
    return strtol(str, NULL, 10);

}

static float strtoreal(const char* str) {

    int i, base;
    float res;
    char *integer, *fractional; // part

    if (!strncmp(str, "0B", 2) || !strncmp(str, "0b", 2)) { base = 2; res = strtol(&str[2], &integer, 2); }
    else if (!strncmp(str, "0O", 2) || !strncmp(str, "0o", 2)) { base = 8; res = strtol(&str[2], &integer, 8); }
    else if (!strncmp(str, "0X", 2) || !strncmp(str, "0x", 2)) { base = 16; res = strtol(&str[2], &integer, 16); }
    else { base = 10; }

    if (base != 10) {
        for (i = 1; i < strlen(integer); ++i) {
            res += (integer[i] <= '9') ? (integer[i] - '0') * (1 / pow(base, i)) : (integer[i] - 'A' + 10) * (1 / pow(base, i));
        }
    } else {
        if (strchr(str, '.')) {
            res = strtol(str, &integer, 10);
            strtol(&integer[1], &fractional, 10);

            for (i = 1; i < strlen(integer) - strlen(fractional); ++i) res += (integer[i] - '0') * (1 / pow(base, i));

            if (strchr(fractional, '+')) res *= pow(10, strtol(&fractional[2], NULL, 10));
            else if (strchr(fractional, '-')) res *= pow(10, strtol(&fractional[2], NULL, 10) * -1);
            else res *= pow(10, strtol(&fractional[1], NULL, 10));
        } else {
            res = strtol(str, &fractional, 10);

            if (strchr(fractional, '+')) res *= pow(10, strtol(&fractional[2], NULL, 10));
            else if (strchr(fractional, '-')) res *= pow(10, strtol(&fractional[2], NULL, 10) * -1);
            else res *= pow(10, strtol(&fractional[1], NULL, 10));
        }
    }

    return res;

}

static char charesc(const char* str) {

    if (str[1] == '\\' && str[2] != '\'') {
        switch (str[2]) {
            case 'n':   return '\n';
            case 'f':   return '\f';
            case 't':   return '\t';
            case 'r':   return '\r';
            case 'b':   return '\b';
            case 'v':   return '\v';
            default:
                scanner_error(yylineno, SCANNER_INVALID_ESCAPE_SEQUENCE_IN_CHARACTER, str[2], str[2], str);
                return '\0';
        }
    } else return str[1];

}

static void stresc(const char* str, char* str_esc) {

    int i, j;

    for (i = 0, j = 0; i < strlen(str); ++i, ++j) {
        if (str[i] == '\\') {
            switch (str[i + 1]) {
                case 'n':   str_esc[j] = '\n'; break;
                case 'f':   str_esc[j] = '\f'; break;
                case 't':   str_esc[j] = '\t'; break;
                case 'r':   str_esc[j] = '\r'; break;
                case 'b':   str_esc[j] = '\b'; break;
                case 'v':   str_esc[j] = '\v'; break;
                case '\\':  str_esc[j] = '\\'; break;
                case '\'':  str_esc[j] = '\''; break;
                case '\"':  str_esc[j] = '\"'; break;
                case '\?':  str_esc[j] = '\?'; break;
                default:    scanner_error(yylineno, SCANNER_INVALID_ESCAPE_SEQUENCE_IN_STRING, str[i + 1], str[i + 1], str);
            }

            ++i;
        } else str_esc[j] = str[i];
    }

    str_esc[j] = '\0';

}