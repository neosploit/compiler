%{
#include <limits.h>
#include "pasc600.h"
#include "ast.h"
#include "ast_print.h"

static void yyerror(const char* error);
%}

%define parse.error verbose

%union {
    /* Terminals */
    char*           identifier;
    int             iconst;
    float           rconst;
    char*           bconst;
    char            cconst;
    char*           sconst;
    char*           listfunc;
    Operator        operator;
    Pass            pass;

    /* Non-terminals */
    Id*             id;
    AST*            ast;
    Declaration*    declaration;
    Definition*     definition;
    Expression*     expression;
    Variable*       variable;
    Constant*       constant;
    Type*           type;
    Statement*      statement;
}

%token  TOKEN_PROGRAM TOKEN_CONST TOKEN_TYPE TOKEN_ARRAY TOKEN_LIST TOKEN_RECORD TOKEN_SET TOKEN_OF TOKEN_VAR TOKEN_INTEGER TOKEN_REAL TOKEN_BOOLEAN TOKEN_CHAR TOKEN_STRING
        TOKEN_FORWARD TOKEN_FUNCTION TOKEN_PROCEDURE TOKEN_BEGIN TOKEN_END TOKEN_IF TOKEN_THEN TOKEN_ELSE TOKEN_WHILE TOKEN_DO TOKEN_FOR TOKEN_DOWNTO TOKEN_TO TOKEN_WITH
        TOKEN_READ TOKEN_WRITE TOKEN_LENGTH TOKEN_NEW TOKEN_LPAREN TOKEN_RPAREN TOKEN_SEMI TOKEN_DOT TOKEN_COMMA TOKEN_COLON TOKEN_LBRACK TOKEN_RBRACK TOKEN_ASSIGN TOKEN_DOTDOT

%token  <identifier>    TOKEN_ID
%token  <iconst>        TOKEN_ICONST
%token  <rconst>        TOKEN_RCONST
%token  <bconst>        TOKEN_BCONST
%token  <cconst>        TOKEN_CCONST
%token  <sconst>        TOKEN_SCONST
%token  <listfunc>      TOKEN_LISTFUNC
%token  <operator>      TOKEN_RELOP
%token  <operator>      TOKEN_ADDOP
%token  <operator>      TOKEN_OROP
%token  <operator>      TOKEN_MULDIVANDOP
%token  <operator>      TOKEN_NOTOP
%token  <operator>      TOKEN_INOP
%token  <operator>      TOKEN_EQU

%type   <operator>      sign
%type   <pass>          pass
%type   <id>            identifiers
%type   <ast>           program
%type   <declaration>   declarations
%type   <definition>    header constdefs constant_defs typedefs type_defs fields field vardefs variable_defs subprograms subprogram sub_header formal_parameters parameter_list
%type   <expression>    expression expressions setlistexpression dims limits iter_space write_list write_item
%type   <variable>      variable read_list read_item
%type   <constant>      constant limit
%type   <type>          type_def typename standard_type
%type   <statement>     comp_statement statements statement assignment if_statement if_tail while_statement for_statement with_statement subprogram_call io_statement

%nonassoc               TOKEN_INOP TOKEN_RELOP TOKEN_EQU
%left                   TOKEN_ADDOP TOKEN_OROP
%left                   TOKEN_MULDIVANDOP
%precedence             TOKEN_NOTOP
%nonassoc               NO_ELSE
%nonassoc               TOKEN_ELSE

%%

program :   header declarations subprograms comp_statement TOKEN_DOT                    { ast = $$ = ast_program($1, $2, $3, $4);
                                                                                            if (DEBUG) {
                                                                                                AstPrintState ast_print_state = ast_print_state_initialize($$);
                                                                                                ast_print(&ast_print_state, $$);
                                                                                                fclose(ast_print_state.fp);
                                                                                                printf("\n\n[!] Program's generated AST was printed in: %s", ast_print_state.filename);
                                                                                            }
                                                                                        }
        |   header declarations subprograms comp_statement error                        { yyerrok; $$ = ast_program($1, $2, $3, $4);
                                                                                            if (DEBUG) {
                                                                                                AstPrintState ast_print_state = ast_print_state_initialize($$);
                                                                                                ast_print(&ast_print_state, $$);
                                                                                                fclose(ast_print_state.fp);
                                                                                                printf("\n\n[!] Program's generated AST was printed in: %s", ast_print_state.filename);
                                                                                            }
                                                                                        }
        ;

header  :   TOKEN_PROGRAM TOKEN_ID TOKEN_SEMI                                           { $$ = ast_definition_program(yylineno, ast_id($2)); }
        |   TOKEN_PROGRAM TOKEN_ID error                                                { yyerrok; $$ = ast_definition_program(yylineno, ast_id($2)); }
        ;

declarations    :   constdefs typedefs vardefs                                          { $$ = ast_program_declarations($1, $2, $3); }
                ;

constdefs   :   TOKEN_CONST constant_defs TOKEN_SEMI                                    { $$ = $2; }
            |   %empty                                                                  { $$ = NULL; }
            ;

constant_defs   :   constant_defs TOKEN_SEMI TOKEN_ID TOKEN_EQU expression              { LIST_APPEND(Definition, $$, $1, ast_definition_constant(yylineno, ast_id($3), $5)); }
                |   TOKEN_ID TOKEN_EQU expression                                       { $$ = ast_definition_constant(yylineno, ast_id($1), $3); }
                ;

expression  :   expression TOKEN_RELOP expression                                       { $$ = ast_expression_binary(yylineno, $2, $1, $3); }
            |   expression TOKEN_EQU expression                                         { $$ = ast_expression_binary(yylineno, $2, $1, $3); }
            |   expression TOKEN_INOP expression                                        { $$ = ast_expression_binary(yylineno, $2, $1, $3); }
            |   expression TOKEN_OROP expression                                        { $$ = ast_expression_binary(yylineno, $2, $1, $3); }
            |   expression TOKEN_ADDOP expression                                       { $$ = ast_expression_binary(yylineno, $2, $1, $3); }
            |   expression TOKEN_MULDIVANDOP expression                                 { $$ = ast_expression_binary(yylineno, $2, $1, $3); }
            |   TOKEN_ADDOP expression                                                  { $$ = ast_expression_unary(yylineno, $1, $2); }
            |   TOKEN_NOTOP expression                                                  { $$ = ast_expression_unary(yylineno, $1, $2); }
            |   variable                                                                { $$ = ast_expression_variable(yylineno, $1); }
            |   TOKEN_ID TOKEN_LPAREN expressions TOKEN_RPAREN                          { $$ = ast_expression_function_call(yylineno, ast_id($1), $3); }
            |   TOKEN_LENGTH TOKEN_LPAREN expression TOKEN_RPAREN                       { $$ = ast_expression_length(yylineno, $3); }
            |   TOKEN_NEW TOKEN_LPAREN expression TOKEN_RPAREN                          { $$ = ast_expression_new(yylineno, $3); }
            |   constant                                                                { $$ = ast_expression_constant(yylineno, $1); }
            |   TOKEN_LPAREN expression TOKEN_RPAREN                                    { $$ = ast_expression_enclosed(yylineno, $2); }
            |   setlistexpression                                                       { $$ = ast_expression_setlist(yylineno, $1); }
            ;

variable    :   TOKEN_ID                                                                { $$ = ast_variable_id(yylineno, ast_id($1)); }
            |   variable TOKEN_DOT TOKEN_ID                                             { $$ = ast_variable_component(yylineno, $1, ast_id($3)); }
            |   variable TOKEN_LBRACK expressions TOKEN_RBRACK                          { $$ = ast_variable_index(yylineno, $1, $3); }
            |   TOKEN_LISTFUNC TOKEN_LPAREN expression TOKEN_RPAREN                     { $$ = ast_variable_listfunc(yylineno, ast_id($1), $3); }
            ;

expressions :   expressions TOKEN_COMMA expression                                      { LIST_APPEND(Expression, $$, $1, $3); }
            |   expression                                                              { $$ = $1; }
            ;

constant    :   TOKEN_ICONST                                                            { $$ = ast_constant_integer(yylineno, UNDEFINED, $1); }
            |   TOKEN_RCONST                                                            { $$ = ast_constant_real(yylineno, $1); }
            |   TOKEN_BCONST                                                            { $$ = ast_constant_boolean(yylineno, $1); }
            |   TOKEN_CCONST                                                            { $$ = ast_constant_character(yylineno, $1); }
            |   TOKEN_SCONST                                                            { $$ = ast_constant_string(yylineno, $1); }
            ;

setlistexpression   :   TOKEN_LBRACK expressions TOKEN_RBRACK                           { $$ = $2; }
                    |   TOKEN_LBRACK TOKEN_RBRACK                                       { $$ = NULL; }
                    ;

typedefs    :   TOKEN_TYPE type_defs TOKEN_SEMI                                         { $$ = $2; }
            |   %empty                                                                  { $$ = NULL; }
            ;

type_defs   :   type_defs TOKEN_SEMI TOKEN_ID TOKEN_EQU type_def                        { LIST_APPEND(Definition, $$, $1, ast_definition_type(yylineno, ast_id($3), $5)); }
            |   TOKEN_ID TOKEN_EQU type_def                                             { $$ = ast_definition_type(yylineno, ast_id($1), $3); }
            ;

type_def    :   TOKEN_ARRAY TOKEN_LBRACK dims TOKEN_RBRACK TOKEN_OF typename            { $$ = ast_type_array(yylineno, $3, $6); }
            |   TOKEN_SET TOKEN_OF typename                                             { $$ = ast_type_set(yylineno, $3); }
            |   TOKEN_LIST TOKEN_OF typename                                            { $$ = ast_type_list(yylineno, $3); }
            |   TOKEN_RECORD fields TOKEN_END                                           { $$ = ast_type_record(yylineno, $2); }
            |   TOKEN_LPAREN identifiers TOKEN_RPAREN                                   { $$ = ast_type_enumeration(yylineno, $2); }
            |   limit TOKEN_DOTDOT limit                                                { $$ = ast_type_subrange(yylineno, $1, $3); }
            ;

dims    :   dims TOKEN_COMMA limits                                                     { LIST_APPEND(Expression, $$, $1, $3); }
        |   limits                                                                      { $$ = $1; }
        ;

limits  :   limit TOKEN_DOTDOT limit                                                    { $$ = ast_expression_limits(yylineno, $1, $3); }
        |   TOKEN_ID                                                                    { $$ = ast_expression_limits_id(yylineno, ast_id($1)); }
        ;

limit   :   sign TOKEN_ICONST                                                           { $$ = ast_constant_integer(yylineno, $1, $2); }
        |   TOKEN_CCONST                                                                { $$ = ast_constant_character(yylineno, $1); }
        |   TOKEN_BCONST                                                                { $$ = ast_constant_boolean(yylineno, $1); }
        |   TOKEN_ADDOP TOKEN_ID                                                        { $$ = ast_constant_id(yylineno, $1, ast_id($2)); }
        |   TOKEN_ID                                                                    { $$ = ast_constant_id(yylineno, UNDEFINED, ast_id($1)); }
        ;

sign    :   TOKEN_ADDOP                                                                 { $$ = $1; }
        |   %empty                                                                      { $$ = UNDEFINED; }
        ;

typename    :   standard_type                                                           { $$ = $1; }
            |   TOKEN_ID                                                                { $$ = ast_type_id(yylineno, ast_id($1)); }
            ;

standard_type   :   TOKEN_INTEGER                                                       { $$ = ast_type_integer(); }
                |   TOKEN_REAL                                                          { $$ = ast_type_real(); }
                |   TOKEN_BOOLEAN                                                       { $$ = ast_type_boolean(); }
                |   TOKEN_CHAR                                                          { $$ = ast_type_character(); }
                |   TOKEN_STRING                                                        { $$ = ast_type_string(); }
                ;

fields  :   fields TOKEN_SEMI field                                                     { LIST_APPEND(Definition, $$, $1, $3); }
        |   field                                                                       { $$ = $1; }
        ;

field   :   identifiers TOKEN_COLON typename                                            { $$ = ast_definition_field(yylineno, $1, $3); }
        ;

identifiers :   identifiers TOKEN_COMMA TOKEN_ID                                        { LIST_APPEND(Id, $$, $1, ast_id($3)); }
            |   TOKEN_ID                                                                { $$ = ast_id($1); }
            ;

vardefs :   TOKEN_VAR variable_defs TOKEN_SEMI                                          { $$ = $2; }
        |   %empty                                                                      { $$ = NULL; }
        ;

variable_defs   :   variable_defs TOKEN_SEMI identifiers TOKEN_COLON typename           { LIST_APPEND(Definition, $$, $1, ast_definition_variable(yylineno, $3, $5)); }
                |   identifiers TOKEN_COLON typename                                    { $$ = ast_definition_variable(yylineno, $1, $3); }
                ;

subprograms :   subprograms subprogram TOKEN_SEMI                                       { LIST_APPEND(Definition, $$, $1, $2); }
            |   %empty                                                                  { $$ = NULL; }
            ;

subprogram  :   sub_header TOKEN_SEMI TOKEN_FORWARD                                     { $$ = ast_definition_subprogram_forward(yylineno, $1); }
            |   sub_header TOKEN_SEMI declarations subprograms comp_statement           { $$ = ast_definition_subprogram(yylineno, $1, $3, $4, $5);
                                                                                            if (DEBUG) {
                                                                                                AstPrintState ast_print_state = ast_definition_print_state_initialize($$);
                                                                                                ast_definition_print(&ast_print_state, $$);
                                                                                                fclose(ast_print_state.fp);
                                                                                                printf("\n\n[!] Subprogram's generated AST was printed in: %s\n", ast_print_state.filename);
                                                                                            }
                                                                                        }
            ;

sub_header  :   TOKEN_FUNCTION TOKEN_ID formal_parameters TOKEN_COLON typename          { $$ = ast_definition_function(yylineno, ast_id($2), $3, $5); }
            |   TOKEN_PROCEDURE TOKEN_ID formal_parameters                              { $$ = ast_definition_procedure(yylineno, ast_id($2), $3); }
            |   TOKEN_FUNCTION TOKEN_ID                                                 { $$ = ast_definition_function(yylineno, ast_id($2), NULL, NULL); }
            ;

formal_parameters   :   TOKEN_LPAREN parameter_list TOKEN_RPAREN                        { $$ = $2; }
                    |   %empty                                                          { $$ = NULL; }
                    ;

parameter_list  :   parameter_list TOKEN_SEMI pass identifiers TOKEN_COLON typename     { LIST_APPEND(Definition, $$, $1, ast_definition_parameter(yylineno, $3, $4, $6)); }
                |   pass identifiers TOKEN_COLON typename                               { $$ = ast_definition_parameter(yylineno, $1, $2, $4); }
                ;

pass    :   TOKEN_VAR                                                                   { $$ = BY_REFERENCE; }
        |   %empty                                                                      { $$ = BY_VALUE; }
        ;

comp_statement  :   TOKEN_BEGIN statements TOKEN_END                                    { $$ = $2; }
                ;

statements  :   statements TOKEN_SEMI statement                                         { LIST_APPEND(Statement, $$, $1, $3); }
            |   statement                                                               { $$ = $1; }
            ;

statement   :   assignment                                                              { $$ = $1; }
            |   if_statement                                                            { $$ = $1; }
            |   while_statement                                                         { $$ = $1; }
            |   for_statement                                                           { $$ = $1; }
            |   with_statement                                                          { $$ = $1; }
            |   subprogram_call                                                         { $$ = $1; }
            |   io_statement                                                            { $$ = $1; }
            |   comp_statement                                                          { $$ = $1; }
            |   %empty                                                                  { $$ = NULL; }
            ;

assignment  :   variable TOKEN_ASSIGN expression                                        { $$ = ast_statement_assignment(yylineno, $1, $3); }
            ;

if_statement    :   TOKEN_IF expression TOKEN_THEN statement if_tail                    { $$ = ast_statement_if(yylineno, $2, $4, $5); }
                ;

if_tail :   TOKEN_ELSE statement                                                        { $$ = $2; }
        |   %empty %prec NO_ELSE                                                        { $$ = NULL; }
        ;

while_statement :   TOKEN_WHILE expression TOKEN_DO statement                           { $$ = ast_statement_while(yylineno, $2, $4); }
                ;

for_statement   :   TOKEN_FOR TOKEN_ID TOKEN_ASSIGN iter_space TOKEN_DO statement       { $$ = ast_statement_for(yylineno, ast_id($2), $4, $6); }
                ;

iter_space  :   expression TOKEN_TO expression                                          { $$ = ast_expression_iter_space(yylineno, TO, $1, $3); }
            |   expression TOKEN_DOWNTO expression                                      { $$ = ast_expression_iter_space(yylineno, DOWNTO, $1, $3); }
            ;

with_statement  :   TOKEN_WITH variable TOKEN_DO statement                              { $$ = ast_statement_with(yylineno, $2, $4); }
                ;

subprogram_call :   TOKEN_ID                                                            { $$ = ast_statement_procedure_call(yylineno, ast_id($1), NULL); }
                |   TOKEN_ID TOKEN_LPAREN expressions TOKEN_RPAREN                      { $$ = ast_statement_procedure_call(yylineno, ast_id($1), $3); }
                ;

io_statement    :   TOKEN_READ TOKEN_LPAREN read_list TOKEN_RPAREN                      { $$ = ast_statement_io_read(yylineno, $3); }
                |   TOKEN_WRITE TOKEN_LPAREN write_list TOKEN_RPAREN                    { $$ = ast_statement_io_write(yylineno, $3); }
                ;

read_list   :   read_list TOKEN_COMMA read_item                                         { LIST_APPEND(Variable, $$, $1, $3); }
            |   read_item                                                               { $$ = $1; }
            ;

read_item   :   variable                                                                { $$ = $1; }
            ;

write_list  :   write_list TOKEN_COMMA write_item                                       { LIST_APPEND(Expression, $$, $1, $3); }
            |   write_item                                                              { $$ = $1; }
            ;

write_item  :   expression                                                              { $$ = $1; }
            ;

%%

static void yyerror(const char* error) {
    
    parser_error(yylineno, error);

}