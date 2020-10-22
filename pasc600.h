#ifndef PASC600_H
#define PASC600_H

#include <stdio.h>
#include <stdlib.h>
#include "ast.h"
#include "error.h"

//////////////////////////////
/// Compiler settings
//////////////////////////////

#define DEBUG                       1
#define PARSER_MAX_ERRORS           5
#define STRING_LENGTH_LIMIT         255
#define SUBPROGRAM_NESTING_LIMIT    10

//////////////////////////////
/// Auxiliary Macros
//////////////////////////////

#define MALLOC(x, t)                    \
        x = (t*) malloc(sizeof(t));     \
        if (!x) internal_error("MALLOC"); \

#define LIST_APPEND(type, assignable, list, elem)               \
        if (!list) assignable = elem;                           \
        else {                                                  \
            type* e;                                            \
            for (e = assignable = list; e->next; e = e->next);  \
            e->next = elem;                                     \
        }

//////////////////////////////
/// Custom variables
//////////////////////////////

extern const char* filename;    // defined in 'pasc600.c'
extern AST* ast;                // defined in 'parser.y'

//////////////////////////////
/// Needed for Flex / Bison
//////////////////////////////

extern FILE* yyin;
extern int yylineno;
int yylex();
int yyparse();
int yylex_destroy();

#endif