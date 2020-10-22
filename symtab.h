#ifndef SYMTAB_H
#define SYMTAB_H

#include "uthash/src/uthash.h"

//////////////////////////////
/// Scope related
//////////////////////////////

#define CURRENT_SCOPE   1
#define ALL_SCOPES      2

typedef int SearchScope;

//////////////////////////////
/// Symbol Information
//////////////////////////////

typedef int Line;

typedef enum SymbolTag {
    PROGRAM,
    CONSTANT,
    TYPE,
    ENUMERATION_ELEMENT,
    RECORD_FIELD,
    VARIABLE,
    SUBPROGRAM,
    PARAMETER
} SymbolTag;

static const char* SymbolTagString[] = {
    "PROGRAM",
    "CONSTANT",
    "TYPE",
    "ENUMERATION ELEMENT",
    "RECORD FIELD",
    "VARIABLE",
    "SUBPROGRAM"
    "PARAMETER"
};

typedef struct SymbolType {
    enum {
        UNDEFINED,
        INTEGER,
        REAL,
        BOOLEAN,
        CHARACTER,
        STRING,
        ENUMERATION,
        SUBRANGE,
        ARRAY,
        SET,
        LIST,
        RECORD,
        ID
    } tag;

    Symbol* id;
} SymbolType;

typedef union SymbolValue {
    int integer;
    float real;
    bool boolean;
    char character;
    char* string;
} SymbolValue;

//////////////////////////////
/// Nodes
//////////////////////////////

typedef struct Symbol Symbol;
typedef struct Scope Scope;
typedef struct SymTab SymTab;

typedef struct Symbol {
    const char* identifier;
    Line line;
    SymbolTag tag;

    union {
        /* SYMBOL_CONSTANT */
        struct {
            SymbolType type;
            SymbolValue value;
        } constant;

        /* SYMBOL_TYPE */
        struct {
            SymbolType type;

            union {
                struct {
                    Symbol** elements;
                    int element_count;
                } enumeration;

                struct {
                    SymbolType lower_limit_type;
                    SymbolType upper_limit_type;
                    SymbolValue lower_limit_value;
                    SymbolValue upper_limit_value;
                } subrange;

                struct {
                    int* dims;
                    int dim_count;
                    SymbolType type;
                } array;

                struct {
                    SymbolType type;
                } set;

                struct {
                    SymbolType type;
                } list;

                struct {
                    Symbol** fields;
                    int field_count;
                } record;
            };
        } type;

        /* SYMBOL_ENUMERATION_ELEMENT */
        struct {
            Symbol* enumeration;
            int value;
        } enumeration_element;

        /* SYMBOL_RECORD_FIELD */
        struct {
            Symbol* record;
            SymbolType type;
        } record_field;

        /* SYMBOL_VARIABLE */
        struct {
            SymbolType type;
        } variable;

        /* SYMBOL_SUBPROGRAM */
        struct {
            enum {
                FUNCTION,
                PROCEDURE
            } type;

            Symbol** formal_parameters;
            int formal_parameter_count;
            SymbolType return_type;
            bool defined;
        } subprogram;

        /* SYMBOL_PARAMETER */
        struct {
            Symbol* subprogram;
            
            enum {
                BY_REFERENCE,
                BY_VALUE
            } pass;

            SymbolType type;
        } parameter;
    };

    UT_hash_handle hh;
} Symbol;

typedef struct Scope {
    Symbol* hash_table;
    Scope* previous;
    Scope* next;
} Scope;

typedef struct SymTab {
    Scope* current_scope;
} SymTab;

//////////////////////////////
/// Functions
//////////////////////////////

SymTab* symtab_initialize();
void symtab_finalize(SymTab* symtab);
Scope* symtab_enter_scope(SymTab* symtab);
Scope* symtab_leave_scope(SymTab* symtab);
void symtab_print_scope(SymTab* symtab);
void symtab_dump_scope(SymTab* symtab, const char* filename);
Symbol* symtab_add_symbol(SymTab* symtab, const char* identifier);
Symbol* symtab_search_symbol(SymTab* symtab, SearchScope search_scope, const char* identifier);

#endif