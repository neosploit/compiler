#ifndef SEM_H
#define SEM_H

// #include "symtab.h"

typedef struct SemanticState {
    SymTab* symtab;
    Symbol* saved_symbol;
    int subprogram_nesting_level;
    bool subprogram_foward;
    int current_parameter;
    Symbol** undefined_subprograms;
    int undefined_subprogram_count;
} SemanticState;

typedef enum SemTypeTag {
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
} SemTypeTag;

typedef struct SemType {
    SemTypeTag tag;

    struct {
        Symbol* symbol;
        SemType symbol_type;
    } id;
} SemType;

typedef union SemValue {
    int integer;
    float real;
    bool boolean;
    char character;
    char* string;
} SemValue;

typedef struct SemExpression {
    SemType type;
    SemValue value;
    // bool boolean_type; Mporei na xreiastei na to energopoihsw giati h ekfrash 1 + 1 einai alhthis alla einai typou integer
    bool constant_value;
} SemExpression;

typedef struct SemVariable {
    Symbol* symbol;
    SemType type;
} SemVariable;

typedef struct SemConstant {
    SemType type;
    SemValue value;
} SemConstant;

void sem_analysis(AST* ast);

#endif