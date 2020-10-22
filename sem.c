#include <stdbool.h>
#include "pasc600.h"
#include "sem.h"
#include "ast.h"
#include "symtab.h"
#include "error.h"

//////////////////////////////
/// Macro functions
//////////////////////////////

#define STANDARD_TYPE(type)     type.tag == INTEGER || type.tag == REAL || type.tag == BOOLEAN || type.tag == CHARACTER || type.tag == STRING
#define COMPLEX_TYPE(type)      type.tag == ARRAY || type.tag == SET || type.tag == LIST || type.tag == RECORD
#define SCALAR_TYPE(type)       type.tag == INTEGER || type.tag == REAL || type.tag == ENUMERATION || type.tag == SUBRANGE
#define NUMERIC_TYPE(type)      type.tag == INTEGER || type.tag == REAL
#define INTEGER_TYPE(type)      type.tag == INTEGER
#define REAL_TYPE(type)         type.tag == REAL
#define BOOLEAN_TYPE(type)      type.tag == BOOLEAN
#define CHARACTER_TYPE(type)    type.tag == CHARACTER
#define STRING_TYPE(type)       type.tag == STRING
#define ENUMERATION_TYPE(type)  type.tag == ENUMERATION
#define SUBRANGE_TYPE(type)     type.tag == SUBRANGE
#define ARRAY_TYPE(type)        type.tag == ARRAY
#define SET_TYPE(type)          type.tag == SET
#define LIST_TYPE(type)         type.tag == LIST
#define RECORD_TYPE(type)       type.tag == RECORD
#define ID_TYPE(type)           type.tag == ID

//////////////////////////////
/// Static functions
//////////////////////////////

static void set_symbol_basic_info(Symbol* symbol, Line line, SymbolTag tag);
static void add_undefined_subprogram(SemanticState* state, Symbol* symbol);
static void remove_undefined_subprogram(SemanticState* state, Symbol* symbol);
static void check_for_undefined_subprograms(SemanticState* state);
static void check_assignment_validation(SemType variable_type, SemType expression_type);

static Symbol* sem_id(SemanticState* state, SearchScope search_scope, Id* id);

static void sem_declaration(SemanticState* state, Declaration* declaration);

static void sem_definition(SemanticState* state, Definition* definition);
static void sem_definition_program(SemanticState* state, Definition* definition);
static void sem_definition_constant(SemanticState* state, Definition* definition);
static void sem_definition_type(SemanticState* state, Definition* definition);
static void sem_definition_field(SemanticState* state, Definition* definition);
static void sem_definition_variable(SemanticState* state, Definition* definition);
static void sem_definition_subprogram_forward(SemanticState* state, Definition* definition);
static void sem_definition_subprogram(SemanticState* state, Definition* definition);
static void sem_definition_function(SemanticState* state, Definition* definition);
static void sem_definition_procedure(SemanticState* state, Definition* definition);
static void sem_definition_parameter(SemanticState* state, Definition* definition);

static SemExpression sem_expression(SemanticState* state, Expression* expression);
static SemExpression sem_expression_binary(SemanticState* state, Expression* expression);
static SemExpression sem_expression_unary(SemanticState* state, Expression* expression);
static SemExpression sem_expression_variable(SemanticState* state, Expression* expression);
static SemExpression sem_expression_function_call(SemanticState* state, Expression* expression);
static SemExpression sem_expression_length(SemanticState* state, Expression* expression);
static SemExpression sem_expression_new(SemanticState* state, Expression* expression);
static SemExpression sem_expression_constant(SemanticState* state, Expression* expression);
static SemExpression sem_expression_enclosed(SemanticState* state, Expression* expression);
static SemExpression sem_expression_setlist(SemanticState* state, Expression* expression);
static SemExpression sem_expression_limits(SemanticState* state, Expression* expression);
static SemExpression sem_expression_limits_id(SemanticState* state, Expression* expression);
static SemExpression sem_expression_iter_space(SemanticState* state, Expression* expression);

static SemVariable sem_variable(SemanticState* state, Variable* variable);
static SemVariable sem_variable_id(SemanticState* state, Variable* variable);
static SemVariable sem_variable_component(SemanticState* state, Variable* variable);
static SemVariable sem_variable_index(SemanticState* state, Variable* variable);
static SemVariable sem_variable_listfunc(SemanticState* state, Variable* variable);

static SemConstant sem_constant(SemanticState* state, Constant* constant);
static SemConstant sem_constant_integer(SemanticState* state, Constant* constant);
static SemConstant sem_constant_real(SemanticState* state, Constant* constant);
static SemConstant sem_constant_boolean(SemanticState* state, Constant* constant);
static SemConstant sem_constant_character(SemanticState* state, Constant* constant);
static SemConstant sem_constant_string(SemanticState* state, Constant* constant);
static SemConstant sem_constant_id(SemanticState* state, Constant* constant);

static SemType sem_type(SemanticState* state, Type* type);
static SemType sem_type_array(SemanticState* state, Type* type);
static SemType sem_type_set(SemanticState* state, Type* type);
static SemType sem_type_list(SemanticState* state, Type* type);
static SemType sem_type_record(SemanticState* state, Type* type);
static SemType sem_type_enumeration(SemanticState* state, Type* type);
static SemType sem_type_subrange(SemanticState* state, Type* type);
static SemType sem_type_integer(SemanticState* state, Type* type);
static SemType sem_type_real(SemanticState* state, Type* type);
static SemType sem_type_boolean(SemanticState* state, Type* type);
static SemType sem_type_character(SemanticState* state, Type* type);
static SemType sem_type_string(SemanticState* state, Type* type);
static SemType sem_type_id(SemanticState* state, Type* type);

static void sem_statement(SemanticState* state, Statement* statement);
static void sem_statement_assignment(SemanticState* state, Statement* statement);
static void sem_statement_if(SemanticState* state, Statement* statement);
static void sem_statement_while(SemanticState* state, Statement* statement);
static void sem_statement_for(SemanticState* state, Statement* statement);
static void sem_statement_with(SemanticState* state, Statement* statement);
static void sem_statement_procedure_call(SemanticState* state, Statement* statement);
static void sem_statement_io_read(SemanticState* state, Statement* statement);
static void sem_statement_io_write(SemanticState* state, Statement* statement);

//////////////////////////////
/// Auxiliary functions
//////////////////////////////

static void set_symbol_basic_info(Symbol* symbol, Line line, SymbolTag tag) {

    symbol->line = line;
    symbol->tag = tag;

}

static void add_undefined_subprogram(SemanticState* state, Symbol* symbol) {

    state->undefined_subprograms = realloc(state->undefined_subprograms, (state->undefined_subprogram_count + 1) * sizeof(Symbol*));
    state->undefined_subprograms[state->undefined_subprogram_count++] = symbol;

}

static void remove_undefined_subprogram(SemanticState* state, Symbol* symbol) {

    assert(state->undefined_subprogram_count);

    for (int i = 0; i < state->undefined_subprogram_count; ++i) {
        if (state->undefined_subprograms[i] == symbol) break;
    }

    assert(i <= state->undefined_subprogram_count);

    Symbol** temp = malloc((state->undefined_subprogram_count - 1) * sizeof(Symbol*));

    if (i) memcpy(temp, state->undefined_subprograms, i * sizeof(Symbol*));
    if (i != (state->undefined_subprogram_count - 1)) memcpy(temp + (i * sizeof(Symbol*)), state->undefined_subprograms + i + 1, (state->undefined_subprogram_count - i - 1) * sizeof(Symbol*));

    free(state->undefined_subprograms);

    state->undefined_subprograms = temp;
    --state->undefined_subprogram_count;

}

static void check_for_undefined_subprograms(SemanticState* state) {

    if (state->undefined_subprogram_count) sem_error(NO_LINE, SEM_DEFINITION_SUBPROGRAM_UNDEFINED, state->undefined_subprograms[0]);

}

static void check_assignment_validation(SemType variable_type, SemType expression_type) {

    if (variable_type == expression_type) return true;
    else if (NUMERIC_TYPE(variable_type) && NUMERIC_TYPE(expression_type)) return true;
    else if (ID_TYPE(variable_type) && ID_TYPE(expression_type)) {
        if (ENUMERATION_TYPE(variable_type.id.symbol->type.type) && ENUMERATION_TYPE(expression_type.id.symbol->type.type) )
    }

}

//////////////////////////////
/// Id
//////////////////////////////

static Symbol* sem_id(SemanticState* state, SearchScope search_scope, Id* id) {

    Symbol* symbol = symtab_search_symbol(state->symtab, search_scope, id->identifier);

    if (state->saved_symbol && state->saved_symbol == symbol) sem_error(id->line, SEM_STATEMENT_FOR_ITERATOR_USAGE_RESTRICTED, symbol->identifier);

    return symbol;

}

//////////////////////////////
/// AST
//////////////////////////////

void sem_analysis(AST* ast) {

    SemanticState state = { .symtab = symtab_initialize(),
                            .saved_symbol = NULL,
                            .subprogram_nesting_level = 0,
                            .subprogram_forward = false,
                            .current_parameter = 0,
                            .undefined_subprograms = NULL,
                            .undefined_subprogram_count = 0 };

    symtab_enter_scope(state.symtab);

    /* Definition: header */

    sem_definition(&state, ast->header);

    /* Declaration: declarations */

    sem_declaration(&state, ast->declarations);

    /* Definition: subprograms */

    if (ast->subprograms) {
        sem_definition(&state, ast->subprograms);
        check_for_undefined_subprograms(&state);
    }

    /* Statement: comp_statement */

    if (ast->comp_statement) sem_statement(&state, ast->comp_statement);

}

//////////////////////////////
/// Declaration
//////////////////////////////

static void sem_declaration(SemanticState* state, Declaration* declaration) {

    /* Definition: constdefs */

    if (declaration->constdefs) sem_definition(state, declaration->constdefs);

    /* Definition: typedefs */

    if (declaration->typedefs) sem_definition(state, declaration->typedefs);

    /* Definition: vardefs */

    if (declaration->vardefs) sem_definition(state, declaration->vardefs);

}

//////////////////////////////
/// Definition
//////////////////////////////

static void sem_definition(SemanticState* state, Definition* definition) {

    switch (definition->tag) {
        case DEFINITION_PROGRAM:
            sem_definition_program(state, definition);
            break;
        case DEFINITION_CONSTANT:
            sem_definition_constant(state, definition);
            break;
        case DEFINITION_TYPE:
            sem_definition_type(state, definition);
            break;
        case DEFINITION_FIELD:
            sem_definition_field(state, definition);
            break;
        case DEFINITION_VARIABLE:
            sem_definition_variable(state, definition);
            break;
        case DEFINITION_SUBPROGRAM_FORWARD:
            sem_definition_subprogram_forward(state, definition);
            break;
        case DEFINITION_SUBPROGRAM:
            sem_definition_subprogram(state, definition);
            break;
        case DEFINITION_FUNCTION:
            sem_definition_function(state, definition);
            break;
        case DEFINITION_PROCEDURE:
            sem_definition_procedure(state, definition);
            break;
        case DEFINITION_PARAMETER:
            sem_definition_parameter(state, definition);
    }

}

static void sem_definition_program(SemanticState* state, Definition* definition) {

    /* Id: id */

    Symbol* symbol = sem_id(state, CURRENT_SCOPE, definition->program.id);

    if (symbol) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);

    symbol = symtab_add_symbol(state->symtab, definition->program.id->identifier);
    set_symbol_basic_info(symbol, definition->line, PROGRAM);

}

static void sem_definition_constant(SemanticState* state, Definition* definition) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, definition->constant.id);

    if (symbol) sem_error(definition->linem, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);

    symbol = symtab_add_symbol(state->symtab, definition->constant.id->identifier);
    set_symbol_basic_info(symbol, definition->line, CONSTANT);

    /* Expression: expression */

    SemExpression sem_expression_ = sem_expression(state, definition->constant.expression);

    if (!sem_expression_.constant_value) sem_error(definition->line, SEM_DEFINITION_INVALID_CONSTANT_EXPRESSION, definition->constant.id->identifier);

    symbol->constant.type = sem_expression_.type;
    symbol->constant.value = sem_expression_.value;

    /* Linked Definitions */

    for (definition = definition->next; definition; definition = definition->next) sem_definition(state, definition);

}

static void sem_definition_type(SemanticState* state, Definition* definition) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, definition->type.id);

    if (symbol) sem_error(definition->linem, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);

    symbol = symtab_add_symbol(state->symtab, definition->type.id->identifier);
    set_symbol_basic_info(symbol, definition->line, TYPE);

    /* Type: type */

    state->saved_symbol = symbol;
    sem_type(state, definition->type.type);
    state->saved_symbol = NULL;

    /* Linked Definitions */

    for (definition = definition->next; definition; definition = definition->next) sem_definition(state, definition);

}

static void sem_definition_field(SemanticState* state, Definition* definition) {

    /* Type: type */

    SemType sem_type_ = sem_type(state, definition->field.type);

    /* Id: identifiers */

    for (Id* id = definition->field.identifiers; id; id = id->next) {
        Symbol* symbol = sem_id(state, CURRENT_SCOPE, id);

        if (symbol) sem_error(type->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);

        symbol = symtab_add_symbol(state->symtab, id->identifier);
        set_symbol_basic_info(symbol, definition->line, RECORD_FIELD);

        symbol->record_field.record = state->saved_symbol;
        symbol->record_field.type = sem_type_;

        state->saved_symbol->type.record.fields = realloc(state->saved_symbol->type.record.fields, (state->saved_symbol->type.record.field_count + 1) * sizeof(Symbol*));
        state->saved_symbol->type.record.fields[state->saved_symbol->type.record.field_count++] = symbol;
    }

    /* Linked Definitions */

    for (definition = definition->next; definition; definition = definition->next) sem_definition(state, definition);

}

static void sem_definition_variable(SemanticState* state, Definition* definition) {

    /* Type: type */

    SemType sem_type_ = sem_type(state, definition->variable.type);

    /* Id: identifiers */

    for (Id* id = definition->variable.identifiers; id; id = id->next) {
        Symbol* symbol = sem_id(state, CURRENT_SCOPE, id);

        if (symbol) sem_error(type->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);

        symbol = symtab_add_symbol(state->symtab, id->identifier);
        set_symbol_basic_info(symbol, definition->line, VARIABLE);

        symbol->variable.type = sem_type_;
    }

    /* Linked Definitions */

    for (definition = definition->next; definition; definition = definition->next) sem_definition(state, definition);

}

static void sem_definition_subprogram_forward(SemanticState* state, Definition* definition) {

    /* Definition: sub_header */

    state->subprogram_foward = true;
    sem_definition(state, definition->subprogram_forward.sub_header);
    state->subprogram_foward = false;

    /* Linked Definitions */

    for (definition = definition->next; definition; definition = definition->next) sem_definition(state, definition);

}

static void sem_definition_subprogram(SemanticState* state, Definition* definition) {

    if (++state->subprogram_nesting_level > SUBPROGRAM_NESTING_LIMIT) sem_error(definition->line, SEM_DEFINITION_SUBPROGRAM_NESTING_LIMIT_EXCEEDED, SUBPROGRAM_NESTING_LIMIT);

    /* Enter scope */

    symtab_enter_scope(state->symtab);

    /* Definition: sub_header */

    sem_definition(state, definition->subprogram.sub_header);

    /* Declaration: declarations */

    sem_declaration(state, definition->subprogram.declarations);

    /* Definition: subprograms */

    if (definition->subprogram.subprograms) {
        sem_definition(state, definition->subprogram.subprograms);
        check_for_undefined_subprograms(state);
    }

    /* Statement: comp_statement */

    if (state, definition->subprogram.comp_statement) sem_statement(state, definition->subprogram.comp_statement);

    /* Leave scope */

    symtab_leave_scope(state->symtab);

    --state->subprogram_nesting_level;

    /* Linked Definitions */

    for (definition = definition->next; definition; definition = definition->next) sem_definition(state, definition);

}

static void sem_definition_function(SemanticState* state, Definition* definition) {

    /* Id: id */
    
    Symbol* symbol = sem_id(state, CURRENT_SCOPE, definition->function.id);

    if (!symbol) {
        symbol = symtab_add_symbol(state->symtab, definition->function.id->identifier);
        set_symbol_basic_info(symbol, definition->line, SUBPROGRAM);

        symbol->subprogram.type = FUNCTION;
        symbol->subprogram.formal_parameters = NULL;
        symbol->subprogram.formal_parameter_count = 0;
        symbol->subprogram.return_type = UNDEFINED;
        symbol->subprogram.defined = !state->subprogram_foward;
    } else {
        if (symbol->tag != SUBPROGRAM) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
        if (symbol->subprogram.type != FUNCTION) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
        if (state->subprogram_foward) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
        if (symbol->subprogram.defined) sem_error(definition->line, SEM_DEFINITION_SUBPROGRAM_REDEFINITION, symbol->identifier, symbol->line);
    }

    /* Definition: formal_parameters */

    if (!state->subprogram_forward && !symbol->subprogram.defined && !definition->function.formal_parameters) sem_error(definition->line, SEM_DEFINITION_FUNCTION_PARAMETER_COUNT, symbol->identifier);
    if (!definition->function.formal_parameters) sem_error(definition->line, SEM_DEFINITION_FUNCTION_NO_PARAMETERS, definition->function.id->identifier);

    state->saved_symbol = symbol;
    state->current_parameter = 0;

    sem_definition(state, definition->function.formal_parameters);
    if (!state->subprogram_forward && !symbol->subprogram.defined && state->current_parameter < symbol->subprogram.formal_parameter_count) sem_error(definition->line,);

    state->saved_symbol = NULL;
    state->current_parameter = 0;

    /* Type: type */

    SemType sem_type_ = sem_type(state, definition->function.type);

    if (!state->subprogram_forward && !symbol->subprogram.defined && sem_type_ != symbol->subprogram.type) sem_error(definition->line);
    if (STRING_TYPE(sem_type_) || ID_TYPE(sem_type_) && !LIST_TYPE(sem_type_.id.symbol_type)) sem_error(definition->line, SEM_DEFINITION_FUNCTION_INVALID_RETURN_TYPE, symbol->identifier);

    symbol->subprogram.type = sem_type_;

    /* body */

    if (state->subprogram_foward) {
        add_undefined_subprogram(state, symbol);
    } else if (!symbol->subprogram.defined) {
        symbol->subprogram.defined = true;
        remove_undefined_subprogram(state, symbol);
    }

}

static void sem_definition_procedure(SemanticState* state, Definition* definition) {

    /* Id: id */

    Symbol* symbol = sem_id(state, CURRENT_SCOPE, definition->procedure.id);

    if (!symbol) {
        symbol = symtab_add_symbol(state->symtab, definition->procedure.id->identifier);
        set_symbol_basic_info(symbol, definition->line, SUBPROGRAM);

        symbol->subprogram.type = PROCEDURE;
        symbol->subprogram.formal_parameters = NULL;
        symbol->subprogram.formal_parameter_count = 0;
        symbol->subprogram.defined = !state->subprogram_foward;
    } else {
        if (symbol->tag != SUBPROGRAM) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
        if (symbol->subprogram.type != PROCEDURE) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
        if (state->subprogram_foward) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
        if (symbol->subprogram.defined) sem_error(definition->line, SEM_DEFINITION_SUBPROGRAM_REDEFINITION, symbol->identifier, symbol->line);
    }

    /* Definition: formal_parameters */

    if (!state->subprogram_forward && !symbol->subprogram.defined) {
        if (definition->procedure.formal_parameters && !symbol->subprogram.formal_parameter_count) sem_error(definition->line, SEM_DEFINITION_PROCEDURE_PARAMETER_COUNT_1, symbol->identifier);
        if (!definition->procedure.formal_parameters && symbol->subprogram.formal_parameter_count) sem_error(definition->line, SEM_DEFINITION_PROCEDURE_PARAMETER_COUNT_2, symbol->identifier);
    }

    if (definition->procedure.formal_parameters) {
        state->saved_symbol = symbol;
        state->current_parameter = 0;

        sem_definition(state, definition->procedure.formal_parameters);
        if (!state->subprogram_forward && !symbol->subprogram.defined && state->current_parameter < symbol->subprogram.formal_parameter_count) sem_error(definition->line,);

        state->saved_symbol = NULL;
        state->current_parameter = 0;
    }

    /* body */

    if (state->subprogram_foward) {
        add_undefined_subprogram(state, symbol);
    } else if (!symbol->subprogram.defined) {
        symbol->subprogram.defined = true;
        remove_undefined_subprogram(state, symbol);
    }

}

static void sem_definition_parameter(SemanticState* state, Definition* definition) {

    /* Type: type */

    SemType sem_type_ = sem_type(state, definition->parameter.type);

    if (STRING_TYPE(sem_type_) || ID_TYPE(sem_type_) && COMPLEX_TYPE(sem_type_.id.symbol_type) && definition->parameter.pass != BY_REFERENCE) sem_error(definition->line, SEM_DEFINITION_PARAMETER_INVALID_PASS);

    /* Id: identifiers */

    for (Id* id = definition->parameter.identifiers; id; id = id->next) {
        Symbol* symbol = sem_id(state, CURRENT_SCOPE, id->identifier);
        
        if (state->subprogram_foward || state->saved_symbol.subprogram.defined) {
            if (symbol) sem_error(definition->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);

            symbol = symtab_add_symbol(state->symtab, id->identifier);
            set_symbol_basic_info(symbol, definition->line, PARAMETER);

            symbol->parameter.subprogram = saved_symbol;
            symbol->parameter.pass = (definition->parameter.pass == BY_REFERENCE) ? BY_REFERENCE : BY_VALUE;
            symbol->parameter.type = sem_type_;

            state->saved_symbol->subprogram.formal_parameters = realloc(state->saved_symbol->subprogram.formal_parameters, (state->saved_symbol->subprogram.formal_parameter_count + 1) * sizeof(Symbol*));
            state->saved_symbol->subprogram.formal_parameters[state->saved_symbol->subprogram.formal_parameter_count++] = symbol;
        } else {
            if (!symbol) sem_error(definition->line, SEM_DEFINITION_PARAMETER_ID_UNDECLARED, state->saved_symbol->identifier, state->current_parameter + 1, id->identifier);
            if (symbol->tag != PARAMETER) sem_error(definition->line, );
            if (symbol->parameter.subprogram != state->saved_symbol) sem_error(definition->line,);
            if (state->current_parameter == state->saved_symbol->subprogram.formal_parameter_count) sem_error(definition->line);
            if (symbol != state->saved_symbol->subprogram.formal_parameters[state->current_parameter]) sem_error(definition->line);
            if (definition->parameter.pass != symbol->parameter.pass) sem_error(definition->line, SEM_DEFINITION_PARAMETER_DIFFERENT_PASS, id->identifier);
            if (definition->parameter.type != symbol->parameter.type) sem_error(definition->line, SEM_DEFINITION_PARAMETER_DIFFERENT_TYPE, id->identifier);
            state->current_parameter++;
        }
    }

    /* Linked Definitions */

    for (definition = definition->next; definition; definition = definition->next) sem_definition(state, definition);

}

//////////////////////////////
/// Expression
//////////////////////////////

static SemExpression sem_expression(SemanticState* state, Expression* expression) {

    switch (expression->tag) {
        case EXPRESSION_BINARY:
            return sem_expression_binary(state, expression);
        case EXPRESSION_UNARY:
            return sem_expression_unary(state, expression);
        case EXPRESSION_VARIABLE:
            return sem_expression_variable(state, expression);
        case EXPRESSION_FUNCTION_CALL:
            return sem_expression_function_call(state, expression);
        case EXPRESSION_LENGTH:
            return sem_expression_length(state, expression);
        case EXPRESSION_NEW:
            return sem_expression_new(state, expression);
        case EXPRESSION_CONSTANT:
            return sem_expression_constant(state, expression);
        case EXPRESSION_ENCLOSED:
            return sem_expression_enclosed(state, expression);
        case EXPRESSION_SETLIST:
            return sem_expression_setlist(state, expression);
        case EXPRESSION_LIMITS:
            return sem_expression_limits(state, expression);
        case EXPRESSION_LIMITS_ID:
            return sem_expression_limits_id(state, expression);
        case EXPRESSION_ITER_SPACE:
            return sem_expression_iter_space(state, expression);
    }

}

static SemExpression sem_expression_binary(SemanticState* state, Expression* expression) {

    SemExpression sem_expression_final;

    /* Expression: left_expression */

    SemExpression left_expression = sem_expression(state, expression->binary.left_expression);

    /* Expression: right_expression */

    SemExpression right_expression = sem_expression(state, expression->binary.right_expression);

    /* Operator: operator */

    switch (expression->binary.operator) {
        case UNDEFINED:
            break;
        case GREATER:
        case GREATER_EQUAL:
        case LESS:
        case LESS_EQUAL:
        case NOT_EQUAL:
        case EQUAL:
            sem_expression_final.type.tag = BOOLEAN;
            break;
        case PLUS:
        case MINUS:
        case MUL:
        case DIV:
        case KW_DIV:
        case KW_MOD:
            if (NUMERIC_TYPE(left_expression.type) && NUMERIC_TYPE(right_expression.type))) {
                if (INTEGER_TYPE(left_expression.type)) {
                    if (INTEGER_TYPE(right_expression.type)) {
                        sem_expression_final.type.tag = (expression->binary.operator == DIV) ? REAL : INTEGER;
                    } else {
                        if (expression->binary.operator == KW_DIV || expression->binary.operator == KW_MOD) sem_error(expression->line, );
                        sem_expression_final.type.tag = REAL;
                    }
                } else {
                    if (expression->binary.operator == KW_DIV || expression->binary.operator == KW_MOD) sem_error(expression->line, );
                    sem_expression_final.type.tag = REAL;
                }
            } else if (SET_TYPE(left_expression.type) && SET_TYPE(right_expression.type)) {
                if (expression->binary.operator != PLUS && expression->binary.operator != MINUS && expression->binary.operator != MUL) sem_error(expression->line);
                sem_expression_final.type.tag = SET;
            } else if (LIST_TYPE(left_expression.type) && LIST_TYPE(right_expression.type)) {
                if (expression->binary.operator != PLUS) sem_error(expression->line);
                sem_expression_final.type.tag = LIST;
            } else if (STRING_TYPE(left_expression.type) && STRING_TYPE(right_expression.type)) {
                if (expression->binary.operator != PLUS) sem_error(expression->line);
                sem_expression_final.type.tag = STRING;
            }

            break;
        case KW_OR:
            if (!BOOLEAN_TYPE(left_expression.type) || !BOOLEAN_TYPE(right_expression.type)) sem_error(expression->line);
            sem_expression_final.type.tag = BOOLEAN;

            break;
        case KW_AND:
        case KW_NOT:
        case KW_IN:
    }

    /* return */

    sem_expression_final.constant_value = false;

    return sem_expression_final;

}

static SemExpression sem_expression_unary(SemanticState* state, Expression* expression) {

    SemExpression sem_expression_final;

    /* Expression: expression */

    SemExpression sem_expression_temp = sem_expression(state, expression->unary.expression);

    /* Token: operator */

    switch (expression->unary.operator) {
        case PLUS:
        case MINUS:
            if (!NUMERIC_TYPE(sem_expression_temp.type)) sem_error(expression->line);
            sem_expression_final.type.tag = sem_expression_temp.type;

            break;
        case KW_NOT:
            if (!BOOLEAN_TYPE(sem_expression_temp.type)) sem_error(expression->line);
            sem_expression_final.type.tag = BOOLEAN;
    }

    /* return */

    sem_expression_final.constant_value = false;

    return sem_expression_final;

}

static SemExpression sem_expression_variable(SemanticState* state, Expression* expression) {

    /* Variable: variable */

    SemVariable sem_variable_temp = sem_variable(state, expression->variable.variable);

    /* return */

    SemExpression sem_expression_final = { .type = sem_variable_temp.type, .constant_value = false };

    return sem_expression_final;

}

static SemExpression sem_expression_function_call(SemanticState* state, Expression* expression) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, expression->function_call.id);

    if (!symbol) sem_error(expression->line, SEM_ID_UNDECLARED, expression->function_call.id->identifier);
    if (symbol->tag != SUBPROGRAM) sem_error(statement->line, SEM_EXPRESSION_FUNCTION_CALL_NOT_SUBPROGRAM, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
    if (symbol->subprogram.type != FUNCTION) sem_error(statement->line, SEM_EXPRESSION_FUNCTION_CALL_NOT_FUNCTION, symbol->identifier);

    /* Expression: expressions */

    Expression* expression_;
    int i;

    for (expression_ = expression->function_call.expressions, i = 0; expression_ && i < symbol->subprogram.formal_parameter_count; expression_ = expression_->next, ++i) {
        SemExpression sem_expression_temp = sem_expression(state, expression_);
        Symbol* formal_parameter = symbol->subprogram.formal_parameters[i];
    }

    if (expression) sem_error(expression->line);
    if (i < symbol->subprogram.formal_parameter_count) sem_error(expression->line);

    /* return */

    SemExpression sem_expression_final = { .type = symbol->subprogram.return_type, .constant_value = false };

    return sem_expression_final;

}

static SemExpression sem_expression_length(SemanticState* state, Expression* expression) {

    /* Expression: expression */

    SemExpression sem_expression_temp = sem_expression(state, expression->length.expression);

    if (!LIST_TYPE(sem_expression_temp.type) && !STRING_TYPE(sem_expression_temp.type)) sem_error(expression->line, SEM_EXPRESSION_LENGTH_INVALID_PARAMETER_TYPE);

    /* return */

    SemExpression sem_expression_final = { .type = INTEGER, .constant_value = false };

    return sem_expression_final;

}

static SemExpression sem_expression_new(SemanticState* state, Expression* expression) {

    /* Expression: expression */

    SemExpression sem_expression_temp = sem_expression(state, expression->new.expression);

    /* return */

    SemExpression sem_expression_final = { .type = LIST, .constant_value = false };

    return sem_expression_final;

}

static SemExpression sem_expression_constant(SemanticState* state, Expression* expression) {

    /* Constant: constant */

    SemConstant sem_constant = sem_constant(state, expression->constant.constant);

    /* return */

    SemExpression sem_expression_final = { .type = sem_constant.type, .value = sem_constant.value, .constant_value = true };

    return sem_expression_final;

}

static SemExpression sem_expression_enclosed(SemanticState* state, Expression* expression) {

    /* Expression: expression */
    
    /* return */

    return sem_expression(state, expression->enclosed.expression);

}

static SemExpression sem_expression_setlist(SemanticState* state, Expression* expression) {

    /* Expression: expression */

    sem_expression(state, expression->setlist.expression);

}

static SemExpression sem_expression_limits(SemanticState* state, Expression* expression) {

    /* Constant: lower_limit */

    SemConstant lower_limit = sem_constant(state, expression->limits.lower_limit);

    /* Constant: upper_limit */

    SemConstant upper_limit = sem_constant(state, expression->limits.upper_limit);

}

static SemExpression sem_expression_limits_id(SemanticState* state, Expression* expression) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, expression->limits_id.id);

    if (!symbol) sem_error(expression->line, SEM_ID_UNDECLARED, expression->limits_id.id->identifier);
    if (symbol->tag != CONSTANT) sem_error(expression->line);

}

static SemExpression sem_expression_iter_space(SemanticState* state, Expression* expression) {

    /* Expression: initial_expression */

    SemExpression initial_expression = sem_expression(state, expression->iter_space.initial_expression);

    if (!INTEGER_TYPE(initial_expression.type)) sem_error(expression->line, SEM_EXPRESSION_ITER_SPACE_INITIAL_EXPRESSION_TYPE);

    /* Expression: final_expression */

    SemExpression final_expression = sem_expression(state, expression->iter_space.final_expression);

    if (!INTEGER_TYPE(final_expression.type)) sem_error(expression->line, SEM_EXPRESSION_ITER_SPACE_FINAL_EXPRESSION_TYPE);

    /* return */

    SemExpression sem_expression_final = { .type.tag = INTEGER, .constant_value = false };

    return sem_expression_final;

}

//////////////////////////////
/// Variable
//////////////////////////////

static void sem_variable(SemanticState* state, Variable* variable) {

    switch (variable->tag) {
        case VARIABLE_ID:
            sem_variable_id(state, variable);
            break;
        case VARIABLE_COMPONENT:
            sem_variable_component(state, variable);
            break;
        case VARIABLE_INDEX:
            sem_variable_index(state, variable);
            break;
        case VARIABLE_LISTFUNC:
            sem_variable_listfunc(state, variable);
    }

}

static SemVariable sem_variable_id(SemanticState* state, Variable* variable) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, variable->id.id);

    if (!symbol) sem_error(variable->line, SEM_ID_UNDECLARED, variable->id.id->identifier);
    if (symbol->tag != VARIABLE) sem_error(variable->line);

    /* return */

    SemVariable sem_variable_final = { .symbol = symbol };

}

static SemVariable sem_variable_component(SemanticState* state, Variable* variable) {

    /* Variable: variable */

    SemVariable sem_variable_temp = sem_variable(state, variable->component.variable);

    if (!RECORD_TYPE(sem_variable_temp.symbol->variable.type)) sem_error(variable->line);

    /* TODO:
    #define RECORD_TYPE(type)       type.tag == RECORD ->
    #define RECORD_TYPE(type)       type.tag == RECORD || (type.tag == ID && RECORD_TYPE(type.id.symbol->type.type))

    OR

    ID_RECORD_TYPE

    /*

    /* Id: id */

    Symbol* symbol = sem_id(state, CURRENT_SCOPE, variable->component.id);

    if (!symbol) sem_error(variable->line);
    if (symbol->tag != RECORD_FIELD) sem_error(variable->line);
    if (symbol->record_field.record)

    for (int i = 0; i < symbol->record_field.record->type.record.field_count)

}

static SemVariable sem_variable_index(SemanticState* state, Variable* variable) {

    /* Variable: variable */

    SemVariable sem_variable_temp = sem_variable(state, variable->index.variable);

    if (!ARRAY_TYPE(sem_variable_temp.symbol->variable.type)) sem_error(variable->line);

    /* Expression: expressions */

    Expression* expression;
    int i;

    for (expression = variable->index.expressions, i = 0; expression && i < sem_variable_temp.symbol->variable.type.id->type.array.dim_count; expression = expression->next, ++i) {
        SemExpression sem_expression_temp = sem_expression(state, expression);

        /* TODO: SCALAR_TYPE */
        if (!SCALAR_TYPE(sem_expression_temp.type) || REAL_TYPE(sem_expression_temp.type)) sem_error(variable->line);
    }

    if (expression) sem_error(variable->line);
    if (i) sem_error(variable->line);

    /* return */

    SemVariable sem_variable_final = { .symbol = sem_variable_temp.symbol, .type = sem_variable_temp.symbol->variable.type.id->type.array.type };

    return sem_variable_final;

}

static SemVariable sem_variable_listfunc(SemanticState* state, Variable* variable) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, variable->listfunc.id);

    if (!symbol) sem_error(variable->line, SEM_ID_UNDECLARED, variable->listfunc.id->identifier);

    /* Expression: expression */

    sem_expression(state, variable->listfunc.expression);

}

//////////////////////////////
/// Constant
//////////////////////////////

static SemConstant sem_constant(SemanticState* state, Constant* constant) {

    switch (constant->tag) {
        case CONSTANT_INTEGER:
            return sem_constant_integer(state, constant);
        case CONSTANT_REAL:
            return sem_constant_real(state, constant);
        case CONSTANT_BOOLEAN:
            return sem_constant_boolean(state, constant);
        case CONSTANT_CHARACTER:
            return sem_constant_character(state, constant);
        case CONSTANT_STRING:
            return sem_constant_string(state, constant);
        case CONSTANT_ID:
            return sem_constant_id(state, constant);
    }

}

static SemConstant sem_constant_integer(SemanticState* state, Constant* constant) {

    SemConstant sem_constant_final = { .type.tag = INTEGER, .value.integer = constant->integer.integer };

    /* Operator: operator */

    if (constant->integer.operator == MINUS) sem_constant_final.value.integer *= -1;

    /* return */

    return sem_constant_final;

}

static SemConstant sem_constant_real(SemanticState* state, Constant* constant) {

    /* return */

    SemConstant sem_constant_final = { .type.tag = REAL, .value.real = constant->real };

    return sem_constant_final;

}

static SemConstant sem_constant_boolean(SemanticState* state, Constant* constant) {

    /* return */

    SemConstant sem_constant_final = { .type.tag = BOOLEAN, .value.boolean = constant->boolean };

    return sem_constant_final;

}

static SemConstant sem_constant_character(SemanticState* state, Constant* constant) {

    /* return */

    SemConstant sem_constant_final = { .type.tag = CHARACTER, .value.character = constant->character };

    return sem_constant_final;

}

static SemConstant sem_constant_string(SemanticState* state, Constant* constant) {

    if (strlen(constant->string) > 255) sem_error(constant->line, SEM_CONSTANT_STRING_LENGTH_LIMIT_EXCEEDED, strlen(constant->string), STRING_LENGTH_LIMIT);

    /* return */

    SemConstant sem_constant_final = { .type.tag = STRING, .value.string = strdup(constant->string) };

    return sem_constant_final;

}

static SemConstant sem_constant_id(SemanticState* state, Constant* constant) {

    SemConstant sem_constant_final = { .type.tag = ID };

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, constant->id.id);

    if (!symbol) sem_error(constant->line, SEM_ID_UNDECLARED, constant->id.id->identifier);
    if (symbol->tag != CONSTANT) sem_error(constant->line, );

    sem_constant_final.type.id.symbol = symbol;
    sem_constant_final.type.id.symbol_type = symbol->constant.type;
    sem_constant_final.value = symbol->constant.value;

    /* Operator: operator */

    if (constant->id.operator == MINUS) {
        
    }

    /* return */

    return sem_constant_final;

}

//////////////////////////////
/// Type
//////////////////////////////

static SemType sem_type(SemanticState* state, Type* type) {

    switch (type->tag) {
        case TYPE_ARRAY:
            return sem_type_array(state, type);
        case TYPE_SET:
            return sem_type_set(state, type);
        case TYPE_LIST:
            return sem_type_list(state, type);
        case TYPE_RECORD:
            return sem_type_record(state, type);
        case TYPE_ENUMERATION:
            return sem_type_enumeration(state, type);
        case TYPE_SUBRANGE:
            return sem_type_subrange(state, type);
        case TYPE_INTEGER:
            return sem_type_integer(state, type);
        case TYPE_REAL:
            return sem_type_real(state, type);
        case TYPE_BOOLEAN:
            return sem_type_boolean(state, type);
        case TYPE_CHARACTER:
            return sem_type_character(state, type);
        case TYPE_STRING:
            return sem_type_string(state, type);
        case TYPE_ID:
            return sem_type_id(state, type);
    }

}

static SemType sem_type_array(SemanticState* state, Type* type) {

    /* Expression: dims */

    int dim_count = 0;

    for (Expression* expression = type->array.dims; expression; expression = expression->next, ++dim_count) {
        sem_expression(state, expression);
    }

    /* Type: type */

    SemType sem_type_temp = sem_type(state, type->array.type);

    /* return */

    state->saved_symbol->type.type.tag = ARRAY;
    state->saved_symbol->type.array.dim_count = dim_count;
    state->saved_symbol->type.array.type = sem_type_temp;

    SemType sem_type_final = { .tag = ARRAY };

    return sem_type_final;

}

static SemType sem_type_set(SemanticState* state, Type* type) {

    state->saved_symbol.type.type.tag = SET;

    /* Type: type */

    SemType sem_type_temp = sem_type(state, type->set.type);

    if (STANDARD_TYPE(sem_type_temp) && INTEGER_TYPE(sem_type_temp) || REAL_TYPE(sem_type_temp) || STRING_TYPE(sem_type_temp)) sem_error(type->line);
    else if (ID_TYPE(sem_type_temp) && !ENUMERATION_TYPE(sem_type_temp.id.symbol_type) || !SUBRANGE_TYPE(sem_type_temp.id.symbol_type)) sem_error(type->line);

    state->saved_symbol.type.set.type = sem_type_temp;

    /* return */

    SemType sem_type_final = { .tag = SET };

    return sem_type_final;

}

static SemType sem_type_list(SemanticState* state, Type* type) {

    state->saved_symbol->type.type.tag = LIST;

    /* Type: type */

    SemType sem_type_temp = sem_type(state, type->list.type);

    if (STANDARD_TYPE(sem_type_temp) && STRING_TYPE(sem_type_temp)) sem_error(type->line,);
    else if (ID_TYPE(sem_type_temp) && ARRAY_TYPE(sem_type_temp.id.symbol_type) || SET_TYPE(sem_type.id.symbol_type)) sem_error(type->line);

    /* return */

    SemType sem_type_final = { .tag = LIST };

    return sem_type_final;

}

static SemType sem_type_record(SemanticState* state, Type* type) {

    state->saved_symbol->type.type.tag = RECORD;
    state->saved_symbol->type.record.fields = NULL;
    state->saved_symbol->type.record.field_count = 0;

    /* Definition: fields */

    for (Definition* definition = type->record.fields; definition; definition = definition->next) sem_definition(state, definition);

    /* return */

    SemType sem_type_final = { .tag = RECORD };

    return sem_type_final;

}

static SemType sem_type_enumeration(SemanticState* state, Type* type) {

    state->saved_symbol->type.type.tag = ENUMERATION;
    state->saved_symbol->type.enumeration.elements = NULL;
    state->saved_symbol->type.enumeration.element_count = 0;

    /* Id: identifiers */

    for (Id* id = type->enumeration.identifiers; id; id = id->next) {
        Symbol* symbol = sem_id(state, CURRENT_SCOPE, id);

        if (!symbol) {
            symbol = symtab_add_symbol(state->symtab, id->identifier);
            set_symbol_basic_info(symbol, type->line, ENUMERATION_ELEMENT);

            symbol->enumeration_element.enumeration = state->saved_symbol;
            symbol->enumeration_element.value = state->saved_symbol->type.enumeration.element_count;

            state->saved_symbol->type.enumeration.elements = realloc(state->saved_symbol->type.enumeration.elements, (state->saved_symbol->type.enumeration.element_count + 1) * sizeof(Symbol*));
            state->saved_symbol->type.enumeration.elements[state->saved_symbol->type.enumeration.element_count++] = symbol;
        } else {
            sem_error(type->line, SEM_ID_REDECLARATION, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
        }
    }

    /* return */

    SemType sem_type_final = { .tag = ENUMERATION };

    return sem_type_final;

}

static SemType sem_type_subrange(SemanticState* state, Type* type) {

    /* Constant: lower_limit */

    SemConstant lower_limit = sem_constant(state, type->subrange.lower_limit);

    /* Constant: upper_limit */

    SemConstant upper_limit = sem_constant(state, type->subrange.upper_limit);

    /* body */

    switch (lower_limit.type) {
        case INTEGER:
            if (lower_limit.value.integer > upper_limit.value.integer) sem_error(type->line);
            break;
        case CHARACTER:
            if (lower_limit.value.character > upper_limit.value.character) sem_error(type->line);
            break;
        case BOOLEAN:
            if (lower_limit.value.boolean > upper_limit.value.boolean) sem_error(type->line);
            break;
        case ID:
    }

    /* return */

    state->saved_symbol->type.type.tag = SUBRANGE;
    state->saved_symbol->type.subrange.lower_limit_type = lower_limit.type;
    state->saved_symbol->type.subrange.lower_limit_value = lower_limit.value;
    state->saved_symbol->type.subrange.upper_limit_value = upper_limit.value;

    SemType sem_type_final = { .tag = SUBRANGE };

    return sem_type_final;

}

static SemType sem_type_integer(SemanticState* state, Type* type) {

    /* return */

    SemType sem_type_final = { .tag = INTEGER };

    return sem_type_final;

}

static SemType sem_type_real(SemanticState* state, Type* type) {

    /* return */

    SemType sem_type_final = { .tag = REAL };

    return sem_type_final;

}

static SemType sem_type_boolean(SemanticState* state, Type* type) {

    /* return */

    SemType sem_type_final = { .tag = BOOLEAN };

    return sem_type_final;

}

static SemType sem_type_character(SemanticState* state, Type* type) {

    /* return */

    SemType sem_type_final = { .tag = CHARACTER };

    return sem_type_final;

}

static SemType sem_type_string(SemanticState* state, Type* type) {

    /* return */

    SemType sem_type_final = { .tag = STRING };

    return sem_type_final;

}

static SemType sem_type_id(SemanticState* state, Type* type) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, type->id.id);

    if (!symbol) sem_error(type->line, SEM_ID_UNDECLARED, type->id.id->identifier);

    /* return */

    SemType sem_type_final = { .tag = ID, .id.symbol = symbol, .id.symbol_type = symbol->type.type.tag };

    return sem_type_final;

}

//////////////////////////////
/// Statement
//////////////////////////////

static void sem_statement(SemanticState* state, Statement* statement) {

    switch (statement->tag) {
        case STATEMENT_ASSIGNMENT:
            sem_statement_assignment(state, statement);
            break;
        case STATEMENT_IF:
            sem_statement_if(state, statement);
            break;
        case STATEMENT_WHILE:
            sem_statement_while(state, statement);
            break;
        case STATEMENT_FOR:
            sem_statement_for(state, statement);
            break;
        case STATEMENT_WITH:
            sem_statement_with(state, statement);
            break;
        case STATEMENT_PROCEDURE_CALL:
            sem_statement_procedure_call(state, statement);
            break;
        case STATEMENT_IO_READ:
            sem_statement_io_read(state, statement);
            break;
        case STATEMENT_IO_WRITE:
            sem_statement_io_write(state, statement);
    }

}

static void sem_statement_assignment(SemanticState* state, Statement* statement) {

    /* Variable: variable */

    SemVariable sem_variable_temp = sem_variable(state, statement->assignment.variable);

    /* Expression: expression */

    SemExpression sem_expression_temp = sem_expression(state, statement->assignment.expression);

    /* body */

    check_assignment_validation(sem_variable_temp.type, sem_expression_temp.type);

}

static void sem_statement_if(SemanticState* state, Statement* statement) {

    /* Expression: expression */

    SemExpression sem_expression_temp = sem_expression(state, statement->if_.expression);

    if (!BOOLEAN_TYPE(sem_expression_Temp.type)) sem_error(statement->line, SEM_STATEMENT_IF_INVALID_EXPRESSION_TYPE);

    /* Statement: if_statement */

    sem_statement(state, statement->if_.if_statement);

    /* Statement: else_statement */

    if (statement->if_.else_statement) sem_statement(state, statement->if_.else_statement);

}

static void sem_statement_while(SemanticState* state, Statement* statement) {

    /* Expression: expression */

    SemExpression sem_expression_temp = sem_expression(state, statement->while_.expression);

    if (!BOOLEAN_TYPE(sem_expression_temp.type)) sem_error(statement->line, SEM_STATEMENT_WHILE_INVALID_EXPRESSION_TYPE);

    /* Statement: statement */

    sem_statement(state, statement->while_.statement);

}

static void sem_statement_for(SemanticState* state, Statement* statement) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, statement->for_.id);

    if (!symbol) sem_error(statement->line, SEM_ID_UNDECLARED, statement->for_.id->identifier);
    if (symbol->tag != VARIABLE) sem_error(statement->line, SEM_STATEMENT_FOR_ITERATOR_NOT_VARIABLE, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
    if (!INTEGER_TYPE(symbol->variable.type) sem_error(statement->line, SEM_STATEMENT_FOR_INVALID_ITERATOR_TYPE, symbol->identifier);

    /* Expression: iter_space */

    sem_expression(state, statement->for_.iter_space);

    /* Statement: statement */

    state->saved_symbol = symbol;
    sem_statement(state, statement->for_.statement);
    state->saved_symbol = NULL;

}

static void sem_statement_with(SemanticState* state, Statement* statement) {

    /* Variable: variable */

    SemVariable sem_variable_temp = sem_variable(state, statement->with.variable);

    if (!RECORD_TYPE(sem_variable_temp.symbol->variable.type)) sem_error(statement->line);

    /* Enter scope */

    symtab_enter_scope(state->symtab);

    for (int i = 0; i < sem_variable_temp.symbol->variable.type.id->type.record.field_count; ++i) {
        Symbol* symbol = symtab_add_symbol(state->symtab, sem_variable_temp.symbol->variable.type.id->type.record.fields[i]->identifier);
        symbol = sem_variable_temp.symbol->variable.type.id->type.record.fields[i]; // TODO: de tha doulepsei giati paizw me pointers
    }

    /* Statement: statement */

    sem_statement(state, statement->with.statement);

    /* Leave scope */

    symtab_leave_scope(state->symtab);

}

static void sem_statement_procedure_call(SemanticState* state, Statement* statement) {

    /* Id: id */

    Symbol* symbol = sem_id(state, ALL_SCOPES, statement->procedure_call.id);

    if (!symbol) sem_error(statement->line, SEM_ID_UNDECLARED, statement->procedure_call.id->identifier);
    if (symbol->tag != SUBPROGRAM) sem_error(statement->line, SEM_STATEMENT_PROCEDURE_CALL_NOT_SUBPROGRAM, symbol->identifier, SymbolTagString[symbol->tag], symbol->line);
    if (symbol->subprogram.type != PROCEDURE) sem_error(statement->line, SEM_STATEMENT_PROCEDURE_CALL_NOT_PROCEDURE, symbol->identifier);

    /* Expression: expressions */

    if (!symbol->subprogram.formal_parameter_count) sem_error(statement->line);

    Expression* expression;
    int i;

    for (expression = statement->procedure_call.expressions, i = 0; expression && i < symbol->subprogram.formal_parameter_count; expression = expression->next, ++i) {
        SemExpression sem_expression_ = sem_expression(state, expression);
        Symbol* formal_parameter_symbol = symbol->subprogram.formal_parameters[i];
    }

    if (expression) sem_error(statement->line);
    if (i < symbol->subprogram.formal_parameter_count) sem_error(statement->line);

}

static void sem_statement_io_read(SemanticState* state, Statement* statement) {

    /* Variable: read_list */

    sem_variable(state, statement->io_read.read_list);

}

static void sem_statement_io_write(SemanticState* state, Statement* statement) {

    /* Expression: write_list */

    sem_expression(state, statement->io_write.write_list);

}