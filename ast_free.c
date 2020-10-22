#include <stdlib.h>
#include "ast.h"

//////////////////////////////
/// Static functions
//////////////////////////////

static void ast_id_free(Id* id);
static void ast_declaration_free(Declaration* declaration);
static void ast_definition_free(Definition* definition);
static void ast_expression_free(Expression* expression);
static void ast_variable_free(Variable* variable);
static void ast_constant_free(Constant* constant);
static void ast_type_free(Type* type);
static void ast_statement_free(Statement* statement);

//////////////////////////////
/// AST
//////////////////////////////

void ast_free(AST* ast) {

    ast_definition_free(ast->header);
    ast_declaration_free(ast->declarations);
    if (ast->subprograms) ast_definition_free(ast->subprograms);
    if (ast->comp_statement) ast_statement_free(ast->comp_statement);

    free(ast);

}

//////////////////////////////
/// Id
//////////////////////////////

static void ast_id_free(Id* id) {

    if (id->next) ast_id_free(id->next);

    free(id->identifier);
    free(id);

}

//////////////////////////////
/// Declaration
//////////////////////////////

static void ast_declaration_free(Declaration* declaration) {

    if (declaration->constdefs) ast_definition_free(declaration->constdefs);
    if (declaration->typedefs) ast_definition_free(declaration->typedefs);
    if (declaration->vardefs) ast_definition_free(declaration->vardefs);

    free(declaration);

}

//////////////////////////////
/// Definition
//////////////////////////////

static void ast_definition_free(Definition* definition) {

    if (definition->next) ast_definition_free(definition->next);

    switch (definition->tag) {
        case DEFINITION_PROGRAM:
            ast_id_free(definition->program.id);

            break;
        case DEFINITION_CONSTANT:
            ast_id_free(definition->constant.id);
            ast_expression_free(definition->constant.expression);

            break;
        case DEFINITION_TYPE:
            ast_id_free(definition->type.id);
            ast_type_free(definition->type.type);

            break;
        case DEFINITION_FIELD:
            ast_id_free(definition->field.identifiers);
            ast_type_free(definition->field.type);

            break;
        case DEFINITION_VARIABLE:
            ast_id_free(definition->variable.identifiers);
            ast_type_free(definition->variable.type);

            break;
        case DEFINITION_SUBPROGRAM_FORWARD:
            ast_definition_free(definition->subprogram_forward.sub_header);

            break;
        case DEFINITION_SUBPROGRAM:
            ast_definition_free(definition->subprogram.sub_header);
            ast_declaration_free(definition->subprogram.declarations);
            if (definition->subprogram.subprograms) ast_definition_free(definition->subprogram.subprograms);
            if (definition->subprogram.comp_statement) ast_statement_free(definition->subprogram.comp_statement);

            break;
        case DEFINITION_FUNCTION:
            ast_id_free(definition->function.id);
            if (definition->function.formal_parameters) ast_definition_free(definition->function.formal_parameters);
            if (definition->function.type) ast_type_free(definition->function.type);

            break;
        case DEFINITION_PROCEDURE:
            ast_id_free(definition->procedure.id);
            if (definition->procedure.formal_parameters) ast_definition_free(definition->procedure.formal_parameters);

            break;
        case DEFINITION_PARAMETER:
            ast_id_free(definition->parameter.identifiers);
            ast_type_free(definition->parameter.type);
    }

    free(definition);

}

//////////////////////////////
/// Expression
//////////////////////////////

static void ast_expression_free(Expression* expression) {

    if (expression->next) ast_expression_free(expression->next);

    switch (expression->tag) {
        case EXPRESSION_BINARY:
            ast_expression_free(expression->binary.left_expression);
            ast_expression_free(expression->binary.right_expression);

            break;
        case EXPRESSION_UNARY:
            ast_expression_free(expression->unary.expression);

            break;
        case EXPRESSION_VARIABLE:
            ast_variable_free(expression->variable.variable);

            break;
        case EXPRESSION_FUNCTION_CALL:
            ast_id_free(expression->function_call.id);
            ast_expression_free(expression->function_call.expressions);

            break;
        case EXPRESSION_LENGTH:
            ast_expression_free(expression->length.expression);

            break;
        case EXPRESSION_NEW:
            ast_expression_free(expression->new.expression);

            break;
        case EXPRESSION_CONSTANT:
            ast_constant_free(expression->constant.constant);

            break;
        case EXPRESSION_ENCLOSED:
            ast_expression_free(expression->enclosed.expression);

            break;
        case EXPRESSION_SETLIST:
            if (expression->setlist.expression) ast_expression_free(expression->setlist.expression);

            break;
        case EXPRESSION_LIMITS:
            ast_constant_free(expression->limits.lower_limit);
            ast_constant_free(expression->limits.upper_limit);

            break;
        case EXPRESSION_LIMITS_ID:
            ast_id_free(expression->limits_id.id);

            break;
        case EXPRESSION_ITER_SPACE:
            ast_expression_free(expression->iter_space.initial_expression);
            ast_expression_free(expression->iter_space.final_expression);
    }

    free(expression);

}

//////////////////////////////
/// Variable
//////////////////////////////

static void ast_variable_free(Variable* variable) {

    if (variable->next) ast_variable_free(variable->next);

    switch (variable->tag) {
        case VARIABLE_ID:
            ast_id_free(variable->id.id);

            break;
        case VARIABLE_COMPONENT:
            ast_variable_free(variable->component.variable);
            ast_id_free(variable->component.id);

            break;
        case VARIABLE_INDEX:
            ast_variable_free(variable->index.variable);
            ast_expression_free(variable->index.expressions);

            break;
        case VARIABLE_LISTFUNC:
            ast_id_free(variable->listfunc.id);
            ast_expression_free(variable->listfunc.expression);
    }

    free(variable);

}

//////////////////////////////
/// Constant
//////////////////////////////

static void ast_constant_free(Constant* constant) {

    switch (constant->tag) {
        case CONSTANT_INTEGER:
            break;
        case CONSTANT_REAL:
            break;
        case CONSTANT_BOOLEAN:
            break;
        case CONSTANT_CHARACTER:
            break;
        case CONSTANT_STRING:
            free(constant->string);

            break;
        case CONSTANT_ID:
            ast_id_free(constant->id.id);
    }

    free(constant);

}

//////////////////////////////
/// Type
//////////////////////////////

static void ast_type_free(Type* type) {

    switch (type->tag) {
        case TYPE_UNDEFINED:
            break;
        case TYPE_ARRAY:
            ast_expression_free(type->array.dims);
            ast_type_free(type->array.type);

            break;
        case TYPE_SET:
            ast_type_free(type->set.type);

            break;
        case TYPE_LIST:
            ast_type_free(type->list.type);

            break;
        case TYPE_RECORD:
            ast_definition_free(type->record.fields);

            break;
        case TYPE_ENUMERATION:
            ast_id_free(type->enumeration.identifiers);

            break;
        case TYPE_SUBRANGE:
            ast_constant_free(type->subrange.lower_limit);
            ast_constant_free(type->subrange.upper_limit);

            break;
        case TYPE_INTEGER:
            break;
        case TYPE_REAL:
            break;
        case TYPE_BOOLEAN:
            break;
        case TYPE_CHARACTER:
            break;
        case TYPE_STRING:
            break;
        case TYPE_ID:
            ast_id_free(type->id.id);
    }

    free(type);

}

//////////////////////////////
/// Statement
//////////////////////////////

static void ast_statement_free(Statement* statement) {

    if (statement->next) ast_statement_free(statement->next);

    switch (statement->tag) {
        case STATEMENT_ASSIGNMENT:
            ast_variable_free(statement->assignment.variable);
            ast_expression_free(statement->assignment.expression);

            break;
        case STATEMENT_IF:
            ast_expression_free(statement->if_.expression);
            ast_statement_free(statement->if_.if_statement);
            if (statement->if_.else_statement) ast_statement_free(statement->if_.else_statement);

            break;
        case STATEMENT_WHILE:
            ast_expression_free(statement->while_.expression);
            ast_statement_free(statement->while_.statement);

            break;
        case STATEMENT_FOR:
            ast_id_free(statement->for_.id);
            ast_expression_free(statement->for_.iter_space);
            ast_statement_free(statement->for_.statement);

            break;
        case STATEMENT_WITH:
            ast_variable_free(statement->with.variable);
            ast_statement_free(statement->with.statement);

            break;
        case STATEMENT_PROCEDURE_CALL:
            ast_id_free(statement->procedure_call.id);
            if (statement->procedure_call.expressions) ast_expression_free(statement->procedure_call.expressions);

            break;
        case STATEMENT_IO_READ:
            ast_variable_free(statement->io_read.read_list);

            break;
        case STATEMENT_IO_WRITE:
            ast_expression_free(statement->io_write.write_list);
    }

    free(statement);

}