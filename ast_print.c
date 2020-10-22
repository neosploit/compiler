#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include "ast.h"
#include "ast_print.h"
#include "error.h"
#include "parser.tab.h"

const char* OperatorString[] = { "undefined", ">", ">=", "<", "<=", "<>", "+", "-", "OR", "*", "/", "DIV", "MOD", "AND", "NOT", "IN", "=" };
const char* PassString[] = { "BY REFERENCE", "BY VALUE" };
const char* IterSpaceString[] = { "TO", "DOWN TO" };

//////////////////////////////
/// AST Print State functions
//////////////////////////////

AstPrintState ast_print_state_initialize(AST* ast) {

    AstPrintState state;
    struct stat st;

    strcpy(state.filename, AST_OUTPUT_DIR_NAME);
    strcat(state.filename, "program.");
    strcat(state.filename, ast->header->program.id->identifier);
    strcat(state.filename, ".txt");

    state.indent = 0;

    if (stat(AST_OUTPUT_DIR_NAME, &st) < 0) {
        if (mkdir(AST_OUTPUT_DIR_NAME, 0600) < 0) {
            internal_error("mkdir");
        }
    }

    if (!(state.fp = fopen(state.filename, "w"))) {
        internal_error("fopen");
    }

    return state;

}

AstPrintState ast_definition_print_state_initialize(Definition* definition) {

    AstPrintState state;
    struct stat st;

    strcpy(state.filename, AST_OUTPUT_DIR_NAME);

    if (definition->subprogram.sub_header->tag == DEFINITION_FUNCTION) {
        strcat(state.filename, "function.");
        strcat(state.filename, definition->subprogram.sub_header->function.id->identifier);
    } else {
        strcat(state.filename, "procedure.");
        strcat(state.filename, definition->subprogram.sub_header->procedure.id->identifier);
    }

    strcat(state.filename, ".txt");

    state.indent = 0;

    if (stat(AST_OUTPUT_DIR_NAME, &st) < 0) {
        if (mkdir(AST_OUTPUT_DIR_NAME, 0600) < 0) {
            internal_error("mkdir");
        }
    }

    if (!(state.fp = fopen(state.filename, "w"))) {
        internal_error("fopen");
    }

    return state;

}

//////////////////////////////
/// Static functions
//////////////////////////////

static void ast_tag_space_print(AstPrintState* state, int tag_length) {

    for (int i = 0; i < AST_TAG_SPACE - tag_length; ++i) fprintf(state->fp, " ");

}

static void ast_indent_space_print(AstPrintState* state) {

    for (int i = 0; i < state->indent * AST_INDENT_SPACE; ++i) fprintf(state->fp, " ");

}

//////////////////////////////
/// AST
//////////////////////////////

void ast_print(AstPrintState* state, AST* ast) {

    ast_definition_print(state, ast->header);
    ast_declaration_print(state, ast->declarations);
    if (ast->subprograms) ast_definition_print(state, ast->subprograms);
    if (ast->comp_statement) ast_statement_print(state, ast->comp_statement);

}

//////////////////////////////
/// Id
//////////////////////////////

void ast_id_print(AstPrintState* state, Id* id) {

    fprintf(state->fp, " (ID: %s)", id->identifier);

    if (id->next) ast_id_print(state, id->next);

}

//////////////////////////////
/// Declaration
//////////////////////////////

void ast_declaration_print(AstPrintState* state, Declaration* declaration) {

    if (declaration->constdefs) ast_definition_print(state, declaration->constdefs);
    if (declaration->typedefs) ast_definition_print(state, declaration->typedefs);
    if (declaration->vardefs) ast_definition_print(state, declaration->vardefs);

}

//////////////////////////////
/// Definition
//////////////////////////////

void ast_definition_print(AstPrintState* state, Definition* definition) {

    switch (definition->tag) {
        case DEFINITION_PROGRAM:
            fprintf(state->fp, "[DEFINITION_PROGRAM]");
            ast_tag_space_print(state, strlen("[DEFINITION_PROGRAM]"));
            ast_indent_space_print(state);

            ast_id_print(state, definition->program.id);

            break;
        case DEFINITION_CONSTANT:
            fprintf(state->fp, "\n[DEFINITION_CONSTANT]");
            ast_tag_space_print(state, strlen("[DEFINITION_CONSTANT]"));
            ast_indent_space_print(state);

            ast_id_print(state, definition->constant.id);
            ast_expression_print(state, definition->constant.expression);

            break;
        case DEFINITION_TYPE:
            fprintf(state->fp, "\n[DEFINITION_TYPE]");
            ast_tag_space_print(state, strlen("[DEFINITION_TYPE]"));
            ast_indent_space_print(state);

            ast_id_print(state, definition->type.id);
            ast_type_print(state, definition->type.type);

            break;
        case DEFINITION_FIELD:
            fprintf(state->fp, "\n[DEFINITION_FIELD]");
            ast_tag_space_print(state, strlen("[DEFINITION_FIELD]"));
            ast_indent_space_print(state);

            ast_id_print(state, definition->field.identifiers);
            ast_type_print(state, definition->field.type);

            break;
        case DEFINITION_VARIABLE:
            fprintf(state->fp, "\n[DEFINITION_VARIABLE]");
            ast_tag_space_print(state, strlen("[DEFINITION_VARIABLE]"));
            ast_indent_space_print(state);

            ast_id_print(state, definition->variable.identifiers);
            ast_type_print(state, definition->variable.type);

            break;
        case DEFINITION_SUBPROGRAM_FORWARD:
            fprintf(state->fp, "\n[DEFINITION_SUBPROGRAM_FORWARD]");
            ast_tag_space_print(state, strlen("[DEFINITION_SUBPROGRAM_FORWARD]"));

            ast_definition_print(state, definition->subprogram_forward.sub_header);

            break;
        case DEFINITION_SUBPROGRAM:
            fprintf(state->fp, "[DEFINITION_SUBPROGRAM]");

            ast_definition_print(state, definition->subprogram.sub_header);

            state->indent++;

            ast_declaration_print(state, definition->subprogram.declarations);
            if (definition->subprogram.subprograms) ast_definition_print(state, definition->subprogram.subprograms);
            if (definition->subprogram.comp_statement) ast_statement_print(state, definition->subprogram.comp_statement);

            state->indent--;

            break;
        case DEFINITION_FUNCTION:
            fprintf(state->fp, "\n[DEFINITION_FUNCTION]");
            ast_tag_space_print(state, strlen("[DEFINITION_FUNCTION]"));
            ast_indent_space_print(state);

            ast_id_print(state, definition->function.id);
            if (definition->function.formal_parameters) ast_definition_print(state, definition->function.formal_parameters);
            if (definition->function.type) ast_type_print(state, definition->function.type);

            break;
        case DEFINITION_PROCEDURE:
            fprintf(state->fp, "\n[DEFINITION_PROCEDURE]");
            ast_tag_space_print(state, strlen("[DEFINITION_PROCEDURE]"));
            ast_indent_space_print(state);

            ast_id_print(state, definition->procedure.id);
            if (definition->procedure.formal_parameters) ast_definition_print(state, definition->procedure.formal_parameters);

            break;
        case DEFINITION_PARAMETER:
            fprintf(state->fp, "[DEFINITION_PARAMETER:");

            fprintf(state->fp, " (PASS: %s)", PassString[definition->parameter.pass]);
            ast_id_print(state, definition->parameter.identifiers);
            ast_type_print(state, definition->parameter.type);

            fprintf(state->fp, "]");
    }

    if (definition->next) ast_definition_print(state, definition->next);

}

//////////////////////////////
/// Expression
//////////////////////////////

void ast_expression_print(AstPrintState* state, Expression* expression) {

    switch (expression->tag) {
        case EXPRESSION_BINARY:
            fprintf(state->fp, " [EXPRESSION_BINARY:");

            ast_expression_print(state, expression->binary.left_expression);
            if (expression->binary.operator != UNDEFINED) fprintf(state->fp, " (OPERATOR: %s)", OperatorString[expression->binary.operator]);
            ast_expression_print(state, expression->binary.right_expression);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_UNARY:
            fprintf(state->fp, " [EXPRESSION_UNARY:");

            if (expression->unary.operator != UNDEFINED) fprintf(state->fp, " (OPERATOR: %s)", OperatorString[expression->unary.operator]);
            ast_expression_print(state, expression->unary.expression);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_VARIABLE:
            fprintf(state->fp, " [EXPRESSION_VARIABLE:");

            ast_variable_print(state, expression->variable.variable);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_FUNCTION_CALL:
            fprintf(state->fp, " [EXPRESSION_FUNCTION_CALL:");

            ast_id_print(state, expression->function_call.id);
            ast_expression_print(state, expression->function_call.expressions);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_LENGTH:
            fprintf(state->fp, " [EXPRESSION_LENGTH:");

            ast_expression_print(state, expression->length.expression);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_NEW:
            fprintf(state->fp, " [EXPRESSION_NEW:");

            ast_expression_print(state, expression->new.expression);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_CONSTANT:
            fprintf(state->fp, " [EXPRESSION_CONSTANT:");

            ast_constant_print(state, expression->constant.constant);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_ENCLOSED:
            fprintf(state->fp, " [EXPRESSION_ENCLOSED:");

            ast_expression_print(state, expression->enclosed.expression);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_SETLIST:
            fprintf(state->fp, " [EXPRESSION_SETLIST:");

            if (expression->setlist.expression) ast_expression_print(state, expression->setlist.expression);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_LIMITS:
            fprintf(state->fp, " [EXPRESSION_LIMITS:");

            ast_constant_print(state, expression->limits.lower_limit);
            ast_constant_print(state, expression->limits.upper_limit);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_LIMITS_ID:
            fprintf(state->fp, " [EXPRESSION_LIMITS_ID:");

            ast_id_print(state, expression->limits_id.id);

            fprintf(state->fp, "]");

            break;
        case EXPRESSION_ITER_SPACE:
            fprintf(state->fp, " [EXPRESSION_ITER_SPACE:");

            ast_expression_print(state, expression->iter_space.initial_expression);
            fprintf(state->fp, " (ITER SPACE: %s)", IterSpaceString[expression->iter_space.iter_space]);
            ast_expression_print(state, expression->iter_space.final_expression);

            fprintf(state->fp, "]");
    }

    if (expression->next) ast_expression_print(state, expression->next);

}

//////////////////////////////
/// Variable
//////////////////////////////

void ast_variable_print(AstPrintState* state, Variable* variable) {

    switch (variable->tag) {
        case VARIABLE_ID:
            fprintf(state->fp, " [VARIABLE_ID:");
            ast_id_print(state, variable->id.id);
            fprintf(state->fp, "]");

            break;
        case VARIABLE_COMPONENT:
            fprintf(state->fp, " [VARIABLE_COMPONENT:");
            ast_variable_print(state, variable->component.variable);
            ast_id_print(state, variable->component.id);
            fprintf(state->fp, "]");

            break;
        case VARIABLE_INDEX:
            fprintf(state->fp, " [VARIABLE_INDEX:");
            ast_variable_print(state, variable->index.variable);
            ast_expression_print(state, variable->index.expressions);
            fprintf(state->fp, "]");

            break;
        case VARIABLE_LISTFUNC:
            fprintf(state->fp, " [VARIABLE_LISTFUNC:");
            ast_id_print(state, variable->listfunc.id);
            ast_expression_print(state, variable->listfunc.expression);
            fprintf(state->fp, "]");
    }

    if (variable->next) ast_variable_print(state, variable->next);

}

//////////////////////////////
/// Constant
//////////////////////////////

void ast_constant_print(AstPrintState* state, Constant* constant) {

    switch (constant->tag) {
        case CONSTANT_INTEGER:
            if (constant->integer.operator != UNDEFINED) fprintf(state->fp, " (OPERATOR: %s)", OperatorString[constant->integer.operator]);
            fprintf(state->fp, " (ICONST: %d)", constant->integer.integer);

            break;
        case CONSTANT_REAL:
            fprintf(state->fp, " (RCONST: %lf)", constant->real);

            break;
        case CONSTANT_BOOLEAN:
            fprintf(state->fp, " (BCONST: %s)", constant->boolean ? "true" : "false");

            break;
        case CONSTANT_CHARACTER:
            fprintf(state->fp, " (CCONST: %c)", constant->character);

            break;
        case CONSTANT_STRING:
            fprintf(state->fp, " (SCONST: %s)", constant->string);

            break;
        case CONSTANT_ID:
            if (constant->id.operator != UNDEFINED) fprintf(state->fp, " (OPERATOR: %s)", OperatorString[constant->id.operator]);
            ast_id_print(state, constant->id.id);
    }

}

//////////////////////////////
/// Type
//////////////////////////////

void ast_type_print(AstPrintState* state, Type* type) {

    switch (type->tag) {
        case TYPE_UNDEFINED:
            break;
        case TYPE_ARRAY:
            fprintf(state->fp, " (TYPE_ARRAY)");
            ast_expression_print(state, type->array.dims);
            ast_type_print(state, type->array.type);

            break;
        case TYPE_SET:
            fprintf(state->fp, " (TYPE_SET)");
            ast_type_print(state, type->set.type);

            break;
        case TYPE_LIST:
            fprintf(state->fp, " (TYPE_LIST)");
            ast_type_print(state, type->list.type);

            break;
        case TYPE_RECORD:
            fprintf(state->fp, " (TYPE_RECORD)");

            state->indent++;
            ast_definition_print(state, type->record.fields);
            state->indent--;

            break;
        case TYPE_ENUMERATION:
            fprintf(state->fp, " (TYPE_ENUMERATION)");
            ast_id_print(state, type->enumeration.identifiers);

            break;
        case TYPE_SUBRANGE:
            fprintf(state->fp, " (TYPE_SUBRANGE)");
            ast_constant_print(state, type->subrange.lower_limit);
            ast_constant_print(state, type->subrange.upper_limit);

            break;
        case TYPE_INTEGER:
            fprintf(state->fp, " (TYPE_INTEGER)");

            break;
        case TYPE_REAL:
            fprintf(state->fp, " (TYPE_REAL)");

            break;
        case TYPE_BOOLEAN:
            fprintf(state->fp, " (TYPE_BOOLEAN)");

            break;
        case TYPE_CHARACTER:
            fprintf(state->fp, " (TYPE_CHARACTER)");

            break;
        case TYPE_STRING:
            fprintf(state->fp, " (TYPE_STRING)");

            break;
        case TYPE_ID:
            fprintf(state->fp, "[TYPE_ID:");
            ast_id_print(state, type->id.id);
            fprintf(state->fp, "]");
    }

}

//////////////////////////////
/// Statement
//////////////////////////////

void ast_statement_print(AstPrintState* state, Statement* statement) {

    switch (statement->tag) {
        case STATEMENT_ASSIGNMENT:
            fprintf(state->fp, "\n[STATEMENT_ASSIGNMENT]");
            ast_tag_space_print(state, strlen("[STATEMENT_ASSIGNMENT]"));
            ast_indent_space_print(state);

            ast_variable_print(state, statement->assignment.variable);
            ast_expression_print(state, statement->assignment.expression);

            break;
        case STATEMENT_IF:
            fprintf(state->fp, "\n[STATEMENT_IF]");
            ast_tag_space_print(state, strlen("[STATEMENT_IF]"));
            ast_indent_space_print(state);

            ast_expression_print(state, statement->if_.expression);

            state->indent++;
            ast_statement_print(state, statement->if_.if_statement);
            state->indent--;

            if (statement->if_.else_statement) {
                fprintf(state->fp, "\n");
                ast_tag_space_print(state, strlen(""));
                ast_indent_space_print(state);
                fprintf(state->fp, " (ELSE)");
                
                state->indent++;
                ast_statement_print(state, statement->if_.else_statement);
                state->indent--;
            }

            break;
        case STATEMENT_WHILE:
            fprintf(state->fp, "\n[STATEMENT_WHILE]");
            ast_tag_space_print(state, strlen("[STATEMENT_WHILE]"));
            ast_indent_space_print(state);

            ast_expression_print(state, statement->while_.expression);

            state->indent++;
            ast_statement_print(state, statement->while_.statement);
            state->indent--;

            break;
        case STATEMENT_FOR:
            fprintf(state->fp, "\n[STATEMENT_FOR]");
            ast_tag_space_print(state, strlen("[STATEMENT_FOR]"));
            ast_indent_space_print(state);

            ast_expression_print(state, statement->for_.iter_space);

            state->indent++;
            ast_statement_print(state, statement->for_.statement);
            state->indent--;

            break;
        case STATEMENT_WITH:
            fprintf(state->fp, "\n[STATEMENT_WITH]");
            ast_tag_space_print(state, strlen("[STATEMENT_WITH]"));
            ast_indent_space_print(state);

            ast_variable_print(state, statement->with.variable);
            ast_statement_print(state, statement->with.statement);

            break;
        case STATEMENT_PROCEDURE_CALL:
            fprintf(state->fp, "\n[STATEMENT_PROCEDURE_CALL]");
            ast_tag_space_print(state, strlen("[STATEMENT_PROCEDURE_CALL]"));
            ast_indent_space_print(state);

            ast_id_print(state, statement->procedure_call.id);
            if (statement->procedure_call.expressions) ast_expression_print(state, statement->procedure_call.expressions);

            break;
        case STATEMENT_IO_READ:
            fprintf(state->fp, "\n[STATEMENT_IO_READ]");
            ast_tag_space_print(state, strlen("[STATEMENT_IO_READ]"));
            ast_indent_space_print(state);

            ast_variable_print(state, statement->io_read.read_list);

            break;
        case STATEMENT_IO_WRITE:
            fprintf(state->fp, "\n[STATEMENT_IO_WRITE]");
            ast_tag_space_print(state, strlen("[STATEMENT_IO_WRITE]"));
            ast_indent_space_print(state);

            ast_expression_print(state, statement->io_write.write_list);
    }

    if (statement->next) ast_statement_print(state, statement->next);

}