#include <string.h>
#include "ast.h"
#include "pasc600.h"

//////////////////////////////
/// Id
//////////////////////////////

Id* ast_id(const char* identifier) {

    Id* id = MALLOC(id, Id);

    id->identifier = strdup(identifier);
    id->next = NULL;

    return id;

}

//////////////////////////////
/// AST
//////////////////////////////

AST* ast_program(Definition* header, Declaration* declarations, Definition* subprograms, Statement* comp_statement) {

    AST* ast = MALLOC(ast, AST);

    ast->header = header;
    ast->declarations = declarations;
    ast->subprograms = subprograms;
    ast->comp_statement = comp_statement;

    return ast;

}

//////////////////////////////
/// Declaration
//////////////////////////////

Declaration* ast_program_declarations(Definition* constdefs, Definition* typedefs, Definition* vardefs) {

    Declaration* declaration = MALLOC(declaration, Declaration);

    declaration->constdefs = constdefs;
    declaration->typedefs = typedefs;
    declaration->vardefs = vardefs;

    return declaration;

}

//////////////////////////////
/// Definition
//////////////////////////////

Definition* ast_definition_program(Line line, Id* id) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_PROGRAM;
    definition->next = NULL;
    definition->program.id = id;

    return definition;

}

Definition* ast_definition_constant(Line line, Id* id, Expression* expression) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_CONSTANT;
    definition->next = NULL;
    definition->constant.id = id;
    definition->constant.expression = expression;

    return definition;

}

Definition* ast_definition_type(Line line, Id* id, Type* type) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_TYPE;
    definition->next = NULL;
    definition->type.id = id;
    definition->type.type = type;

    return definition;

}

Definition* ast_definition_field(Line line, Id* identifiers, Type* type) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_FIELD;
    definition->next = NULL;
    definition->field.identifiers = identifiers;
    definition->field.type = type;

    return definition;

}

Definition* ast_definition_variable(Line line, Id* identifiers, Type* type) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_VARIABLE;
    definition->next = NULL;
    definition->variable.identifiers = identifiers;
    definition->variable.type = type;

    return definition;

}

Definition* ast_definition_subprogram_forward(Line line, Definition* sub_header) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_SUBPROGRAM_FORWARD;
    definition->next = NULL;
    definition->subprogram_forward.sub_header = sub_header;

    return definition;

}

Definition* ast_definition_subprogram(Line line, Definition* sub_header, Declaration* declarations, Definition* subprograms, Statement* comp_statement) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_SUBPROGRAM;
    definition->next = NULL;
    definition->subprogram.sub_header = sub_header;
    definition->subprogram.declarations = declarations;
    definition->subprogram.subprograms = subprograms;
    definition->subprogram.comp_statement = comp_statement;

    return definition;

}

Definition* ast_definition_function(Line line, Id* id, Definition* formal_parameters, Type* type) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_FUNCTION;
    definition->next = NULL;
    definition->function.id = id;
    definition->function.formal_parameters = formal_parameters;
    definition->function.type = type;

    return definition;

}

Definition* ast_definition_procedure(Line line, Id* id, Definition* formal_parameters) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_PROCEDURE;
    definition->next = NULL;
    definition->procedure.id = id;
    definition->procedure.formal_parameters = formal_parameters;

    return definition;

}

Definition* ast_definition_parameter(Line line, Pass pass, Id* identifiers, Type* type) {

    Definition* definition = MALLOC(definition, Definition);

    definition->line = line;
    definition->tag = DEFINITION_PARAMETER;
    definition->next = NULL;
    definition->parameter.pass = pass;
    definition->parameter.identifiers = identifiers;
    definition->parameter.type = type;

    return definition;

}

//////////////////////////////
/// Expression
//////////////////////////////

Expression* ast_expression_binary(Line line, Operator operator, Expression* left_expression, Expression* right_expression) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_BINARY;
    expression_->next = NULL;
    expression_->binary.operator = operator;
    expression_->binary.left_expression = left_expression;
    expression_->binary.right_expression = right_expression;

    return expression_;

}

Expression* ast_expression_unary(Line line, Operator operator, Expression* expression) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_UNARY;
    expression_->next = NULL;
    expression_->unary.operator = operator;
    expression_->unary.expression = expression;

    return expression_;

}

Expression* ast_expression_variable(Line line, Variable* variable) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_VARIABLE;
    expression_->next = NULL;
    expression_->variable.variable = variable;

    return expression_;

}

Expression* ast_expression_function_call(Line line, Id* id, Expression* expressions) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_FUNCTION_CALL;
    expression_->next = NULL;
    expression_->function_call.id = id;
    expression_->function_call.expressions = expressions;

    return expression_;

}

Expression* ast_expression_length(Line line, Expression* expression) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_LENGTH;
    expression_->next = NULL;
    expression_->length.expression = expression;

    return expression_;

}

Expression* ast_expression_new(Line line, Expression* expression) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_NEW;
    expression_->next = NULL;
    expression_->new.expression = expression;

    return expression_;

}

Expression* ast_expression_constant(Line line, Constant* constant) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_CONSTANT;
    expression_->next = NULL;
    expression_->constant.constant = constant;

    return expression_;

}

Expression* ast_expression_enclosed(Line line, Expression* expression) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_ENCLOSED;
    expression_->next = NULL;
    expression_->enclosed.expression = expression;

    return expression_;

}

Expression* ast_expression_setlist(Line line, Expression* expression) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_SETLIST;
    expression_->next = NULL;
    expression_->setlist.expression = expression;

    return expression_;

}

Expression* ast_expression_limits(Line line, Constant* lower_limit, Constant* upper_limit) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_LIMITS;
    expression_->next = NULL;
    expression_->limits.lower_limit = lower_limit;
    expression_->limits.upper_limit = upper_limit;

    return expression_;

}

Expression* ast_expression_limits_id(Line line, Id* id) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_LIMITS_ID;
    expression_->next = NULL;
    expression_->limits_id.id = id;

    return expression_;

}

Expression* ast_expression_iter_space(Line line, IterSpace iter_space, Expression* initial_expression, Expression* final_expression) {

    Expression* expression_ = MALLOC(expression_, Expression);

    expression_->line = line;
    expression_->tag = EXPRESSION_ITER_SPACE;
    expression_->next = NULL;
    expression_->iter_space.iter_space = iter_space;
    expression_->iter_space.initial_expression = initial_expression;
    expression_->iter_space.final_expression = final_expression;

    return expression_;

}

//////////////////////////////
/// Variable
//////////////////////////////

Variable* ast_variable_id(Line line, Id* id) {

    Variable* variable_ = MALLOC(variable_, Variable);

    variable_->line = line;
    variable_->tag = VARIABLE_ID;
    variable_->next = NULL;
    variable_->id.id = id;

    return variable_;

}

Variable* ast_variable_component(Line line, Variable* variable, Id* id) {

    Variable* variable_ = MALLOC(variable_, Variable);

    variable_->line = line;
    variable_->tag = VARIABLE_COMPONENT;
    variable_->next = NULL;
    variable_->component.variable = variable;
    variable_->component.id = id;

    return variable_;

}

Variable* ast_variable_index(Line line, Variable* variable, Expression* expressions) {

    Variable* variable_ = MALLOC(variable_, Variable);

    variable_->line = line;
    variable_->tag = VARIABLE_INDEX;
    variable_->next = NULL;
    variable_->index.variable = variable;
    variable_->index.expressions = expressions;

    return variable_;

}

Variable* ast_variable_listfunc(Line line, Id* id, Expression* expression) {

    Variable* variable_ = MALLOC(variable_, Variable);

    variable_->line = line;
    variable_->tag = VARIABLE_LISTFUNC;
    variable_->next = NULL;
    variable_->listfunc.id = id;
    variable_->listfunc.expression = expression;

    return variable_;

}

//////////////////////////////
/// Constant
//////////////////////////////

Constant* ast_constant_integer(Line line, Operator operator, int integer) {

    Constant* constant = MALLOC(constant, Constant);

    constant->line = line;
    constant->tag = CONSTANT_INTEGER;
    constant->integer.operator = operator;
    constant->integer.integer = integer;

    return constant;

}

Constant* ast_constant_real(Line line, float real) {

    Constant* constant = MALLOC(constant, Constant);

    constant->line = line;
    constant->tag = CONSTANT_REAL;
    constant->real = real;

    return constant;

}

Constant* ast_constant_boolean(Line line, const char* boolean) {

    Constant* constant = MALLOC(constant, Constant);

    constant->line = line;
    constant->tag = CONSTANT_BOOLEAN;
    constant->boolean = (!strcmp(boolean, "true")) ? true : false;

    return constant;

}

Constant* ast_constant_character(Line line, char character) {

    Constant* constant = MALLOC(constant, Constant);

    constant->line = line;
    constant->tag = CONSTANT_CHARACTER;
    constant->character = character;

    return constant;

}

Constant* ast_constant_string(Line line, const char* string) {

    Constant* constant = MALLOC(constant, Constant);

    constant->line = line;
    constant->tag = CONSTANT_STRING;
    constant->string = strdup(string);

    return constant;

}

Constant* ast_constant_id(Line line, Operator operator, Id* id) {

    Constant* constant = MALLOC(constant, Constant);

    constant->line = line;
    constant->tag = CONSTANT_ID;
    constant->id.operator = operator;
    constant->id.id = id;

    return constant;

}

//////////////////////////////
/// Type
//////////////////////////////

Type* ast_type_array(Line line, Expression* dims, Type* type) {

    Type* type_ = MALLOC(type_, Type);

    type_->line = line;
    type_->tag = TYPE_ARRAY;
    type_->array.dims = dims;
    type_->array.type = type;

    return type_;

}

Type* ast_type_set(Line line, Type* type) {

    Type* type_ = MALLOC(type_, Type);

    type_->line = line;
    type_->tag = TYPE_SET;
    type_->set.type = type;

    return type_;

}

Type* ast_type_list(Line line, Type* type) {

    Type* type_ = MALLOC(type_, Type);

    type_->line = line;
    type_->tag = TYPE_LIST;
    type_->list.type = type;

    return type_;

}

Type* ast_type_record(Line line, Definition* fields) {

    Type* type_ = MALLOC(type_, Type);

    type_->line = line;
    type_->tag = TYPE_RECORD;
    type_->record.fields = fields;

    return type_;

}

Type* ast_type_enumeration(Line line, Id* identifiers) {

    Type* type_ = MALLOC(type_, Type);

    type_->line = line;
    type_->tag = TYPE_ENUMERATION;
    type_->enumeration.identifiers = identifiers;

    return type_;

}

Type* ast_type_subrange(Line line, Constant* lower_limit, Constant* upper_limit) {

    Type* type_ = MALLOC(type_, Type);

    type_->line = line;
    type_->tag = TYPE_SUBRANGE;
    type_->subrange.lower_limit = lower_limit;
    type_->subrange.upper_limit = upper_limit;

    return type_;

}

Type* ast_type_integer() {

    Type* type_ = MALLOC(type_, Type);

    type_->tag = TYPE_INTEGER;

    return type_;

}

Type* ast_type_real() {

    Type* type_ = MALLOC(type_, Type);

    type_->tag = TYPE_REAL;

    return type_;

}

Type* ast_type_boolean() {

    Type* type_ = MALLOC(type_, Type);

    type_->tag = TYPE_BOOLEAN;

    return type_;

}

Type* ast_type_character() {

    Type* type_ = MALLOC(type_, Type);

    type_->tag = TYPE_CHARACTER;

    return type_;

}

Type* ast_type_string() {

    Type* type_ = MALLOC(type_, Type);

    type_->tag = TYPE_STRING;

    return type_;

}

Type* ast_type_id(Line line, Id* id) {

    Type* type_ = MALLOC(type_, Type);

    type_->line = line;
    type_->tag = TYPE_ID;
    type_->id.id = id;

    return type_;

}

//////////////////////////////
/// Statement
//////////////////////////////

Statement* ast_statement_assignment(Line line, Variable* variable, Expression* expression) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_ASSIGNMENT;
    statement_->next = NULL;
    statement_->assignment.variable = variable;
    statement_->assignment.expression = expression;

    return statement_;

}

Statement* ast_statement_if(Line line, Expression* expression, Statement* if_statement, Statement* else_statement) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_IF;
    statement_->next = NULL;
    statement_->if_.expression = expression;
    statement_->if_.if_statement = if_statement;
    statement_->if_.else_statement = else_statement;

    return statement_;

}

Statement* ast_statement_while(Line line, Expression* expression, Statement* statement) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_WHILE;
    statement_->next = NULL;
    statement_->while_.expression = expression;
    statement_->while_.statement = statement;

    return statement_;

}

Statement* ast_statement_for(Line line, Id* id, Expression* iter_space, Statement* statement) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_FOR;
    statement_->next = NULL;
    statement_->for_.id = id;
    statement_->for_.iter_space = iter_space;
    statement_->for_.statement = statement;

    return statement_;

}

Statement* ast_statement_with(Line line, Variable* variable, Statement* statement) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_WITH;
    statement_->next = NULL;
    statement_->with.variable = variable;
    statement_->with.statement = statement;

    return statement_;

}

Statement* ast_statement_procedure_call(Line line, Id* id, Expression* expressions) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_PROCEDURE_CALL;
    statement_->next = NULL;
    statement_->procedure_call.id = id;
    statement_->procedure_call.expressions = expressions;

    return statement_;

}

Statement* ast_statement_io_read(Line line, Variable* read_list) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_IO_READ;
    statement_->next = NULL;
    statement_->io_read.read_list = read_list;

    return statement_;

}

Statement* ast_statement_io_write(Line line, Expression* write_list) {

    Statement* statement_ = MALLOC(statement_, Statement);

    statement_->line = line;
    statement_->tag = STATEMENT_IO_WRITE;
    statement_->next = NULL;
    statement_->io_write.write_list = write_list;

    return statement_;

}