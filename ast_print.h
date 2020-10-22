#ifndef AST_PRINT_H
#define AST_PRINT_H

#define AST_OUTPUT_DIR_NAME     "ast_output/"
#define AST_TAG_SPACE           30
#define AST_INDENT_SPACE        4

typedef struct AstPrintState {
    char filename[64];
    int indent;
    FILE* fp;
} AstPrintState;

AstPrintState ast_print_state_initialize(AST* ast);
AstPrintState ast_definition_print_state_initialize(Definition* definition);

void ast_print(AstPrintState* state, AST* ast);
void ast_id_print(AstPrintState* state, Id* id);
void ast_declaration_print(AstPrintState* state, Declaration* declaration);
void ast_definition_print(AstPrintState* state, Definition* definition);
void ast_expression_print(AstPrintState* state, Expression* expression);
void ast_variable_print(AstPrintState* state, Variable* variable);
void ast_constant_print(AstPrintState* state, Constant* constant);
void ast_type_print(AstPrintState* state, Type* type);
void ast_statement_print(AstPrintState* state, Statement* statement);

#endif