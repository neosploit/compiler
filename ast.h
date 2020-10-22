#ifndef AST_H
#define AST_H

#include <stdbool.h>

//////////////////////////////
/// Node Information
//////////////////////////////

#define NO_LINE -1
typedef int     Line;
typedef enum    Operator    { UNDEFINED, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, NOT_EQUAL, PLUS, MINUS, KW_OR, MUL, DIV, KW_DIV, KW_MOD, KW_AND, KW_NOT, KW_IN, EQUAL } Operator;
typedef enum    Pass        { BY_REFERENCE, BY_VALUE } Pass;
typedef enum    IterSpace   { TO, DOWNTO } IterSpace;

//////////////////////////////
/// Node Tags
//////////////////////////////

typedef enum DefinitionTag {
    DEFINITION_PROGRAM,
    DEFINITION_CONSTANT,
    DEFINITION_TYPE,
    DEFINITION_FIELD,
    DEFINITION_VARIABLE,
    DEFINITION_SUBPROGRAM_FORWARD,
    DEFINITION_SUBPROGRAM,
    DEFINITION_FUNCTION,
    DEFINITION_PROCEDURE,
    DEFINITION_PARAMETER
} DefinitionTag;

typedef enum ExpressionTag {
    EXPRESSION_BINARY,
    EXPRESSION_UNARY,
    EXPRESSION_VARIABLE,
    EXPRESSION_FUNCTION_CALL,
    EXPRESSION_LENGTH,
    EXPRESSION_NEW,
    EXPRESSION_CONSTANT,
    EXPRESSION_ENCLOSED,
    EXPRESSION_SETLIST,
    EXPRESSION_LIMITS,
    EXPRESSION_LIMITS_ID,
    EXPRESSION_ITER_SPACE
} ExpressionTag;

typedef enum VariableTag {
    VARIABLE_ID,
    VARIABLE_COMPONENT,
    VARIABLE_INDEX,
    VARIABLE_LISTFUNC
} VariableTag;

typedef enum ConstantTag {
    CONSTANT_INTEGER,
    CONSTANT_REAL,
    CONSTANT_BOOLEAN,
    CONSTANT_CHARACTER,
    CONSTANT_STRING,
    CONSTANT_ID
} ConstantTag;

typedef enum TypeTag {
    TYPE_UNDEFINED,
    TYPE_ARRAY,
    TYPE_SET,
    TYPE_LIST,
    TYPE_RECORD,
    TYPE_ENUMERATION,
    TYPE_SUBRANGE,
    TYPE_INTEGER,
    TYPE_REAL,
    TYPE_BOOLEAN,
    TYPE_CHARACTER,
    TYPE_STRING,
    TYPE_ID
} TypeTag;

typedef enum StatementTag {
    STATEMENT_ASSIGNMENT,
    STATEMENT_IF,
    STATEMENT_WHILE,
    STATEMENT_FOR,
    STATEMENT_WITH,
    STATEMENT_PROCEDURE_CALL,
    STATEMENT_IO_READ,
    STATEMENT_IO_WRITE
} StatementTag;

//////////////////////////////
/// Nodes
//////////////////////////////

typedef struct Id Id;
typedef struct AST AST;
typedef struct Declaration Declaration;
typedef struct Definition Definition;
typedef struct Expression Expression;
typedef struct Variable Variable;
typedef struct Constant Constant;
typedef struct Type Type;
typedef struct Statement Statement;

typedef struct Id {
    char* identifier;
    Id* next;
} Id;

typedef struct AST {
    Definition* header;
    Declaration* declarations;
    Definition* subprograms;
    Statement* comp_statement;
} AST;

typedef struct Declaration {
    Definition* constdefs;
    Definition* typedefs;
    Definition* vardefs;
} Declaration;

typedef struct Definition {
    Line line;
    DefinitionTag tag;
    Definition* next;

    union {
        /* DEFINITION_PROGRAM */
        struct {
            Id* id;
        } program;

        /* DEFINITION_CONSTANT */
        struct {
            Id* id;
            Expression* expression;
        } constant;

        /* DEFINITION_TYPE */
        struct {
            Id* id;
            Type* type;
        } type;

        /* DEFINITION_FIELD */
        struct {
            Id* identifiers;
            Type* type;
        } field;

        /* DEFINITION_VARIABLE */
        struct {
            Id* identifiers;
            Type* type;
        } variable;

        /* DEFINITION_SUBPROGRAM_FORWARD */
        struct {
            Definition* sub_header;
        } subprogram_forward;

        /* DEFINITION_SUBPROGRAM */
        struct {
            Definition* sub_header;
            Declaration* declarations;
            Definition* subprograms;
            Statement* comp_statement;
        } subprogram;

        /* DEFINITION_FUNCTION */
        struct {
            Id* id;
            Definition* formal_parameters;
            Type* type;
        } function;

        /* DEFINITION_PROCEDURE */
        struct {
            Id* id;
            Definition* formal_parameters;
        } procedure;

        /* DEFINITION_PARAMETER */
        struct {
            Pass pass;
            Id* identifiers;
            Type* type;
        } parameter;
    };
} Definition;

typedef struct Expression {
    Line line;
    ExpressionTag tag;
    Expression* next;

    union {
        /* EXPRESSION_BINARY */
        struct {
            Operator operator;
            Expression* left_expression;
            Expression* right_expression;
        } binary;

        /* EXPRESSION_UNARY */
        struct {
            Operator operator;
            Expression* expression;
        } unary;

        /* EXPRESSION_VARIABLE */
        struct {
            Variable* variable;
        } variable;

        /* EXPRESSION_FUNCTION_CALL */
        struct {
            Id* id;
            Expression* expressions;
        } function_call;

        /* EXPRESSION_LENGTH */
        struct {
            Expression* expression;
        } length;

        /* EXPRESSION_NEW */
        struct {
            Expression* expression;
        } new;

        /* EXPRESSION_CONSTANT */
        struct {
            Constant* constant;
        } constant;

        /* EXPRESSION_ENCLOSED */
        struct {
            Expression* expression;
        } enclosed;

        /* EXPRESSION_SETLIST */
        struct {
            Expression* expression;
        } setlist;

        /* EXPRESSION_LIMITS */
        struct {
            Constant* lower_limit;
            Constant* upper_limit;
        } limits;

        /* EXPRESSION_LIMITS_ID */
        struct {
            Id* id;
        } limits_id;

        /* EXPRESSION_ITER_SPACE */
        struct {
            IterSpace iter_space;
            Expression* initial_expression;
            Expression* final_expression;
        } iter_space;
    };
} Expression;

typedef struct Variable {
    Line line;
    VariableTag tag;
    Variable* next;

    union {
        /* VARIABLE_ID */
        struct {
            Id* id;
        } id;

        /* VARIABLE_COMPONENT */
        struct {
            Variable* variable;
            Id* id;
        } component;

        /* VARIABLE_INDEX */
        struct {
            Variable* variable;
            Expression* expressions;
        } index;

        /* VARIABLE_LISTFUNC */
        struct {
            Id* id;
            Expression* expression;
        } listfunc;
    };
} Variable;

typedef struct Constant {
    Line line;
    ConstantTag tag;

    union {
        /* CONSTANT_INTEGER */
        struct {
            Operator operator;
            int integer;
        } integer;

        /* CONSTANT_REAL */
        float real;

        /* CONSTANT_BOOLEAN */
        bool boolean;

        /* CONSTANT_CHARACTER */
        char character;

        /* CONSTANT_STRING */
        char* string;

        /* CONSTANT_ID */
        struct {
            Operator operator;
            Id* id;
        } id;
    };
} Constant;

typedef struct Type {
    Line line;
    TypeTag tag;

    union {
        /* TYPE_ARRAY */
        struct {
            Expression* dims;
            Type* type;
        } array;

        /* TYPE_SET */
        struct {
            Type* type;
        } set;

        /* TYPE_LIST */
        struct {
            Type* type;
        } list;

        /* TYPE_RECORD */
        struct {
            Definition* fields;
        } record;

        /* TYPE_ENUMERATION */
        struct {
            Id* identifiers;
        } enumeration;

        /* TYPE_SUBRANGE */
        struct {
            Constant* lower_limit;
            Constant* upper_limit;
        } subrange;

        /* TYPE_ID */
        struct {
            Id* id;
        } id;
    };
} Type;

typedef struct Statement {
    Line line;
    StatementTag tag;
    Statement* next;

    union {
        /* STATEMENT_ASSIGNMENT */
        struct {
            Variable* variable;
            Expression* expression;
        } assignment;

        /* STATEMENT_IF */
        struct {
            Expression* expression;
            Statement* if_statement;
            Statement* else_statement;
        } if_;

        /* STATEMENT_WHILE */
        struct {
            Expression* expression;
            Statement* statement;
        } while_;

        /* STATEMENT_FOR */
        struct {
            Id* id;
            Expression* iter_space;
            Statement* statement;
        } for_;

        /* STATEMENT_WITH */
        struct {
            Variable* variable;
            Statement* statement;
        } with;

        /* STATEMENT_PROCEDURE_CALL */
        struct {
            Id* id;
            Expression* expressions;
        } procedure_call;

        /* STATEMENT_IO_READ */
        struct {
            Variable* read_list;
        } io_read;

        /* STATEMENT_IO_WRITE */
        struct {
            Expression* write_list;
        } io_write;
    };
} Statement;

//////////////////////////////
/// Functions
//////////////////////////////

Id* ast_id(const char* identifier);

AST* ast_program(Definition* header, Declaration* declarations, Definition* subprograms, Statement* comp_statement);
Declaration* ast_program_declarations(Definition* constdefs, Definition* typedefs, Definition* vardefs);

Definition* ast_definition_program(Line line, Id* id);
Definition* ast_definition_constant(Line line, Id* id, Expression* expression);
Definition* ast_definition_type(Line line, Id* id, Type* type);
Definition* ast_definition_field(Line line, Id* identifiers, Type* type);
Definition* ast_definition_variable(Line line, Id* identifiers, Type* type);
Definition* ast_definition_subprogram_forward(Line line, Definition* sub_header);
Definition* ast_definition_subprogram(Line line, Definition* sub_header, Declaration* declarations, Definition* subprograms, Statement* comp_statement);
Definition* ast_definition_function(Line line, Id* id, Definition* formal_parameters, Type* type);
Definition* ast_definition_procedure(Line line, Id* id, Definition* formal_parameters);
Definition* ast_definition_parameter(Line line, Pass pass, Id* identifiers, Type* type);

Expression* ast_expression_binary(Line line, Operator operator, Expression* left_expression, Expression* right_expression);
Expression* ast_expression_unary(Line line, Operator operator, Expression* expression);
Expression* ast_expression_variable(Line line, Variable* variable);
Expression* ast_expression_function_call(Line line, Id* id, Expression* expressions);
Expression* ast_expression_length(Line line, Expression* expression);
Expression* ast_expression_new(Line line, Expression* expression);
Expression* ast_expression_constant(Line line, Constant* constant);
Expression* ast_expression_enclosed(Line line, Expression* expression);
Expression* ast_expression_setlist(Line line, Expression* expression);
Expression* ast_expression_limits(Line line, Constant* lower_limit, Constant* upper_limit);
Expression* ast_expression_limits_id(Line line, Id* id);
Expression* ast_expression_iter_space(Line line, IterSpace iter_space, Expression* initial_expression, Expression* final_expression);

Variable* ast_variable_id(Line line, Id* id);
Variable* ast_variable_component(Line line, Variable* variable, Id* id);
Variable* ast_variable_index(Line line, Variable* variable, Expression* expressions);
Variable* ast_variable_listfunc(Line line, Id* id, Expression* expression);

Constant* ast_constant_integer(Line line, Operator operator, int integer);
Constant* ast_constant_real(Line line, float real);
Constant* ast_constant_boolean(Line line, const char* boolean);
Constant* ast_constant_character(Line line, char character);
Constant* ast_constant_string(Line line, const char* string);
Constant* ast_constant_id(Line line, Operator operator, Id* id);

Type* ast_type_array(Line line, Expression* dims, Type* type);
Type* ast_type_set(Line line, Type* type);
Type* ast_type_list(Line line, Type* type);
Type* ast_type_record(Line line, Definition* fields);
Type* ast_type_enumeration(Line line, Id* identifiers);
Type* ast_type_subrange(Line line, Constant* lower_limit, Constant* upper_limit);
Type* ast_type_integer();
Type* ast_type_real();
Type* ast_type_boolean();
Type* ast_type_character();
Type* ast_type_string();
Type* ast_type_id(Line line, Id* id);

Statement* ast_statement_assignment(Line line, Variable* variable, Expression* expression);
Statement* ast_statement_if(Line line, Expression* expression, Statement* if_statement, Statement* else_statement);
Statement* ast_statement_while(Line line, Expression* expression, Statement* statement);
Statement* ast_statement_for(Line line, Id* id, Expression* iter_space, Statement* statement);
Statement* ast_statement_with(Line line, Variable* variable, Statement* statement);
Statement* ast_statement_procedure_call(Line line, Id* id, Expression* expressions);
Statement* ast_statement_io_read(Line line, Variable* read_list);
Statement* ast_statement_io_write(Line line, Expression* write_list);

#endif