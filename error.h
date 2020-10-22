#ifndef ERROR_H
#define ERROR_H

//////////////////////////////
/// ANSI escape codes
//////////////////////////////

#define ANSI_RED    "\e[0;31m"
#define ANSI_MAG    "\e[0;35m"
#define ANSI_WHT    "\e[0;37m"
#define ANSI_BWHT   "\e[1;37m"
#define ANSI_RESET  "\e[0m"

//////////////////////////////
/// Error types
//////////////////////////////

#define WARNING     1
#define ERROR       2

//////////////////////////////
/// Scanner errors
//////////////////////////////

#define SCANNER_LINE_FEED_IN_STRING                         "Newline (line feed) in string: \"%s\""
#define SCANNER_EOF_IN_STRING                               "EOF in string: \"%s\""
#define SCANNER_UNKNOWN_CHARACTER_IN_STRING                 "Unknown character '%c' (ASCII value: %d) in string: \"%s\""
#define SCANNER_INVALID_ESCAPE_SEQUENCE_IN_CHARACTER        "Invalid escape sequence '\\%c' (ASCII value: %d) in character: %s"
#define SCANNER_INVALID_ESCAPE_SEQUENCE_IN_STRING           "Invalid escape sequence '\\%c' (ASCII value: %d) in string: \"%s\""
#define SCANNER_EOF_IN_COMMENT                              "EOF in comment"
#define SCANNER_UNKNOWN_CHARACTER                           "Unknown character '%c' (ASCII value: %d)"

//////////////////////////////
/// Semantic errors
//////////////////////////////

/* ID */

#define SEM_ID_UNDECLARED                                   "Undeclared identifier '%s'"
#define SEM_ID_REDECLARATION                                "Redeclaration of identifier '%s'. Previously declared as '%s' at line %d"

/* DEFINITION_CONSTANT */

#define SEM_DEFINITION_INVALID_CONSTANT_EXPRESSION          "'%s': Constant's expression doesn't produce a constant value"

/* DEFINITION_SUBPROGRAM */

#define SEM_DEFINITION_SUBPROGRAM_NESTING_LIMIT_EXCEEDED    "Subprogram nesting limit (%u) is exceeded"
#define SEM_DEFINITION_SUBPROGRAM_REDEFINITION              "Subprogram '%s' has been already defined"
#define SEM_DEFINITION_SUBPROGRAM_UNDEFINED                 "Subprogram '%s' was declared but not defined"

/* DEFINITION_FUNCTION */

#define SEM_DEFINITION_FUNCTION_PARAMETER_COUNT             "Function '%s''s definition takes no parameters while declaration does"     // TODO: name. merge at definition_subprogram?
#define SEM_DEFINITION_FUNCTION_NO_PARAMETERS               "Function '%s' takes no parameters - should take at least one"
#define SEM_DEFINITION_FUNCTION_INVALID_RETURN_TYPE         "Function '%s' has invalid return type"

/* DEFINITION_PROCEDURE */

#define SEM_DEFINITION_PROCEDURE_PARAMETER_COUNT_1          "Procedure '%s''s definition takes parameters while declaration does not"   // TODO: name. merge at definition_subprogram?
#define SEM_DEFINITION_PROCEDURE_PARAMETER_COUNT_2          "Procedure '%s''s definition takes no parameters while declaration does"    // TODO: name. merge at definition_subprogram?

/* DEFINITION_PARAMETER */

#define SEM_DEFINITION_PARAMETER_INVALID_PASS               "Parameters with string or complex type should be passed by reference"
#define SEM_DEFINITION_PARAMETER_ID_UNDECLARED              "Subprogram '%s''s declaration's parameter's (no. %d) identifier is not '%s'"
#define SEM_DEFINITION_PARAMETER_ID_REDECLARATION           ""
#define SEM_DEFINITION_PARAMETER_INCORRECT_COUNT            ""
#define SEM_DEFINITION_PARAMETER_DIFFERENT_PASS             "Parameter '%s' is passed differently in subprogram's declaration"
#define SEM_DEFINITION_PARAMETER_DIFFERENT_TYPE             "Parameter '%s' has different type in subprogram's declaration"

/* EXPRESSION_FUNCTION_CALL */

#define SEM_EXPRESSION_FUNCTION_CALL_NOT_SUBPROGRAM         "'%s' is not declared as subprogram. It is declared as '%s' at line %d"
#define SEM_EXPRESSION_FUNCTION_CALL_NOT_FUNCTION           "Subprogram '%s' is declared as subprogram but not as function"
#define SEM_EXPRESSION_FUNCTION_CALL_ARGUMENTS              "Function '%s' takes exactly %d argument(s) (%d given)"

/* EXPRESSION_LENGTH */

#define SEM_EXPRESSION_LENGTH_INVALID_PARAMETER_TYPE        "Expression's parameter is invalid. It is neither a list nor string"

/* EXPRESSION_ITER_SPACE */

#define SEM_EXPRESSION_ITER_SPACE_INITIAL_EXPRESSION_TYPE   "Iteration space's initial expression is not of type 'integer'"
#define SEM_EXPRESSION_ITER_SPACE_FINAL_EXPRESSION_TYPE     "Iteration space's final expression is not of type 'integer'"

/* CONSTANT_STRING */

#define SEM_CONSTANT_STRING_LENGTH_LIMIT_EXCEEDED           "String's length (%u characters) is exceeding the limit (%u characters)"

/* TYPE_SUBRANGE */

#define SEM_TYPE_SUBRANGE_DIFFERENT_LIMIT_TYPE              "'%s': Subrange's lower limit's type is different than upper limit's type"
#define SEM_TYPE_SUBRANGE_LOWER_LIMIT_GREATER_EQUAL         "'%s': Subrange's lower limit's value is greater than or equal to upper limit's value"
#define SEM_TYPE_SUBRANGE_INVALID_LIMIT_TYPE                "'%s': Subrange's limits' type is invalid"

/* STATEMENT_IF */

#define SEM_STATEMENT_IF_INVALID_EXPRESSION_TYPE            "Expression of 'if' statement is not of type 'boolean'"

/* STATEMENT_WHILE */

#define SEM_STATEMENT_WHILE_INVALID_EXPRESSION_TYPE         "Expression of 'while' statement is not of type 'boolean'"

/* STATEMENT_FOR */

#define SEM_STATEMENT_FOR_ITERATOR_NOT_VARIABLE             "'%s': Statement's iterator is not a variable. It is declared as '%s' at line %d"
#define SEM_STATEMENT_FOR_INVALID_ITERATOR_TYPE             "'%s': Statement's iterator is not of type integer"
#define SEM_STATEMENT_FOR_ITERATOR_USAGE_RESTRICTED         "'%s': Statement's iterator cannot be used inside the current statement"

/* STATEMENT_PROCEDURE_CALL */

#define SEM_STATEMENT_PROCEDURE_CALL_NOT_SUBPROGRAM         "'%s' is not declared as subprogram. It is declared as '%s' at line %d"
#define SEM_STATEMENT_PROCEDURE_CALL_NOT_PROCEDURE          "Subprogram '%s' is declared as subprogram but not as procedure"
#define SEM_STATEMENT_PROCEDURE_CALL_ARGUMENTS              "Procedure '%s' takes exactly %d argument(s) (%d given)"

//////////////////////////////
/// Error report functions
//////////////////////////////

void internal_error(const char* error);
void scanner_error(int lineno, const char* error, ...);
void parser_error(int lineno, const char* error);
void sem_error(int lineno, const char* error, ...);

#endif