#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "pasc600.h"
#include "error.h"

void internal_error(const char* error) {

    perror(error);
    exit(EXIT_FAILURE);

}

void scanner_error(int lineno, const char* error, ...) {

    va_list args;
    va_start(args, error);

    printf("\n" ANSI_BWHT "%s:%d: " ANSI_MAG "warning: " ANSI_RESET, filename, lineno);
    vprintf(error, args);
    printf("\n");

    va_end(args);

}

void parser_error(int lineno, const char* error) {

    static unsigned parser_error_count = 0; // yyerrs

    printf("\n" ANSI_BWHT "%s:%d: " ANSI_RED "error: " ANSI_RESET "%s", filename, lineno, error);

    if (++parser_error_count == PARSER_MAX_ERRORS) {
        printf("\n\nParsing error limit (%u) is exceeded. Exiting...\n", PARSER_MAX_ERRORS);
        exit(EXIT_FAILURE);
    }

}

void sem_error(int lineno, const char* error, ...) {

    va_list args;
    va_start(args, error);

    if (lineno == NO_LINE) {
        printf("\n" ANSI_BWHT "%s: " ANSI_MAG "error: " ANSI_RESET, filename);
    } else {
        printf("\n" ANSI_BWHT "%s:%d: " ANSI_MAG "error: " ANSI_RESET, filename, lineno);
    }

    vprintf(error, args);
    printf("\n");

    va_end(args);

    exit(EXIT_FAILURE);

}