#include <stdio.h>
#include <stdlib.h>
#include "pasc600.h"
#include "ast.h"
#include "ast_free.h"
// #include "sem.h"

const char* filename;
AST* ast;

int main(int argc, char* argv[]) {

    if (argc == 2) {
        filename = argv[1];

        if (!(yyin = fopen(filename, "r"))) {
            perror(filename);
            exit(EXIT_FAILURE);
        }

        if (DEBUG) printf("-> Line No. 1 <-");

        yyparse();
        fclose(yyin);

        // sem_analysis(ast);

        yylex_destroy();
        ast_free(ast);

        printf("\n");
        exit(EXIT_SUCCESS);
    }

    printf("\nUsage: %s {filename}\n", argv[0]);
    exit(EXIT_FAILURE);

}