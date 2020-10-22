#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pasc600.h"
#include "symtab.h"
#include "uthash/src/uthash.h"
#include "libfort/lib/fort.h"

SymTab* symtab_initialize() {

    SymTab* symtab = MALLOC(symtab, SymTab);

    symtab->current_scope = NULL;

    return symtab;

}

void symtab_finalize(SymTab* symtab) {

    free(symtab);

}

Scope* symtab_enter_scope(SymTab* symtab) {

    Scope* new_scope = MALLOC(new_scope, Scope);

    new_scope->hash_table = NULL;
    new_scope->previous = symtab->current_scope;
    new_scope->next = NULL;

    if (new_scope->previous) new_scope->previous->next = new_scope;

    symtab->current_scope = new_scope;

    return symtab->current_scope;

}

Scope* symtab_leave_scope(SymTab* symtab) {

    Scope* old_scope = symtab->current_scope;
    Symbol* symbol, *tmp;

    symtab->current_scope = old_scope->previous;

    if (symtab->current_scope) symtab->current_scope->next = NULL;

    HASH_ITER(hh, old_scope->hash_table, symbol, tmp) {
        HASH_DEL(old_scope->hash_table, symbol);
        free(symbol);
    }

    free(old_scope);

    return symtab->current_scope;

}

void symtab_print_scope(SymTab* symtab) {

    Symbol* symbol, *tmp;
    ft_table_t* table = ft_create_table();

    ft_set_cell_prop(table, 0, FT_ANY_COLUMN, FT_CPROP_ROW_TYPE, FT_ROW_HEADER);
    ft_write_ln(table, "Identifier");

    HASH_ITER(hh, symtab->current_scope->hash_table, symbol, tmp) {
        ft_printf_ln(table, "%s", symbol->identifier);
    }

    printf("\n\n%s", ft_to_string(table));
    ft_destroy_table(table);

}

void symtab_dump_scope(SymTab* symtab, const char* filename) {

    Symbol* symbol, *tmp;
    FILE* fp = fopen(filename, "w");
    ft_table_t* table = ft_create_table();

    if (fp) {
        ft_set_cell_prop(table, 0, FT_ANY_COLUMN, FT_CPROP_ROW_TYPE, FT_ROW_HEADER);
        ft_write_ln(table, "Identifier");

        HASH_ITER(hh, symtab->current_scope->hash_table, symbol, tmp) {
            ft_printf_ln(table, "%s", symbol->identifier);
        }

        fprintf(fp, "%s", ft_to_string(table));
        fclose(fp);
    } else {
        perror(filename);
    }

    ft_destroy_table(table);

}

Symbol* symtab_add_symbol(SymTab* symtab, const char* identifier) {

    Symbol* symbol = MALLOC(symbol, Symbol);

    symbol->identifier = strdup(identifier);
    HASH_ADD_STR(symtab->current_scope->hash_table, identifier, symbol);

    return symbol;

}

Symbol* symtab_search_symbol(SymTab* symtab, SearchScope search_scope, const char* identifier) {

    Symbol* symbol;
    Scope* scope;

    if (search_scope == CURRENT_SCOPE) {
        HASH_FIND_STR(symtab->current_scope->hash_table, identifier, symbol);
        if (symbol) return symbol;
    } else {
        for (scope = symtab->current_scope; scope; scope = scope->previous) {
            HASH_FIND_STR(scope->hash_table, identifier, symbol);
            if (symbol) return symbol;
        }
    }

    return NULL;

}