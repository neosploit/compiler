BISON = bison
BISON_FLAGS = -v -d
FLEX = flex
CC_FLAGS = -lm -Wall

PASC600 = pasc600.c pasc600.h
ERROR = error.c error.h
AST = ast.c ast.h ast_print.c ast_print.h ast_free.c ast_free.h
UTHASH = uthash/src/uthash.h
LIBFORT = libfort/lib/fort.c libfort/lib/fort.h

all: pasc600

pasc600: parser.tab.c lex.yy.c $(PASC600) $(ERROR) $(AST) $(UTHASH) $(LIBFORT)
	$(CC) parser.tab.c lex.yy.c pasc600.c error.c ast.c ast_print.c ast_free.c libfort/lib/fort.c -o $@ $(CC_FLAGS)

lex.yy.c: parser.tab.h scanner.l
	$(FLEX) scanner.l

parser.tab.c parser.tab.h: parser.y
	$(BISON) $(BISON_FLAGS) parser.y

clean:
	rm -f pasc600 lex.yy.c parser.output parser.tab.c parser.tab.h