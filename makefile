LIB=lib
LEX=lex
YACC=yacc
CC=gcc

.PHONY: all, clean


all: a.out


a.out:

        $(GENTLE) grama.g
        $(REFLEX)
        $(LEX) gen.l
        $(YACC) gen.y

        $(CC) -c y.tab.c
        $(CC) -c lex.yy.c
        $(CC) -c grama.c

        $(CC) -c $(LIB)/idents.c
        $(CC) -c $(LIB)/errmsg.c
        $(CC) -c $(LIB)/output.c
        $(CC) -c $(LIB)/main.c

        $(CC) zad2.o \
                y.tab.o lex.yy.o \
                idents.o errmsg.o output.o main.o \
                -lfl \
                $(GRTS)


clean:
        rm -f a.out b.out *.h *.o *.c *.l *.y *.lit *.tkn
