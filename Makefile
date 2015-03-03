OCC?=ocamlopt
OCC_FLAGS?= -c -w A -warn-error
SRC=
MENHIRSRC = src/parser.mly
SRC_TEST=
RES=duce
RES_TEST=check/test
CLEAN= ${RES} ${RES_TEST} _build \
    src/parser.ml src/parser.mli src/parser.automaton

all: menhir
	${OCC} ${SRC} -o ${RES}

menhir:
	menhir -v ${MENHIRSRC}

check: all
	${OCC} ${SRC_TEST} -o ${RES_TEST}
	./${RES_TEST}

clean:
	rm -rf ${CLEAN} ${RES} ${RES_TEST} _build
