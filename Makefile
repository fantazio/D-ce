OCC?=ocamlopt
OCC_FLAGS?= -c -w A -warn-error
SRC=
SRC_TEST=
RES=duce
RES_TEST=./check/test

all:
	${OCC} ${SRC} -o ${RES}

check: all
	${OCC} ${SRC_TEST} -o ${RES_TEST}
	./${RES_TEST}

clean:
	rm -rf ${RES} ${RES_TEST}
