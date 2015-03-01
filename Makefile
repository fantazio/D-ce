OCC?=ocamlopt
OCC_FLAGS?= -c -w A -warn-error
SRC=
SRC_TEST=check/*
RES=duce
CHECKRES=check/test

all:
	${OCC} ${SRC} -o ${RES}

check: all
	${OCC} ${CHECKSRC} -o ${CHECKRES}
	./${CHECKRES}

clean:
	rm -rf ${RES} ${CHECKRES}
