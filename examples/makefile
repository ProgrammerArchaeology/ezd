.SUFFIXES:
.SUFFIXES: .sc .o

SCFLAGS = -O
XLIB = /udir/bartlett/mips/bin/scxl.a -lX11
EZD = ../libezd.a

all:	dragon puzzle_in_c puzzle clock

.sc.o:
	scc -c ${SCFLAGS} $*.sc

dragon:	${EZD} dragon.o
	scc -o dragon ${SCFLAGS} dragon.o ${EZD} ${XLIB}

xc:	${EZD} xc.o
	scc -o xc ${SCFLAGS} xc.o ${EZD} ${XLIB}

puzzle_in_c:	puzzle_in_c.c
	cc -o puzzle_in_c puzzle_in_c.c

puzzle:	${EZD} puzzle.o
	scc -o puzzle ${SCFLAGS} puzzle.o ${EZD} ${XLIB}

clock:	clock.c
	cc -o clock clock.c -lm

clean:
	rm -f dragon.o dragon.c puzzle.o puzzle.c clock.o

noprogs:
	rm -f dragon puzzle puzzle_in_c clock
