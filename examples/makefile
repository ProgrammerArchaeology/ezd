.SUFFIXES:
.SUFFIXES: .sc .o

SCFLAGS = -O
XLIB = /udir/bartlett/mips/bin/scxl.a -lX11
EZD = ../libezd.a

.sc.o:
	scc -c ${SCFLAGS} $*.sc

dragon:	${EZD} dragon.o
	scc -o dragon ${SCFLAGS} dragon.o ${EZD} ${XLIB}

xc:	${EZD} xc.o
	scc -o xc ${SCFLAGS} xc.o ${EZD} ${XLIB}

goodbye:	goodbye.c
	cc -o goodbye goodbye.c

goodbye2:	goodbye2.c
	cc -o goodbye2 goodbye2.c

puzzle_in_c:	puzzle_in_c.c
	cc -o puzzle_in_c puzzle_in_c.c

color:	${EZD} color.o
	scc -o color ${SCFLAGS} color.o ${EZD} ${XLIB}

puzzle:	${EZD} puzzle.o
	scc -o puzzle ${SCFLAGS} puzzle.o ${EZD} ${XLIB}

clock:	clock.c
	cc -o clock clock.c -lm

clean:
	rm -f dragon.o dragon.c color.o color.c puzzle.o puzzle.c clock.o

noprogs:
	rm -f color dragon goodbye goodbye2 puzzle puzzle_in_c clock

all:	color dragon goodbye goodbye2 puzzle_in_c puzzle clock
