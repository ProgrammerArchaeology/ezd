.SUFFIXES:
.SUFFIXES: .sc .o

OBJS =	ezd.o pps.o commands.o ginfo.o display.o window.o view.o drawing.o \
	graphic.o rectangle.o line.o text.o arc.o psdraw.o events.o \
	interact.o stringinput.o popupmenu.o buttons.o slider.o textree.o \
	jtextree.o textdrawing.o mouseedit.o editcolor.o struct.o match.o \
	quilt.o transpbuttons.o

SCFLAGS = -O

SCXL = /usr/local/lib/scheme2c/scxl.a
SC = /usr/local/lib/scheme2c/libsc.a
X11 = -lX11 -L/opt/X11/lib

.sc.o:
	scc -c ${SCFLAGS} $*.sc

ezd:	${OBJS} ezdmain.o
	scc -o ezd ${SCFLAGS} ezdmain.o ${OBJS} ${SCXL} ${X11}

libezd.a:	${OBJS}
	rm -f libezd.a
	ar q libezd.a ${OBJS}
	ranlib libezd.a

DECstation-ezd.o:	${OBJS}
	ld -o DECstation-ezd.o -r ezdmain.o ${OBJS} ${SCXL} ${SC}

ezd-for-DECstation:
	cc -o ezd DECstation-ezd.o ${X11} -lm

clean:
	rm -f ${OBJS} ezdmain.o DECstation-ezd.o

noprogs:
	rm -f ezd libezd.a

tartape:
	tar cvf ezd.tar README DECstation-ezd.o *.sc *.sch makefile \
		doc/ezd.1 doc/ezd.psf \
		examples/clock.c examples/clock.sc examples/dragon.sc \
		examples/puzzle.sc examples/puzzle_in_c.c examples/xc.sc \
		examples/makefile
	rm -f ezd.tar.Z
	compress ezd.tar

all:	ezd libezd.a DECstation-ezd.o
