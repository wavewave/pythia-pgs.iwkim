STDHEP_DIR = ../libraries/PGS4
PYDIR = ../libraries/pylib
PGS = ../libraries/PGS4
LHAPDF = ../libraries/lhapdf
THISDIR = ${PWD}

UNAME = $(shell uname)
SUNAME = $(shell uname | sed s/[_-].*//)
ifeq "$(SUNAME)" "CYGWIN"
    XLIBS1 = -Wl,-u,_WinMain@16
    XLIBS2 = -lrpc -lg2c -lm
endif
ifeq "$(UNAME)" "Darwin"
    XLIBS2 = -lSystemStubs
endif

FF = gfortran
FFLAGS = -O -fno-automatic -I$(PGS)/src -m64

LDIRS = -L${STDHEP_DIR}/lib -L${PYDIR}/lib -L${LHAPDF}/lib 
LIBS1 = ${XLIBS1} -ltauola -lpythiaext -lstdhep -lFmcfio \
	-lLHAPDF ${XLIBS2}

OBJS = 	ME2pythia.o getjet.o ktclusdble.o pgs_ranmar.o
LIBS = 	libstdhep libpythia libLHAPDF libtauola libFmcfio
LINKS = mass_width_2004.mc pgs clean_output PDFsets

.f.o:
	${FF} ${FFLAGS} -c $<

all: ${LINKS} pythia hep2lhe

cleanall: clean cleanlibs

clean:
	rm -f pythia hep2lhe *.o ${LINKS}

cleanlibs:
	cd ${PGS}; make clean; rm -v lib/*; cd ..; \
	cd ${PYDIR}; make clean; rm -v lib/*; cd ..; \
	cd ${LHAPDF}; make clean; rm -v lib/*; cd ..;

pythia: pythia.o ${OBJS} ${LIBS}
	${FF} ${FFLAGS} -o $@ $< ${OBJS} ${LDIRS} ${LIBS1}

hep2lhe: hep2lhe.o ${OBJS} ${LIBS}
	${FF} ${FFLAGS} -o $@ $< ${OBJS} ${LDIRS} ${LIBS1}

pgs:    $(PGS)/examples/olympics
	ln -sf $(PGS)/examples/olympics pgs

clean_output:  $(PGS)/examples/clean_output
	ln -sf $(PGS)/examples/clean_output

libstdhep:
	cd ${STDHEP_DIR}; mkdir -p lib; make stdhep; make mcfio

libFmcfio:
	cd ${STDHEP_DIR}; mkdir -p lib; make mcfio

libtauola:
	cd ${STDHEP_DIR}; mkdir -p lib; make tauola

libpythia:
	cd ${PYDIR}; mkdir -p lib; make all

libpgs:
	cd ${PGS}; mkdir -p lib; make all

libLHAPDF:
	cd ${LHAPDF}; mkdir -p lib; ./configure --prefix=`pwd` --datadir=${THISDIR}; \
	make; make install

mass_width_2004.mc:
	ln -sf ${STDHEP_DIR}/examples/mass_width_2004.mc  .        

PDFsets:
	ln -sf lhapdf/PDFsets

$(PGS)/examples/olympics:
	cd $(PGS);make examples

$(PGS)/examples/clean_output:
	cd $(PGS);make examples
