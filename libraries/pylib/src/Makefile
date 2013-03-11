SRCS := $(wildcard *.f)
OBJS = ${SRCS:.f=.o}

FF = gfortran
FFOPTS = -O1 -fno-automatic -Wuninitialized -m64 
# -finit-local-zero

.f.o:
	$(FF) $(FFOPTS) -c $<

all: ../lib/libpythiaext.a

../lib/libpythiaext.a: ${OBJS}
	ar rc ../lib/libpythiaext.a ${OBJS}
