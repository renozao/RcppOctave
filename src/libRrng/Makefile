# Makefile for the Octave modules used in RcppOctave
#
# Inspired from Dirk Eddelbuettel's randmt Octave module
# http://dirk.eddelbuettel.com/code/octave-mt.html
# Copyright (C) 1999 Dirk Eddelbuettel <edd@debian.org> 
#
# Copyright (C) 2011 Renaud Gaujoux
#
# Released under the GNU General Public License just like Octave

CC = gcc
CFLAGS = -fPIC -Wall -O6 -std=gnu99

FILES = Rrng.cc utils.cc libRrng.h libRrng.c Makefile PKG_ADD
LIB_OBJECTS = libRrng.h snorm.o sexp.o rgamma.o datetime.o

all: Rrng.oct utils.oct

%.oct:
	mkoctfile -c  -I/usr/share/R/include $*.cc 
	mkoctfile $*.o -lR -o $@

#Rrng.oct: Rrng.cc libRrng.o
#	mkoctfile --output Rrng.oct $< *.o 
	
utils.oct: utils.cc
	mkoctfile $<
	
libRrng.o: $(LIB_OBJECTS)

tests: $(LIB_OBJECTS)
	g++ *.c -o tests

snorm.o: fmin2.o fmax2.o qnorm.o

dist:
	tar cvfz Rrng.tar.gz ${FILES}

clean:
	rm -vf *.o *~
	
clean-all: clean
	rm *.oct
