#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrsurface.x which to extract data from ADP BUFR
#  input files, and place the data into a basic text file.  It is used to
#  extract data from these kinds of files:
#      gdas.adpsfc.tHHz.YYYYMMDD.bufr 
#      gdas.sfcshp.tHHz.YYYYMMDD.bufr
#
#  dumpbufr.x:        used to dump all contents of a BUFR file.
#
#  Note: NCEP BUFRLIB library is required to compile this source code,
#        and the location of your local installation of BUFRLIB should
#        be defined in the 'LIB' variable below.  The NCEP BUFRLIB 
#        software is available at
#        https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php
#  ------------------------------------------------------------------------
 
set -eua
 
SRC=../src
LIB=/path/to/bufrlib.a
EXE=../exe
INSTALL=.

cflag=""
fflag=""

export FC=gfortran
export CC=gcc
fflag=" -DUNDERSCORE -fno-second-underscore -w"
cflag=" -DUNDERSCORE -w"

#  Compile the decode programs
#  ---------------------------------------
 
echo "Compiling bufr_configdecode_ADPsfc programs..."
$FC $fflag -c $SRC/dumpbufr.f
$FC $fflag -c $SRC/bufrsurface.f
 
#  link and load the executables
#  -----------------------------

echo "Linking..."
$FC $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB
$FC $fflag -o $EXE/bufrsurface.x bufrsurface.o $LIB

#  clean up
#  --------

rm -f *.o

echo "Finished."
