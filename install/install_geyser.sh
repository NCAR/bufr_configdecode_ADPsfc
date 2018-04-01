#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrsurface.x which to extract data from ADP BUFR
#  input files, and place the data into a basic text file.  It is used to
#  extract data from these kinds of files:
#      gdas.adpupa.tHHz.YYYYMMDD.bufr 
#      gdas.aircft.tHHz.YYYYMMDD.bufr
#      gdas.satwnd.tHHz.YYYYMMDD.bufr 
#      gdas.aircar.tHHz.YYYYMMDD.bufr
#
#  dumpbufr.x:        used to dump all contents of a BUFR file.
#  ** Make sure the "ar" command location has been set in your path
#  environment variable.  Type "which ar" to check if this is done. **
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=/glade/apps/opt/BUFRLIB/11.0.0/intel/12.1.5/lib  # path to BUFRLIB
EXE=../exe
INSTALL=.

#  different platforms use different link name protocols
#  -----------------------------------------------------

# if using linux, BUFR files must be run through the "grabbufr/grabbufr.sh" script
# with the resulting output used as input for the decoders.  Set appropriate compiler
# in grabbufr.sh, and exe/convert.csh
 
cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   CC=/glade/apps/opt/cmpwrappers/icc    # activated 2014.02.27
   FC=/glade/apps/opt/cmpwrappers/ifort  # activated 2014.02.27
   fflag=""
   cflag="-DUNDERSCORE"

#   export FC=gfortran
#   export CC=gcc
#   fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
#   cflag=" -O3 -DUNDERSCORE -w"
fi

#  Compile and archive the Bufr Library
#  ------------------------------------
#echo "Compiling BUFRLIB Library..."
#cd $LIB
#if [ -e bufrlib.a ]
#then
#  rm bufrlib.a
#fi
#$LIB/makebufrlib.sh
#cd $INSTALL

#  Compile the decode programs
#  ---------------------------------------
 
echo "Compiling bufr_configdecode_ADPupa programs..."
$FC $fflag -c $SRC/dumpbufr.f
$FC $fflag -c $SRC/bufrsurfacefd.f
$FC $fflag -c $SRC/bufrsurface_if.f
 
#  link and load the executables
#  -----------------------------

echo "Linking..."
$FC $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/libbufr.a
$FC $fflag -o $EXE/bufrsurfacefd.x bufrsurfacefd.o $LIB/libbufr.a
$FC $fflag -o $EXE/bufrsurface_if.x bufrsurface_if.o $LIB/libbufr.a

#  clean up
#  --------

rm -f *.o

echo "Finished."

