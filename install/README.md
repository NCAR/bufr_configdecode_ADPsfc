# Compiling and installing the BUFR decoding software

The relevant bufrdecode sub-directories are:
```
exe         For storing the decoder executables.

install     Includes a shell script for compiling and installing the
            BUFR decoder.

src         Contains two Fortran source code files to decode BUFR 
            data files.
```
Edit `install.sh` and set the `CPLAT` variable to match the
platform on which you are running (default value is `CPLAT=linux`).  Then
run the `install.sh` script to compile the source code and build the 
executables.  This will gather source code from the `bufrdecode/src` directory 
and create executables in the `bufrdecode/exe` directory.  Note that you
will also need to compile and install the NCEP BUFRLIB library and provide a 
link to it in `install.sh`.  The NCEP BUFRLIB software is available at 
https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php
