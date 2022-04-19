# Configuration file user guide and templates

The `/configs` directory contains sample configuration files which can
be provided to the `bufrsurface.f` executable.

The parameters to be extracted are identified by mnemonics.  If the set of
default parameters defined in `bufrsurface.f` is sufficient, you will not
need to know other mnemonics.  To add more parameters, please refer
to a list of mnemonics, some of which are shown in `../docs/mnemonics.txt`.  

A complete list of mnemonics is available in the NCEP BUFR document:
https://www.emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib/tables/bufrtab_tableb.html.

Following is a list and description of the sample configurations.
```
CONFIGURATION FILE               DESCRIPTION
-------------------------------------------------------------------------------------------
bufrsurface_config_template     A template with a description of the options and
                                  how to enter them.

bufrsurface_config_all          Gets everything from the given ADPSFC or SFCSHP file

bufrsurface_config_circles      Gets only the stations within a couple of circles

bufrsurface_config_gmeq         Gets only the stations inside a latitude-
                                  longitude window including the place where the
                                  Greenwich Meridian meets the equator.

bufrsurface_config_high         Gets only the stations within the specified range
                                  of elevation

bufrsurface_config_idl          Gets only the stations inside a latitude-
                                  longitude window for the Bering Strait region,
                                  on either side of the International Date Line.
                                  Note that this includes where the longitude
                                  changes from maximum west (-179.9) to maximum
                                  east (179.9)

bufrsurface_config_US_def       Gets a latitude-longitude window which includes
                                  the 48 contiguous United States

bufrsurface_config_wbb          Gets only the SYNOP stations from the given
                                  WMO blocks

bufrsurface_config_wmo          Gets only the specified SYNOP stations
```
