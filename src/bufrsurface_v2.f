C ##############################################################################
C #     PROGRAM bufrsurface_v2.f                                               #
C #                                                                            #
C #      A BUFR INPUT DATA FILE CONTAINS A SERIES OF "MESSAGES" (WHICH ARE     #
C #        VARIABLE LENGTH RECORDS), EACH CONTAINING AT LEAST ONE BUFR         #
C #        "SUB-MESSAGE" (REPORT).  THIS PROGRAM BREAKS THESE OPEN AND PRINTS  #
C #        OUT THE REPORTS, WITH OPTIONS PROVIDED BY THE USER'S CONFIGURATION  #
C #        FILE.                                                             #
C ##############################################################################
C
        CHARACTER*1 DODIAG        ! SET BY DSS STAFF (NOT BY USERS) TO OBTAIN
C                                 !   EXECUTION OR PERFORMANCE DIAGNOSTICS
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        PARAMETER  ( IIUNIT=11 )  ! BUFR INPUT FILE UNIT
        PARAMETER  ( IPUNIT=21 )  ! OUTPUT FILE UNIT 
        PARAMETER  ( ICUNIT=8 )   ! CONFIGURATION INPUT FILE
        PARAMETER  ( IDUNIT=7 )   ! DIAGNOSTIC OUTPUT FILE

        PARAMETER  ( LENINMX=128 )   ! MAXIMUM LENGTH OF INPUT BASE FILE NAMES
        CHARACTER*128 INFILE

        CHARACTER*128 input_file, output_file, config_file, argv
        CHARACTER*32 diag_file
        DATA diag_file /'bufrsurface_diagnostics         '/

        INTEGER NARG              ! NUMBER OF COMMAND-LINE ARGUMENTS

C       BUFR INPUT DATA FILE NAMES 
C           gdas.adpsfc.t00z.20100323.bufr
C           gdas.sfcshp.t00z.20100323.bufr
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       VALUES FOR DATA PROCESSING ARRAY DIMENSIONS.  
C         USERS SHOULD NOT MAKE CHANGES TO THESE.
C
        PARAMETER (MXMN=16)
C       PARAMETER (MXREPL=240)
        PARAMETER (MXREPL=1000)
        PARAMETER (MXBF=16000 )
        PARAMETER (NEMLIM=100)
C
C       THIS COMMON BLOCK IS CONNECTED TO BUFRLIB IN MYSTERIOUS WAYS.  
C         USERS SHOULD NOT MAKE CHANGES TO THIS.
C
        COMMON /BITBUF/ MAXBYT,IBIT,IBAY(5000),MBYT(32),MBAY(5000,32)
C
        CHARACTER*1000 CSTRING          ! FOR READING ENTRIES FROM THE
C                                       !   CONFIGURATION FILE
        PARAMETER (NEMSCAN=1000)        ! NUMBER OF CHARACTERS IN CSTRING
        INTEGER INDEX, IDX              ! CSTRING INDEX NUMBERING
C
        CHARACTER*1    DEFAULT, IDOH, IDOHDR
C
        CHARACTER*8    CSUBSET    ! HAS THE CODED RECORD TYPE AND OBSERVATION TYPE
        CHARACTER*6    RECTYPE    ! DECODED RECORD TYPE
        CHARACTER*8    OBSTYPE    ! DECODED OBSERVATION TYPE
        CHARACTER*8    A8RPID
        DATA A8RPID / '        ' /
C
        CHARACTER*6    RECGET(20)       ! FOR LIST OF RECORDS TO GET
        CHARACTER*1    IRECDO
C
C       RECORD AND REPORT COUNTERS
C
        INTEGER        RECORDS, RECSREJ, RECSACC, RECREPS, REPORTS
        INTEGER        REPSACC, REPSREJ
C
        CHARACTER*1    IFILTER
        CHARACTER*8    OBSGET(1000)
        CHARACTER*1    IOBSDO
C
        CHARACTER*1    DATEDO
        CHARACTER*1    DATEOK
        CHARACTER*6    RRLEV
C
        CHARACTER*1    KELCEL               ! 'k'/'c'  KELVIN / CELSIUS 
C
        CHARACTER*8    NEMLIST(NEMLIM)  ! FOR EXTRA PARAMETERS
        CHARACTER*1    NMDO
        CHARACTER*10   XN      ! FOR BLANK FILLING IN MNEMONIC USAGE
        DATA XN      / '          ' /
C
C
C
C
        CHARACTER*1    LLDO
        CHARACTER*1    LLWRAP
C
        CHARACTER*1    LLRDO
C
        CHARACTER*1    WMODO
        CHARACTER*5    WMOLIST(100)
        CHARACTER*1    WBBDO
        CHARACTER*2    WBBLIST(100)
C
        CHARACTER*1    IELEVDO
C
        INTEGER        PLEVL, PLEVH
        CHARACTER*1    PLEVDO
C
        CHARACTER*1    ACK
C
C       NEXT TWO STRING SIZES LIMITED BY BUFR LIBRARY TO 80 CHARACTERS.
C         SEE ROUTINE string.f
C
C       MAXIMUM NUMBER OF PARAMETERS RETURNED BY UFBINT FOR A MNEMONIC REQUEST
C         (OF UP TO 80 CHARACTERS) IS 80 / 5 = 16 (MXMN).  COMMON MNEMONICS ARE
C         USUALLY 4 CHARACTERS, OTHERS CAN BE AS LONG AS 8.  SEE:
C         http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm
C         ONE OR MORE OF THE PARAMETERS MAY BE "REPLICATED"
C
        CHARACTER*80 QIDENT               ! REPORT IDENTIFICATION TABLE B MNEMONICS
C
        CHARACTER*80 QBPARM               ! BASIC PARAMETER TABLE B MNEMONICS
C
        REAL*8       R8IDENT(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN QIDENT
        REAL*8       R8BPARM(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN QBPARM
        REAL*8       R8XPARM(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN NEMLIST
C
        CHARACTER*1 ISBPARM, ISXPARM
C
C
C       A STRING OF MNEMONICS PROVIDED TO UFBINT CAN NOT INVOLVE MORE THAN ONE
C         "REPLICATION GROUP."  FROM "GUIDE TO WMO TABLE DRIVEN CODE FORMS:"
C           REPLICATION IS THE REPEATING OF A SINGLE PARAMETER OR A GROUP OF
C           PARAMETERS SOME NUMBER OF TIMES, AS IN A TEMP OR PILOT REPORT
C           WITH MANY LEVELS.
C
C       THE BUFRLIB ROUTINE PARUSER WILL COMPLAIN ABOUT INPUT STRING STORE
C         NODES (MNEMONICS), WHEN REPLICATION GETS BROKEN SOMEHOW.
C
        REAL*8 R8CLAT, R8CLON
        REAL*8 R8PRES, R8PMSL, R8WDIR, R8WSPD
C
        REAL*8 R8TMDB, R8TMDP, R8REHU
C
        REAL*8 R8BIG
        DATA R8BIG / 9999999999.0 /
C
        CHARACTER*8  CHSTR
C
        REAL*8       EXTRA(100)         ! FOR EXTRA PARAMETERS (BEYOND THE BASICS)
C
        CHARACTER    MINUTE*2
C
        REAL LATR(100), LONR(100)
        INTEGER RADR
        INTEGER RECDATE, I8DATE, IBEGDAT, IENDDAT
C
        INTEGER   I500, I30     
        DATA I500, I30 / 500, 30 /
C
C       DO NOT CHANGE THESE NEXT TWO DECLARATIONS (THE INTEGER AND THE
C         EQUIVALENCE)
C
        INTEGER IBFMSG(MXBF/4), LN, CODE, Y, Z, JJ, IARGC, N
        CHARACTER    CBFMSG*(MXBF)
C
        EQUIVALENCE (CBFMSG(1:4),IBFMSG(1))     ! NEITHER ARE USED ANYWHERE?
C
        CHARACTER*1200 DUMPHED(3)
C
        CHARACTER*1 DOHEAD, PARSEOK
C
C-----7---------------------------------------------------------------72
C     END OF DECLARATIONS  ############## BEGIN INSTRUCTIONS
C-----7---------------------------------------------------------------72
C
C       THESE NEXT TWO STRING SIZES ARE LIMITED BY THE BUFR
C         LIBRARY - SEE ROUTINE string.f
C
        QIDENT(001:025) = 'WMOB WMOS RPID CLAT CLON '
        QIDENT(026:050) = 'SELV YEAR MNTH DAYS HOUR '
        QIDENT(051:075) = 'MINU                     '
        QIDENT(076:080) = '     '
C
        QBPARM(001:025) = '     PRES PMSL ALSE WDIR '
        QBPARM(026:050) = 'WSPD TMDB TMDP REHU HOVI '
        QBPARM(051:075) = 'TP03 TP24                '
        QBPARM(076:080) = '     '
C
        NNBASIC = 11    ! MUST EQUAL THE NUMBER OF BASIC PARAMETER MNEMONICS IN QBPARM
C
C       SET THE HEADER STRINGS (FOR THE DEFAULT MODE - LATER RESET IF
C         NOT IN DEFAULT MODE)
C       2010.07.01 - LET'S NOT SHOW THE RECORD DATE, NOR USE IT TO
C                      SELECT RECORDS.  DO NOT SHOW THE CORRECTION FLAG
C       ================================================================
        DUMPHED(1)(001:028) = ' REC      OBS       REPORT T'
        DUMPHED(1)(029:068) = 'IME   STATION   LATI-   LONGI-   ELEV   '
        DUMPHED(1)(069:108) = 'STN PR  STN DSLP  ALTIM     AIR.T    DEW'
        DUMPHED(1)(109:148) = 'PT    R.HUM    WIND     WIND      HOR   '
        DUMPHED(1)(149:188) = '  3H PR   24H PR    |                   '
        IHDEND = 171
        DUMPHED(1)(189:200) = '            '
C
        DUMPHED(1)(201:240) = '                                        '
        DUMPHED(1)(241:280) = DUMPHED(1)(201:240)
        DUMPHED(1)(281:360) = DUMPHED(1)(201:280)
        DUMPHED(1)(361:520) = DUMPHED(1)(201:360)
        DUMPHED(1)(521:840) = DUMPHED(1)(201:520)
        DUMPHED(1)(841:1200)= DUMPHED(1)(201:560)
C
        DUMPHED(2)(001:028) = ' TYPE     TYPE      YYYYMMDD'
        DUMPHED(2)(029:068) = 'HHMM  BBSSS     TUDE     TUDE     (M)   '
        DUMPHED(2)(069:108) = '(HP=MB)  (HP=MB)  (HP=MB)     (C)      ('
        DUMPHED(2)(109:148) = 'C)     (%)     DIR    SPD(M/S)   VIS(M) '
        DUMPHED(2)(149:188) = ' (KG/M2)  (KG/M2)   |                   '    
        IHDEND = 171
        DUMPHED(2)(189:200) = '            '
C
        DUMPHED(2)(201:240) = '                                        '
        DUMPHED(2)(241:280) = DUMPHED(2)(201:240)
        DUMPHED(2)(281:360) = DUMPHED(2)(201:280)
        DUMPHED(2)(361:520) = DUMPHED(2)(201:360)
        DUMPHED(2)(521:840) = DUMPHED(2)(201:520)
        DUMPHED(2)(841:1200)= DUMPHED(2)(201:560)
C       ================================================================
C
        XMSG = -9999.9
        XN = '          '
C
C-----7---------------------------------------------------------------72
C    INITIALIZE SELECTION CONFIGURATION AND SET DEFAULTS
C-----7---------------------------------------------------------------72
C
        DODIAG = 'n'
        DEFAULT = 'y'           ! WHEN SET TO 'n', PROGRAM WILL GET ONLY THE
C                               !   PARAMETERS SPECIFIED BY THE USER. E.G. JUST
C                               !   ONE, LIKE TEMPERATURE
C
C-------
        IRECBEG = 1
        IRECEND = 10
        DO N = 1, 20
          RECGET(N) = '        '
        ENDDO
        NREC = 0
        IRECDO = 'n'
C
        OBSGET(1) = '        '  ! INDEX 026   n/a                 BUFR (report) types to get
        IOBSBEG = 1
        IOBSEND = 8
        DO N = 2, 50
          OBSGET(N) = '        '
        ENDDO
        NOBS = 0
        IOBSDO = 'n'
C-------
        DO N = 1, NEMLIM
          NEMLIST(N) = '        ' ! INDEX 041   e.g. PMSL           list of extra mnemonics (parameters)
C                       see  http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm
        ENDDO
        NML = 0
        INBEG = 1
        INEND = INBEG + 14
        NMDO = 'n'
C-------
        LATS = -90              ! INDEX 051   LAT                 southern latitude of a lat-lon box
        LATN = 90               ! INDEX 051   LAT                 northern latitude of a lat-lon box
        LONW = -180             ! INDEX 051   LON                 western longitude of a lat-lon box
        LONE = 180              ! INDEX 051   LON                 eastern longitude of a lat-lon box
        LLDO = 'n'
C-------
        RADR = 0                ! INDEX 061   n/a                 radius of all circles, kilometers
        DO N = 1, 100
          LATR(N) = 99.99       ! INDEX 061   LAT                 latitude  for a circle locus
          LONR(N) = 999.99      ! INDEX 061   LON                 longitude for a circle locus
        ENDDO
        NIR = 0
        IRBEG = 1
        IREND = IRBEG + 3
        LLRDO = 'n'
C-------
        DO N = 1, 100
          WMOLIST(N) = '     '  ! INDEX 071   RPID                list of stations (WMO numbers)
          WBBLIST(N) = '  '     ! INDEX 076   RPID                list of stations (WMO numbers)
        ENDDO
        NWMO = 0
        NWBB = 0
        IWBEG = 1
        IWEND = IWBEG + 9
        WMODO = 'n'
        WBBDO = 'n'
C-------
        IELEVL = -1000          ! INDEX 081   SELV                lowest  station elevation, meters
        IELEVH = 12000          ! INDEX 081   SELV                highest station elevation, meters
        IELEVDO = 'n'
C-------
C
        KELCEL = 'c'            ! INDEX 0

C-----7---------------------------------------------------------------72
C       READ INPUT ARGUMENTS                                    
C-----7---------------------------------------------------------------72
C
       NARG=IARGC()
       IF (NARG .LT. 3) THEN
          WRITE (*,*) 'Three input arguments required:'
          WRITE (*,*) 'Usage: bufrsurface.x input_file '//
     +                'output_file config_file'
          STOP 'Insufficient input given'
       ELSE
          CALL GETARG(1,argv)
            input_file=argv
          CALL GETARG(2,argv)
            output_file=argv
          CALL GETARG(3,argv)
            config_file=argv            
          WRITE (*,*) 'input file: ',input_file
          WRITE (*,*) 'output file: ',output_file
          WRITE (*,*) 'configuration file: ',config_file
       ENDIF
C
C-----7---------------------------------------------------------------72
C      OPEN AND READ CONFIGURATION FILE                                 
C-----7---------------------------------------------------------------72
C
        OPEN (ICUNIT, FILE=config_file)
        IFILTER = 'n'
C
        DO IC = 1, 100        ! BEGIN  CONFIGURATION FILE READS
C 
          IF (DODIAG.EQ.'y')  THEN
            WRITE (*,*)  'read line ',ic,' from the configuration file'
          ENDIF
C
          READ (ICUNIT,8020,IOSTAT=IOS)  CSTRING  ! READ NEXT ENTRY IN CONFIGURATION FILE
8020      FORMAT (A)
          IF (IOS.NE.0)  EXIT
          READ (CSTRING,'(I3)')  INDEX

          IF (INDEX.EQ.999)  THEN
            EXIT
          ENDIF
          IDX = INDEX / 10

c         IDX = 0: CHECK DIAGNOSTIC FLAG
          IF (IDX.EQ. 0)  THEN
C
C           FOR NCAR/CISL/DSS USAGE ONLY, FOR DIAGNOSTIC RUNS
C
            READ (CSTRING,'(4X,A1)')  DODIAG
            CYCLE
          ENDIF

C         IDX = 2: BUFR RECORD/REPORT TYPES
          IF (IDX.EQ. 2)  THEN
            IF (INDEX.EQ.21)  THEN
              READ (CSTRING,'(4X,10(A6,1X))')
     +        (RECGET(N),N=IRECBEG,IRECEND)
              IF (IRECEND.LT.20)  THEN
                IRECBEG = IRECBEG + 10
                IRECEND = IRECBEG + 9
                IRECDO = 'y'
                IFILTER = 'y'
              ENDIF
            ENDIF
            IF (INDEX.EQ.26)  THEN
              READ (CSTRING,'(4X,8(A8,1X))')
     +        (OBSGET(N),N=IOBSBEG,IOBSEND)
              IF (IOBSEND.LT.48)  THEN
                IOBSBEG = IOBSBEG + 8
                IOBSEND = IOBSBEG + 7
                IOBSDO = 'y'
                IFILTER = 'y'
              ELSE
                IOBSBEG = 49
                IOBSEND = 50
              ENDIF
            ENDIF
            CYCLE
          ENDIF

C         IDX = 4: PARAMETER MNEMONIVS SUBSETTING
          IF (IDX.EQ. 4)  THEN
            IF (INDEX.EQ.41.AND.CSTRING(5:5).EQ.'n')  THEN
              DEFAULT = 'n'
C
C             MUST RESET THE HEADER STRINGS
C
        DUMPHED(1)(001:028) = ' REC      OBS       REPORT T'
        DUMPHED(1)(029:068) = 'IME   STATION   LATI-   LONGI-   ELE-   '
        DUMPHED(1)(069:103) = '                                   '
        DUMPHED(1)(104:143) = '                                        '
        DUMPHED(1)(144:183) = '                                        '
        DUMPHED(1)(184:200) = '                 '
C
        DUMPHED(2)(001:028) = ' TYPE     TYPE      YYYYMMDD'
        DUMPHED(2)(029:068) = 'HHMM  BBSSS     TUDE     TUDE   VATION  '
        DUMPHED(2)(069:103) = '                                   '
        DUMPHED(2)(104:143) = '                                        '
        DUMPHED(2)(144:183) = '                                        '
        DUMPHED(2)(184:200) = '                 '
C
              IHDEND =  67
              CYCLE
            ENDIF
            IF (INDEX.EQ.41.AND.CSTRING(5:5).EQ.'y')  THEN
              CYCLE
            ENDIF
            NPT = 0
            DO LK = 5, NEMSCAN       ! LOOK (SCAN) FOR MNEMONICS 
              IF (CSTRING(LK:LK).NE.' ')  THEN
                NPT = NPT + 1           ! ANOTHER CHARACTER IN THIS MNEMONIC
                IF (NPT.EQ.1)  NML = NML + 1    ! ANOTHER MNEMONIC FOR OUR LIST
                IF (NML.GT.NEMLIM)  THEN
                  CYCLE
                ENDIF
                IF (NPT.LE.8)  THEN     ! ADD THIS CHARACTER TO THE NMLth MNEMONIC
                  NEMLIST(NML)(NPT:NPT) = CSTRING(LK:LK)
                ENDIF
              ELSE
                IF (NPT.GT.0)  THEN
                  I = IHDEND
                  M = NML
                  N = NPT
C                 IF (N.LE.7)  THEN       ! PUT THIS MNEMONIC IN THE HEADER
                  IF (N.LE.9)  THEN       ! PUT THIS MNEMONIC IN THE HEADER
                    DUMPHED(2)(I+1:I+10)  = XN(1:10-N)//NEMLIST(M)(1:N)
                  ELSE
                    DUMPHED(2)(I+1:I+10)  =             NEMLIST(M)(1:N)
                  ENDIF
C                 IHDEND = IHDEND + 10
                  IHDEND = IHDEND + 12
                  NPT = 0
                ENDIF
              ENDIF
            ENDDO
C
C           IDEALLY WE SHOULD VERIFY THE USER'S MNEMONICS WITH THE
C             NCEP BUFR TABLES
C
            IF (NML.GT.0)  THEN
              NMDO = 'y'
              IFILTER = 'y'
            ENDIF
            CYCLE
          ENDIF
          
C         IDX = 5: LATITUDE/LONGITUDE BOUNDARIES
          IF (IDX.EQ. 5)  THEN
            IF (CSTRING(05:10).EQ.'      '.OR.
     +          CSTRING(11:16).EQ.'      '.OR.
     +          CSTRING(17:22).EQ.'      '.OR.
     +          CSTRING(23:28).EQ.'      ')  THEN
              CYCLE
            ENDIF
            READ (CSTRING,'(4X,4I6)',IOSTAT=IOS)  LATS, LATN, LONW, LONE
            IF (IOS.NE.0)  CYCLE
            IF (LATS.NE.-90.OR.LATN.NE.90.
     +       OR.LONW.NE.-180.OR.LONE.NE.180)  THEN
              LLDO = 'y'
              IFILTER = 'y'
            ENDIF
C
C           WHEN THE "WINDOW" INCLUDES THE D.L., WE NEED TO DEAL WITH THE
C             DATE LINE, WHERE, SCANNING EASTWARD, THE SIGN OF THE LONGITUDE
C             FLIPS FROM POSITIVE TO NEGATIVE, I.E. FROM 179.9 TO -179.9
C           FIRST WE NEED TO RECOGNIZE THAT WE ARE DEALING WITH SUCH A FLIP,
C             OR WRAP-AROUND, WHICH OCCURS WHEN LONE < LONW
C
            IF (LONW.LT.LONE)  THEN
              LLWRAP = 'n'
            ELSE
              LLWRAP = 'y'
            ENDIF
            CYCLE
          ENDIF
          
C         IDX = 6: SELECT REPORTS WITHIN A CIRCLE RADIUS OF A POINT LOCATION
          IF (IDX.EQ. 6)  THEN
            IF (INDEX.EQ.61.AND.CSTRING(1:4).NE.'   ')  THEN
              READ (CSTRING,'(4X,I3)',IOSTAT=IOS)  RADR
              IF (IOS.NE.0)  CYCLE
              IF (RADR.LT.5.OR.RADR.GT.999)  THEN     ! TOO SMALL OR TOO BIG
                RADR = 0
                CYCLE
              ENDIF
            ENDIF
            IF (INDEX.EQ.62.AND.RADR.NE.0)  THEN
              DO  K = 5, 53, 16
                IF (CSTRING(K  :K+ 7).EQ.'        '.OR.
     +              CSTRING(K+8:K+15).EQ.'        ')  THEN
                  CYCLE
                ENDIF
                READ (CSTRING(K:K+15),'(2F8.2)',IOSTAT=IOS)
     +            XLATR, XLONR
                IF (IOS.NE.0)  CYCLE
                IF (XLATR.GE. -90.AND.XLATR.LE. 90.AND.
     +              XLONR.GE.-180.AND.XLONR.LE.180)  THEN
                  NIR = NIR + 1
                  LATR(NIR) = XLATR
                  LONR(NIR) = XLONR
                  LLRDO = 'y'
                  IFILTER = 'y'
                ENDIF
              ENDDO
              CYCLE
            ENDIF
          ENDIF
          
C         IDX = 7: WMO STATION NUMBER OR WMO BLOCK ID
          IF (IDX.EQ. 7)  THEN
            IF (INDEX.EQ.71)  THEN
              READ (CSTRING,'(4X,10(A5,1X))')
     +          (WMOLIST(N),N=IWBEG,IWEND)
              IF (IWEND.LE.90)  THEN
                IWBEG = IWBEG + 10
                IWEND = IWBEG + 9
                WMODO = 'y'
                IFILTER = 'y'
              ENDIF
              CYCLE
            ENDIF
            IF (INDEX.EQ.76)  THEN
              READ (CSTRING,'(4X,10(A2,1X))')
     +          (WBBLIST(N),N=IWBEG,IWEND)
              IF (IWEND.LE.90)  THEN
                IWBEG = IWBEG + 10
                IWEND = IWBEG + 9
                WBBDO = 'y'
                IFILTER = 'y'
              ENDIF
              CYCLE
            ENDIF
          ENDIF

C         IDX = 8: OBSERVATION PLATFORM LEVEL (STATION ELEVATION OR
C             PRESSURE LEVEL OF A SOUNDING)
          IF (IDX.EQ. 8)  THEN
            IF (INDEX.EQ.81)  THEN
              IF (CSTRING(05:10).NE.'      '.AND.
     +          CSTRING(11:16).NE.'      ')  THEN
                READ (CSTRING,'(4X,2I6)',IOSTAT=IOS)  IELEVL, IELEVH
                IF (IOS.NE.0)  CYCLE
                IF (IELEVL.GE.-1000.AND.IELEVH.LE.12000)  THEN
                  IELEVDO = 'y'
                  IFILTER = 'y'
                ENDIF
              ENDIF
            ENDIF
            IF (INDEX.EQ.86)  THEN
              IF (CSTRING(05:10).NE.'      '.AND.
     +            CSTRING(11:16).NE.'      ')  THEN
                READ (CSTRING,'(4X,2I6)',IOSTAT=IOS)  PLEVL, PLEVH
                IF (IOS.NE.0)  CYCLE
                IF (PLEVL.LE.1100.AND.PLEVH.GT.0.AND.
     +              PLEVH.LE.PLEVL)  THEN
                  PLEVDO = 'y'
                  IFILTER = 'y'
                ENDIF
              ENDIF
            ENDIF
            CYCLE
          ENDIF
          
C         IDX = 9: TEMPERATURE UNIT CONVERSION (C/K)
          IF (IDX.EQ. 9)  THEN
            IF (INDEX.EQ.91)  THEN    ! TEMPERATURES IN KELVIN OR CELSIUS
              READ (CSTRING,'(4X,A1)',IOSTAT=IOS)  KELCEL
              IF (IOS.NE.0)  CYCLE
              IF (KELCEL.NE.'c'.AND.KELCEL.NE.'k')  THEN
                KELCEL = 'c'
              ENDIF
            ENDIF
            CYCLE
          ENDIF
C
C         IF (IDX.EQ.10)  THEN
C           CYCLE
C         ENDIF
C
        ENDDO                   ! END OF CONFIGURATION FILE READS
C
        CLOSE (ICUNIT )

C-----7---------------------------------------------------------------72
C
        IF (IRECDO.EQ.'y')  THEN
          DO N = 1, 20
            IF (RECGET(N).EQ.'      ')  THEN
              NREC = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NREC = 0
        ENDIF
        IF (IOBSDO.EQ.'y')  THEN
          DO N = 1, 50
            IF (OBSGET(N).EQ.'        ')  THEN
              NOBS = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NOBS = 0
        ENDIF
        IF (WMODO.EQ.'y')  THEN
          DO N = 1, 100
            IF (WMOLIST(N).EQ.'     ')  THEN
              NWMO = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NWMO = 0
        ENDIF
        IF (WBBDO.EQ.'y')  THEN
          DO N = 1, 100
            IF (WBBLIST(N).EQ.'  ')  THEN
              NWBB = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NWBB = 0
        ENDIF
C

C Removing this if-block 30 Sep 2014 (T. Cram), since we want to retain
c more than just SYNOP reports from individual stations.

c        IF (WMODO.EQ.'y'.OR.WBBDO.EQ.'y')  THEN
c          IF (IOBSDO.EQ.'n')  THEN
c            IOBSDO = 'y'
c            NOBS = 1
c            OBSGET(1) = 'SYNOP   '
C
C
C           DATA WRITTEN TO OBSGET(2) AND OBSGET(3) IS JUST
C             FOR THE CONFIGURATION PRINTOUT, AND NOTHING ELSE.
C             I.E., WITH NOBS SET TO 1, FILTERING FOR WMO NUMBERS
C             WORKS ONLY FOR SYNOP OBS TYPE
C
c            OBSGET(2) = '  forced'
c            IF (WMODO.EQ.'y')  OBSGET(3) = 'by WMODO'
c            IF (WBBDO.EQ.'y')  OBSGET(3) = 'by WBBDO'
c            OBSGET(4) = '        '
c          ENDIF
c        ENDIF
C
        WRITE (*,*)
        WRITE (*,*)  'CONFIGURATION FILE HAS BEEN ACCEPTED:'
C
C-----7---------------------------------------------------------------72
C       PRINT CONFIGURATION SETTINGS TO STDOUT
C-----7---------------------------------------------------------------72
C
        WRITE (*,9070)
     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
ctac     +      DATEDO, IBEGDAT, IENDDAT,
     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
     +      LLDO, LATS, LATN, LONW, LONE,
     +      LLRDO, RADR,
     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
     +      IELEVDO, IELEVL, IELEVH,
     +
     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
C
C-----7---------------------------------------------------------------72
C        OPEN DIAGNOSTIC OUTPUT FILE, IF NEEDED
C-----7---------------------------------------------------------------72
        IF (DODIAG.EQ.'y'.OR.DODIAG.EQ.'x')  THEN
          OPEN (IDUNIT, FILE=diag_file)    ! IN THE bufrobs DIRECTORY
          WRITE (IDUNIT,9064) diag_file
9064      FORMAT (/,1X,'DIAGNOSTIC FILE ',A32,' OPENED')
C
          WRITE (IDUNIT,9065)  CONFILE
9065      FORMAT (/,1X,'CONFIGURATION FILE ',A32,' OPENED',
     +            /,3X,'CHOICES FOLLOW')
          WRITE (*,*)
          WRITE (*,*)  'DIAGNOSTIC FILE HAS BEEN OPENED'
        ENDIF
C
C-----7---------------------------------------------------------------72
C        PRINT CONFIGURATION SETTINGS TO DIAGNOSTIC OUTPUT FILE
C-----7---------------------------------------------------------------72
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,9070)
     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
ctac     +      DATEDO, IBEGDAT, IENDDAT,
     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
     +      LLDO, LATS, LATN, LONW, LONE,
     +      LLRDO, RADR,
     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
     +      IELEVDO, IELEVL, IELEVH,
     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
9070      FORMAT (/,
     +      ' -------------------------------------------------------',/
     +      '    FILTER   USE  CRITERIA',/
     +      ' RECORD TYPE  ',A1,'   RECGET  (1- 4 OF',
     +            I3,')', 4(2X,A6),/
     +      '    OBS TYPE  ',A1,'   OBSGET  (1- 4 OF',
     +            I3,')',4(2X,A8),/
ctac     +      '   DATE/TIME  ',A1,'   IBEGDAT ',I10,'  IENDDAT ',I10,/
     +      '   MNEMONICS  ',A1,'   NEMLIST (1-20 OF',
     +            I3,')',10(2X,A8),/35X,10(2X,A8),/
     +      '   LAT-LON    ',A1,'   LATS',I4,'  LATN',I4,
     +                          '  LONW',I5,'  LONE',I5,/
     +      '   CIRCLES    ',A1,'   RADR ',I6,/
     +      '                  LATR, LONR (1- 3 OF',
     +                          I3,') ',3(1X,F6.2,', ',F7.2,1X),/
     +      '  ELEVATION   ',A1,'   IELEVL ',I5,'  IELEVH ',I5,/
     +
     +      'WMO STATIONS  ',A1,'   WMOLIST (1-20 OF',I3,') ',
     +                          10(A5,1x),/36X,10(A5,1x),/
     +      'WMO BLOCKS    ',A1,'   WBBLIST (1-10 OF',I3,') ',
     +                          10(A2,1x)/,
     +      ' -------------------------------------------------------')
        ENDIF

C-----7---------------------------------------------------------------72
C       OPEN BUFR INPUT FILE
C-----7---------------------------------------------------------------72
        INFILE = input_file

        OPEN (IIUNIT, FILE=INFILE, FORM='UNFORMATTED' )
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,9090) INFILE
9090      FORMAT (/,1X,'BUFR DATA INPUT FILE ',A128,' OPENED')
        ENDIF
        WRITE (*,*)
        WRITE (*,*)  'BUFR INPUT FILE OPENED: ',INFILE
C
C       ASSOCIATE THE TABLES FILE WITH THE MESSAGES FILE, AND IDENTIFY
C       THE LATTER TO THE BUFRLIB SOFTWARE.
C
        CALL OPENBF  ( IIUNIT, 'IN', 11 )
C           
C-----7---------------------------------------------------------------72
C       OPEN OUTPUT FILE
C-----7---------------------------------------------------------------72
C
        OPEN(IPUNIT,FILE=output_file,STATUS='UNKNOWN',FORM='FORMATTED')
        IF (DODIAG.EQ.'y') THEN
          WRITE (IDUNIT,9100) output_file
9100      FORMAT (/,1X,'BUFR OUTPUT FILE OPENED ',A128)
        ENDIF
        WRITE (*,*)
        WRITE (*,*)  'BUFR OUTPUT FILE OPENED: ',output_file
C
C       SPECIFY THAT WE WOULD LIKE ROUTINE READNS TO RETURN RECDATE VALUES WITH
C         10 DIGITS (I.E. YYYYMMDDHH ), WHICH IS THE MAXIMUM BECAUSE MINUTES ARE
C         NOT AVAILABLE.
C
        CALL DATELEN (10)
        LN = 0          ! NEVER REDEFINED, UNLESS IT'S THROUGH THE EQIVALENCE
C
C ##### INITIALIZE BUFFER RECORD AND BUFR REPORT COUNTERS
C
        RECORDS = 0     ! TOTAL BUFR RECORDS FOUND (READNS CALLS)
        RECSREJ = 0     ! TOTAL BUFR RECORDS REJECTED
C
        RECREPS = 0     ! TOTAL BUFR REPORTS IN CURRENT RECORD
        REPORTS = 0     ! TOTAL BUFR REPORTS OPENED
        REPSACC = 0     ! TOTAL BUFR REPORTS ACCEPTED
        REPSREJ = 0     ! TOTAL BUFR REPORTS REJECTED
C
C-----7---------------------------------------------------------------72
C     LOOP TO READ BUFR RECORDS FROM THE DATA FILE
C-----7---------------------------------------------------------------72
C
        DORECS: DO                      ! LOOP TO READ BUFR RECORDS (MESSAGES)
C
C         READ THE NEXT BUFR MESSAGE ("RECORD") FROM THE FILE

          CALL READNS(IIUNIT,CSUBSET,RECDATE,ISTATUS)
          IF  (ISTATUS.NE. 0 )  THEN    ! END OF DATA FILE
            CALL CLOSBF (IIUNIT)
            CLOSE (IIUNIT)
            EXIT DORECS         ! GO PRINT STATISTICS FOR THIS FILE'S PROCESSING
          ENDIF
C
          RECORDS = RECORDS + 1

          MODREC = MOD(RECORDS,I500)
          IF (DODIAG.EQ.'y')  THEN
            IF (RECORDS.LE.50.OR.MODREC.EQ.1)  THEN
              WRITE (IDUNIT,9120)  RECORDS, RECDATE, CSUBSET
9120          FORMAT (/,1X,111('#'),
     +          /,1X,'BUFR RECORD ',I8,' WITH RECDATE ',I10,
     +          ' OPENED,  CSUBSET ',A8)
            ENDIF
          ENDIF
C
C         THE RETURNED RECDATE (DAY/TIME) AND MAYBE CSUBSET APPLY TO
C           ALL BUFR REPORTS IN THE RECORD
C
C         GET (DECODE) A VERBOSE RECORD TYPE AND REPORT TYPE
C           FROM CSUBSET, THEN CHECK WHETHER WE WANT THIS
C           PARTICULAR RECORD TYPE'S DATA
C
          CALL GETRO (CSUBSET,RECTYPE,OBSTYPE)
C
          IF (IRECDO.EQ.'y')  THEN
            CALL CKREC (RECORDS,RECTYPE,RECGET,NREC,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (RECORD TYPE)
          ENDIF
C
C ---------
C
C         CHECK WHETHER WE WANT THIS PARTICULAR OBSERVATION TYPE'S
C           DATA.  NOTE: ALTHOUGH UFBINT BREAKS OUT THE REPORTS IN
C           THE INNER LOOP (NAMED DOREPS), WE ALREADY OBTAINED THE
C           REPORT TYPE (FROM CSUBSET) WHICH APPLIES TO ALL REPORTS
C           IN THIS RECORD. - AND CAN NOT GET IT WITH UFBINT. SO WE
C           CAN FILTER HERE.
C
          IF (IOBSDO.EQ.'y')  THEN
            CALL CKOBS (RECORDS,OBSTYPE,OBSGET,NOBS,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (OBSERVATION TYPE)
          ENDIF
C
C ---------
C
          IRNO = 0               ! MAKE AN INDEX FOR CERTAIN ARRAYS AND
C                                !   ACTIONS IN THE DOREPS LOOP WHICH
C                                !   FOLLOWS
          IF (RECTYPE.EQ.'ADPSFC')  IRNO = 1
          IF (RECTYPE.EQ.'SFCSHP')  IRNO = 2
C
          IF (DODIAG.EQ.'y')  THEN
            WRITE (*,*)  RECORDS
            WRITE (IDUNIT,9977)  RECORDS
9977        FORMAT (9X,'RECORD ',I9,', START MAIN UFBINT LOOP')
          ENDIF
C
C-----7---------------------------------------------------------------72
C     LOOP TO READ BUFR REPORTS FROM THE RECORD, AND DO THE FILTERING
C-----7---------------------------------------------------------------72
C
          DOREPS: DO                            ! LOOP TO READ BUFR REPORTS (SUB-MESSAGES)
C
C           AT THIS POINT, WE HAVE A BUFR MESSAGE ("RECORD") OPEN WITHIN THE
C             INTERNAL ARRAYS OF BUFRLIB.  NEXT GO THROUGH ITS CONTENTS, WHICH
C             WILL INCLUDE ONE OR MORE SUB-MESSAGES ("REPORTS")
C
C           SUBROUTINE UFBINT EXTRACTS THE DESIRED PARAMETERS FROM A REPORT, AS
C             REQUESTED BY THE MNEMONICS GIVEN IN THE QIDENT, QBPARM AND
C             NEMLIST STRINGS.  THE "PATTERN" OF MNEMONICS MUST RESPECT SOME
C             GROUPING OR "REPLICATION" RULES.  THESE RULES GET COMPLICATED
C             BEYOND THE BASICS, SO THE OPTIONAL EXTRA (NEMLIST) PARAMETERS
C             ARE EXTRACTED ONE AT A TIME.  THAT GETS SLOW.
C
C               CALL UFBINT (IIUNIT,R8IDENT,MXMN,MXREPL,NREPL,QIDENT)
C               CALL UFBINT (IIUNIT,R8BPARM,MXMN,MXREPL,NREPL,QBPARM)
C               CALL UFBINT (IIUNIT,R8XPARM,MXMN,MXREPL,NREPL,QXPARM)
C
C           THE DESIRED PARAMETERS ARE RETURNED IN THE R8IDENT, R8BPARM AND
C             R8XPARM ARRAYS.  THESE WILL BE MOVED TO VARIABLES WHOSE NAMES HAVE
C             THE FORM X8NEMO, WHERE X CAN BE I (INTEGER), R (REAL), OR A
C             (CHARACTER).  THE 'NEMO' IS THE MNEMONIC (TRUNCATED TO FOUR
C             CHARACTERS WHEN NECESSARY) THAT WAS USED IN THE QIDENT, QBPARM,
C             OR NEMLIST STRINGS TO TELL UFBINT WHAT VARIABLES TO GET.
C
C           UFBINT RETURNS NREPL REPLICATIONS (PERHAPS LEVELS) OF A MAXIMUM OF MXREPL
C           UFBINT RETURNS UP TO MXMN PARAMETERS (AT EACH NREPL), CORRESPONDING TO
C             THE MAXIMUM NUMBER OF MNEMONICS IN QIDENT
C
C           THE MAXIMUM NUMBER OF PARAMETERS RETURNED BY UFBINT FOR A MNEMONIC
C             REQUEST STRING OF UP TO 80 CHARACTERS IS  80 / 5 = 16 (MXMN)
C             THERE CAN BE UP TO MXREPL REPLICATIONS
C
C ----------
C           (RE)INITIALIZE PARAMETER ARRAYS FOR THIS NEXT REPORT
C
            DO  MZ = 1, MXMN
                R8IDENT(MZ,1)  = XMSG
              DO  LZ = 1, MXREPL
                R8BPARM(MZ,LZ) = XMSG
                R8XPARM(MZ,LZ) = XMSG
              ENDDO
            ENDDO
C
C ++++++++++
C           THE FOLLOWING UFBINT CALL GETS A REPORT'S IDENTIFICATION.  THIS
C             INCLUDES STATION NUMBER OR CALL SIGN, LOCATION, ELEVATION, DATE
C             AND TIME.
C
C           IN THE BUR TABLES, THE UNITS ARE SOMETIMES GIVEN AS "CCITT IA5 (T, G)"
C             THIS IS "A character coding standard (ComitE Consultatif International
C             Telegraphique et Telephonique, International Alphabet No. 5), functionally
C             equivalent to ASCII. All character data within BUFR and CREX messages
C             are coded according to this standard."  WHEN THIS IS USED, EFFECTED
C             REAL*8 VARIABLES RETURNED FROM A UFBINT CALL SHOULD BE WRITTEN TO A
C             CHARACTER VARIABLE WITH AN 'A' FORMAT DESCRIPTOR.  EXAMPLES ARE THE
C             REPORT ID AND AIRCRAFT ID.
C
C           MXREPL HAS BEEN SET TO ONE BECAUSE THERE SHOULD NOT BE ANY
C             REPLICATION OF ID PARAMETERS.
C ++++++++++
C
            CALL UFBINT (IIUNIT,R8IDENT,MXMN,MXREPL,NREPL,QIDENT)
C
            REPORTS = REPORTS + 1
            RECREPS = RECREPS + 1
            IDUMP = MOD(REPORTS,I500)
C
            Z = 1
C
C           TOSS, AS SOON AS POSSIBLE, RECORDS WHICH ARE MISSING ABSOLUTELY
C             ESSENTIAL INFORMATION.  THIS AVOIDS UNNECESSARY PROCESSING
C             OF SUBSEQUENT INFORMATION IN A REPORT
C
            DO  ITOSS = 4, 10
              IF (ITOSS.EQ.6)  CYCLE
C
C             BAD LATITUDE, LONGITUDE, YEAR, MONTH, DAY, HOUR
C                 4         5          7     8      9    10
C
              IF (R8IDENT(ITOSS,Z).GT.R8BIG)  GO TO 290 ! MISSING VALUE
            ENDDO
C
            R8CLAT = R8IDENT(4,Z)
            R8CLON = R8IDENT(5,Z)
C
            IF (LLDO.EQ.'y')  THEN
C              WRITE (IDUNIT,9987)  R8CLAT,R8CLON,LATS,LATN,LONW,LONE
9987          FORMAT (1X,'CALLING R8CLAT,R8CLON,LATS,LATN,LONW,LONE ',/
     +                1X,F7.2,F7.2,4I5)
              CALL CKLL (RECORDS,RECREPS,R8CLAT,R8CLON,LATS,LATN,
     +                   LONW,LONE,LLWRAP,IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
            IF (LLRDO.EQ.'y')  THEN
              CALL CKRAD (RECORDS,RECREPS,R8CLAT,R8CLON,RADR,LATR,LONR,
     +                   NIR,IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
C           IF (R8IDENT(1,Z).GT.R8BIG)  R8IDENT(1,Z) = 99.
C           I8WMOB = R8IDENT(1,Z)               ! DO NOT NEED THIS
C
C           IF (R8IDENT(2).GT.R8BIG)  R8IDENT(2,Z) = 999.
C           I8WMOS = R8IDENT(2,Z)               ! DO NOT NEED THIS
C
            WRITE (A8RPID,9125)  R8IDENT(3,Z)   ! R8IDENT(3,Z) IS CHARACTER
9125        FORMAT (A8)                         ! AND THIS ACTION YIELDS A
C                                               ! LEFT-JUSTIFIED NUMBER OR STRING
C
            IF (WMODO.EQ.'y')  THEN
              CALL CKWMO (RECORDS,RECREPS,A8RPID,WMOLIST,NWMO,
     +                  IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  THEN
                GO TO 290                       ! REJECT UNINTERESTING REPORT
              ENDIF
            ENDIF
            IF (WBBDO.EQ.'y')  THEN
              CALL CKWBB (RECORDS,RECREPS,A8RPID,WBBLIST,NWBB,
     +                  IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
            IF (R8IDENT(6,Z).GT.R8BIG)  R8IDENT(6,Z)   = 99999.
            I8SELV = R8IDENT(6,Z)
C
            IF (IELEVDO.EQ.'y')  THEN
              CALL CKELEV (RECORDS,RECREPS,I8SELV,IELEVL,IELEVH,
     +          IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
            I8YEAR = R8IDENT(7,Z)
            I8MNTH = R8IDENT(8,Z)
            I8DAYS = R8IDENT(9,Z)
            I8HOUR = R8IDENT(10,Z)
            I8DATE = I8YEAR*1000000 + I8MNTH*10000 + I8DAYS*100 + I8HOUR
C
C-----7---------------------------------------------------------------72
C      DONE FILTERING
C-----7---------------------------------------------------------------72
C
            IF (R8IDENT(11,Z).GT.R8BIG)  R8IDENT(11,Z)   = 99.
            I8MINU = R8IDENT(11,Z)
C
C       2010.07.01 - LET'S NOT RETRIEVE OR SHOW THE CORRECTION FLAG
C
C           IF (R8IDENT(12,Z).GT.R8BIG)  R8IDENT(12,Z)   = 99.
C           I8CORN = R8IDENT(12,Z)
C
C ++++++++++
C           THE FOLLOWING UFBINT CALL GETS A REPORT'S BASIC METEOROLOGICAL
C             PARAMETERS - WHEN A USER HAS NOT TURNED THIS DEFAULT OFF.
C
C           MXREPL IS THE MAXIMUM (ESTIMATED) NUMBER OF REPLICATIONS WHICH
C             MIGHT BE RETURNED, WHILE NREPL IS THE NUMBER RETURNED.
C ++++++++++
C
            IF (DEFAULT.EQ.'y')  THEN
C
              CALL UFBINT (IIUNIT,R8BPARM,MXMN,MXREPL,NREPL,QBPARM)
C
              IF (NREPL.GT.1)  STOP 99991
              IF (R8BPARM(1,NREPL).GT.R8BIG)   R8BPARM(1,NREPL) = XMSG
              R8PRES = R8BPARM(1,NREPL)
C
              IF (R8BPARM(2,NREPL).GT.R8BIG)   R8BPARM(2,NREPL) = XMSG
              R8PMSL = R8BPARM(2,NREPL)
C
              IF (R8BPARM(3,NREPL).GT.R8BIG)   R8BPARM(3,NREPL) = XMSG
              R8ALSE = R8BPARM(3,NREPL)
C
              IF (R8BPARM(4,NREPL).GT.R8BIG)   R8BPARM(4,NREPL)  = XMSG
              R8WDIR = R8BPARM(4,NREPL)
C
              IF (R8BPARM(5,NREPL).GT.R8BIG)   R8BPARM(5,NREPL)  = XMSG
              R8WSPD = R8BPARM(5,NREPL)
C
              IF (R8BPARM(6,NREPL).GT.R8BIG)   R8BPARM(6,NREPL)  = XMSG
              R8TMDB = R8BPARM(6,NREPL)
C
              IF (R8BPARM(7,NREPL).GT.R8BIG)   R8BPARM(7,NREPL)  = XMSG
              R8TMDP = R8BPARM(7,NREPL)
C
              IF (R8BPARM(8,NREPL).GT.R8BIG)   R8BPARM(8,NREPL)  = XMSG
              R8REHU = R8BPARM(8,NREPL)
C
              IF (R8BPARM(9,NREPL).GT.R8BIG)   R8BPARM(9,NREPL)  = XMSG
              R8HOVI = R8BPARM(9,NREPL)
C
              IF (R8BPARM(10,NREPL).GT.R8BIG)  R8BPARM(10,NREPL) = XMSG
              R8TP03 = R8BPARM(10,NREPL)
C
              IF (R8BPARM(11,NREPL).GT.R8BIG)  R8BPARM(11,NREPL) = XMSG
              R8TP24 = R8BPARM(11,NREPL)
            ENDIF
C
C ++++++++++
C           THE FOLLOWING LOOP ON UFBINT CALLS GETS A SELECTION OF EXTRA
C             METEOROLOGICAL PARAMETERS, WHEN SPECIFIED BY THE USER (WHEN
C             NML .NE. 0)
C           WHEN DEFAULT = 'n' THIS IS THE ONLY WAY TO GET ANY PARAMETERS.
C           MXREPL IS THE MAXIMUM (ESTIMATED) NUMBER OF REPLICATIONS WHICH
C             MIGHT BE RETURNED, WHILE NREPL IS THE NUMBER RETURNED.
C ++++++++++
C
C           NML = 3                                     ! 2011.07.28 14:48 TEST
            IF (NML.GT.0)  THEN
              DO NX = 1, NML
                CALL UFBINT (IIUNIT,R8XPARM,MXMN,MXREPL,NREPL,
     +            NEMLIST(NX))
C               IF (NREPL.GT.1)  STOP 99992       ! DEACTIVATED 2011.07.29_12:30
C
C               NEMLIST(NX) HAS THE MNEMONICS - NML OF THEM
C
C               NREPL > 1  WHEN MORE THAN ONE LEVEL - FOR UPPER AIR DATA
C               NREPL = 1  SHOULD BE ONLY ONE LEVEL FOR SURFACE DATA
C
C               IF (R8XPARM(1,NREPL).GT.R8BIG)  R8XPARM(1,NREPL)= XMSG
                IF (R8XPARM(1,NREPL).GT.R8BIG)  R8XPARM(1,NREPL)= XMSG
                EXTRA(NX) = R8XPARM(1,NREPL)
                IF (DODIAG.EQ.'y')  THEN
                  WRITE (IDUNIT,2222)  NX, NML,
     +              R8XPARM(1,NREPL), EXTRA(NX)
2222              FORMAT (1X,'NX, NML, R8XPARM(1,NREPL), EXTRA(NX)',2I4,
     +              2F15.6)
                ENDIF
              ENDDO
            ENDIF
C
            IRECDAT = RECDATE
            WRITE (MINUTE,FMT='(I2)') I8MINU
            DO K = 1, 2
              IF (MINUTE(K:K).EQ.' ') THEN
                MINUTE(K:K) = '0'
              ENDIF
            ENDDO
            IF (DEFAULT.EQ.'y')  THEN
              IF (R8PRES.NE.XMSG)  THEN
                R8PRES = R8PRES / 100.
              ENDIF
              IF (R8PMSL.NE.XMSG)  THEN
                R8PMSL = R8PMSL / 100.
              ENDIF
              IF (R8ALSE.NE.XMSG)  THEN
                R8ALSE = R8ALSE / 100.
              ENDIF
              IF (R8TMDB.NE.XMSG) THEN
                R8TMDB = R8TMDB - 273.16
              ENDIF
              IF (R8TMDP.NE.XMSG) THEN
                R8TMDP = R8TMDP - 273.16
              ENDIF
            ENDIF
C
C-----7---------------------------------------------------------------72
C      PRINT THIS REPORT, UNLESS THERE ARE NO MEASURED DATA VALUES
C-----7---------------------------------------------------------------72
C
C           2010.07.01 - LET'S NOT RETRIEVE OR SHOW THE RECORD DATE
C
            NOBASIC = NNBASIC                 ! FOR THE BASIC PARAMETER CHECK LOOP, INTIALIZE
            IF (DEFAULT.EQ.'n')  NOBASIC = 0  !   THE NUMBER OF EXPECTED BASIC PARAMETERS
C
            NOEXTRA = NML                     ! FOR THE EXTRA PARAMETER CHECK LOOP, INTIALIZE
            IF (NML.EQ.0)  NOEXTRA = 0        !   THE NUMBER OF EXPECTED EXTRA PARAMETERS
C
            IF (NOBASIC.GT.0)  THEN
              DO IEE = 1, NNBASIC
                IF (R8BPARM(IEE,1).EQ.XMSG)  THEN
                  NOBASIC = NOBASIC - 1       ! DECREMENT NUMBER OF BASIC FILLED FIELDS
                ENDIF
              ENDDO
            ENDIF
            IF (NOEXTRA.GT.0)  THEN
              DO IEE = 1, NML
                IF (EXTRA(IEE).LE.XMSG+.1)  THEN         
                  NOEXTRA = NOEXTRA - 1       ! DECREMENT NUMBER OF EXTRA FILLED FIELDS
                ENDIF
              ENDDO
            ENDIF
C           WRITE (6,6666)  DEFAULT, NNBASIC, NOBASIC, NML, NOEXTRA
6666        FORMAT (/1X,'DEFAULT, NNBASIC, NOBASIC, NML, NOEXTRA ',
     F        A,10I8) 
            IF (NOBASIC.LT.0)  NOBASIC = 0
            IF (NOEXTRA.LT.0)  NOEXTRA = 0
            NODATA = NOBASIC + NOEXTRA
            IF (NODATA.EQ.0)  GO TO 290       ! NO MEASURED VALUES (ALL REQUESTED FIELDS MISSING)
C
            REPSACC = REPSACC + 1
C
C           IF (MOD(REPSACC,30).EQ.1)  THEN
            IF (    REPSACC    .EQ.1)  THEN
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +           PRINT A HEADER EVERY 30 REPORTS -                                +
C +             2011.05.13 - WE'RE DOING JUST THE FIRST ONE, TO MAKE IT SIMPLER+
C +             TO LOAD THE FILE INTO A SPREADSHEET.                           + 
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
              WRITE (IPUNIT,9270)  DUMPHED(1)(001:IHDEND),
     +                             DUMPHED(2)(001:IHDEND)
C270          FORMAT ('# '/'# ',a/'# ',a)  # DROPPED THE # ON 211.05.13
9270          FORMAT ('  '/'  ',a/'  ',a)
            ENDIF
            IF (DEFAULT.EQ.'y')  THEN
              WRITE (IPUNIT,9280)
C    &        RECTYPE, IRECDAT, OBSTYPE,
     &        RECTYPE,          OBSTYPE,
     &        I8DATE, MINUTE, A8RPID, R8CLAT, R8CLON, I8SELV,
C    &        I8CORN, R8PRES, R8PMSL,
     &                R8PRES, R8PMSL, R8ALSE,
     &        R8TMDB, R8TMDP, R8REHU, R8WDIR, R8WSPD, R8HOVI,
     &        R8TP03, R8TP24,
     &        (EXTRA(NX),NX=1,NML)
9280          FORMAT (2x,
C    +        1X,A6,2X,I10,3X,A8,
     +        1X,A6,1X,    2X,A8,
     +        2X,I10,A2,2X,A6,2X,F7.2,2X,F7.2,2X,I5,
C    +        2X,I3,2X,F7.1,2X,F7.1,
     +              2X,F7.1,2X,F7.1,2X,F7.1,
C    +        2X,F5.1,2X,F5.1,2X,F5.1,2X,F6.1,2X,F6.1,2X,F7.1,
     +        2X,F7.1,2X,F7.1,2X,F7.1,2X,F7.1,2X,F7.1,2X,F7.1,
     +        2X,F7.1,2X,F7.1,4X,'|',
     +        100F12.2)
            ENDIF
C
            IF (DEFAULT.EQ.'n')  THEN
              WRITE (IPUNIT,9285)
     &        RECTYPE,          OBSTYPE,
     &        I8DATE, MINUTE, A8RPID, R8CLAT, R8CLON, I8SELV,
     &        (EXTRA(NX),NX=1,NML)
9285          FORMAT (2x,
     +        1X,A6,1X,    2X,A8,
     +        2X,I10,A2,2X,A6,2X,F7.2,2X,F7.2,2X,I5,
     +        100F12.2)
            ENDIF
C
            GO TO 300
 290        CONTINUE
C
C-----7---------------------------------------------------------------72
C         WE HAVE SKIPPED DOWN TO HERE TO REJECT A REPORT
C-----7---------------------------------------------------------------72
C
            REPSREJ = REPSREJ + 1
C
 300        CONTINUE
C
C           SEE IF THERE IS ANOTHER SUB-MESSAGE ("REPORT") IN THIS MESSAGE ("RECORD")
C             NOTE THAT IN THE BUFR LIBRARY COMMENTS, NCEP USES THE TERM "SUBSET"
C             INSTEAD OF SUB-MESSAGE - CLEAR AS MUD, EH?
C
            CALL READSB  ( IIUNIT, IERRSB )
C
            IF  (IERRSB.NE.0)  THEN
              EXIT DOREPS
            ENDIF
          ENDDO DOREPS          ! END OF READING A BUFR REPORT (SUB-MESSAGE)
          RECREPS = 0
C
        ENDDO DORECS            ! END OF READING A BUFR RECORD (MESSAGE)
C
        IF (DODIAG.EQ.'y')  THEN
          WRITE(IDUNIT,9800)
9800      FORMAT (//,' ************* INPUT FILE DONE ')
        ENDIF
        RECSACC = RECORDS - RECSREJ
C
C-----7---------------------------------------------------------------72
C     PRINT THE CONFIGURATION AT THE END OF THE OUTPUT FILE
C-----7---------------------------------------------------------------72
C
C       DEACTIVATED 2011.05.13 TO AVOID CONFUSING SPREADSHEET USAGE
C
C       WRITE (IPUNIT,9880) config_file
9880    FORMAT (/,1X,'CONFIGURATION FILE ',A128,
     +          /,3X,'CHOICES FOLLOW')
C       WRITE (IPUNIT,9070)
C    +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
C    +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
C    +      DATEDO, IBEGDAT, IENDDAT,
C    +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
C    +      LLDO, LATS, LATN, LONW, LONE,
C    +      LLRDO, RADR,
C    +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
C    +      IELEVDO, IELEVL, IELEVH,
C    +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
C    +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
C
        WRITE (IDUNIT,9890)  INFILE, RECORDS, RECSREJ, RECSACC,
     +     REPORTS, REPSACC, REPSREJ
9890    FORMAT (/,1X,'BUFR EXTRACTION STATISTICS',
     +            1X,'FOR FILE INFILE  ',A66,/,
     +            5X,'RECORDS    FOUND ',I12,/,
     +            5X,'RECORDS REJECTED ',I12,/,
     +            5X,'RECORDS ACCEPTED ',I12,//,
     +            5X,'REPORTS   OPENED ',I12,
     +                  ' (FROM ACCEPTED RECORDS)',/
     +           ,5X,'REPORTS ACCEPTED ',I12,/,
     +            5X,'REPORTS REJECTED ',I12)
C
C       WRITE (IPUNIT,9890)  INFILE, RECORDS, RECSREJ, RECSACC,
C    +     REPORTS, REPSACC, REPSREJ
        CLOSE   ( IPUNIT )
        IF (REPSACC.LE.0)  THEN      ! REMOVE AN EMPTY OUTPUT FILE, WHEN IT EXISTS
          CALL SYSTEM ("rm -f output_file")
        ENDIF
C
C-----7---------------------------------------------------------------72
C   Don't loop
C        ENDDO DOFILS            ! END OF READING A BUFR DATA FILE
C-----7---------------------------------------------------------------72

C        WRITE(IDUNIT,9900)  INK
9900    FORMAT (//,' **** ALL ',I5,' BUFR DATA FILES PROCESSED *****',
     +          //,' ************ DONE ************')
C        STOP 99999
        END
C
C       ########################################################################
C
        SUBROUTINE GETRO (CSUBSET,RECTYPE,OBSTYPE)
C
C       GET AND MAP RECORD TYPE AND OBSERVATION (REPORT) TYPE
C
C       CSUBSET--   see  http://www.emc.ncep.noaa.gov/mmb/data_processing/data_dumping.doc/table_1.htm
C
        CHARACTER*8  CSUBSET    ! HAS THE CODED RECORD TYPE AND OBSERVATION TYPE
        CHARACTER*6  RECTYPE    ! UNCODED RECORD TYPE
        CHARACTER*8  OBSTYPE    ! UNCODED OBSERVATION TYPE
C
        CHARACTER*6    ASURF(10)
        DATA ASURF(1)  /'ADPSFC'/    ! SURFACE LAND
        DATA ASURF(2)  /'SFCSHP'/    ! SURFACE SEA
C       DATA ASURF(3)  /'ADPUPA'/    ! VERTICAL SOUNDINGS (NOT SATELLITE)
C       DATA ASURF(4)  /'SATSDG'/    ! VERTICAL SOUNDINGS (SATELLITE) (DSS NAME NOT NCEP'S)
C       DATA ASURF(5)  /'AIRCFT'/    ! SINGLE LEVEL UPPER AIR (NOT SATELLITE)
C       DATA ASURF(6)  /'SATWND'/    ! SINGLE LEVEL UPPER AIR (SATELLITE)
        DATA ASURF(7)  /'      '/    !
        DATA ASURF(8)  /'      '/    !
        DATA ASURF(9)  /'      '/    !
        DATA ASURF(10) /'      '/    !
C
        CHARACTER*8 OBSTYP(10,200)
        DATA OBSTYP /2000*'        '/
C
        SAVE
C
C       OBSTYP(1,000) = 'SYNOPR  '  !  SYNOPTIC RESTRICTED (WMO40)
        OBSTYP(1,001) = 'SYNOP   '  !  FIXED MANUAL AND AUTOMATIC
        OBSTYP(1,002) = 'SYNOPM  '  !  MOBILE MANUAL AND AUTOMATIC
        OBSTYP(1,003) = '        '  !
        OBSTYP(1,004) = '        '  !
        OBSTYP(1,005) = '        '  !
        OBSTYP(1,006) = '        '  !
        OBSTYP(1,007) = 'METAR   '  !  AVIATION METAR
        OBSTYP(1,008) = 'PRFLRS  '  !  PROFILERS NOAA (NPN) AND MULTI-AGENCY (MAP)
        OBSTYP(1,009) = '        '  !
        OBSTYP(1,010) = 'SHEFCM  '  !  MISCELLANEOUS (ORIGINALLY IN SHEF) 
        OBSTYP(1,011) = 'SHEFF   '  !  AFOS - PRECIP (ORIGINALLY IN SHEF)
        OBSTYP(1,012) = 'SCD     '  !  AVIATION SUPPL AND CLIMAT (SCD)
C
        OBSTYP(2,001) = 'SHIPS   '  !  SHIP MANUAL AND AUTOMATIC, RESTRICTED (WMO40)
        OBSTYP(2,002) = 'DBUOY   '  !  MOORED OR DRIFTING BUOY, F-18
        OBSTYP(2,003) = 'MBUOY   '  !  MOORED BUOY. F-13
        OBSTYP(2,004) = 'LCMAN   '  !  CMAN STATION
        OBSTYP(2,005) = 'TIDEG   '  !  TIDE GAUGE REPORTS (ORIGINALLY IN CREX)
        OBSTYP(2,006) = 'SLPBG   '  !  SEA LEVEL PRESSURE BOGUS
        OBSTYP(2,007) = 'CSTGD   '  !  U.S. COAST GUARD REPORTS
        OBSTYP(2,008) = 'TIGDCM  '  !  TIDE GAUGE REPORTS (ORIGINALLY IN CMAN)
        OBSTYP(2,009) = 'RIVER   '  !  USGS RIVER / STREAM
        OBSTYP(2,010) = 'CMANSH  '  !  CMAN (ORIGINALLY IN SHEF)
        OBSTYP(2,011) = 'BUOYSH  '  !  MOORED OR DRIFTING BUOY (ORIGINALLY IN SHEF)
        OBSTYP(2,012) = 'TIDGSH  '  !  TIDE GAUGE REPORTS (ORIGINALLY IN SHEF)
        OBSTYP(2,013) = 'SHIPSU  '  !  SHIP MANUAL AND AUTOMATIC (UNRESTRICTED)
C
        READ (CSUBSET(3:5),'(I3)')  J
        J = J + 1                              ! BUFR RECORD TYPE POINTER
        RECTYPE = ASURF(J)
C
        READ (CSUBSET(8:8),'(I3)')  K
C
        IF (OBSTYP(J,K).NE.'        ')  THEN
          OBSTYPE = OBSTYP(J,K)                ! GRAB IT
        ELSE
          OBSTYPE(1:8) = CSUBSET(1:8)          ! DEFAULT IS THE CODE (WHEN UNRECOGNIZED)
        ENDIF
C
C       FOLLOWING ARE IN OTHER BUFR FILES:
C          CSUBSET(1:8).EQ.'NC255nnn')         ! MESONET MADIS RECORDS FOR VARIOUS (NNN) REGIONS
C                                              !   http://madis.noaa.gov/
C          CSUBSET(1:8).EQ.'NC006nnn')         ! NEXRAD LEVEL II RADAR DATA SEQUENCES AT HOURS NNN
C
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKREC (RECS,RECTYP,RECGET,NREC,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK RECORD TYPE, E.G. ADPSFC, SFCSHP ...
C
        INTEGER*4 RECS
        CHARACTER*6 RECTYP, RECGET(20)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO NR = 1, NREC
          IF (RECTYP.EQ.RECGET(NR))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'y')  THEN
            WRITE (IDUNIT,7706) RECS, RECTYP
7706        FORMAT (1X,'CKREC:  RECORD ',I9,', ACCEPTED, RECTYP ',
     +       A6)
          ELSE
            WRITE (IDUNIT,7705) RECS, RECTYP
7705        FORMAT (1X,'CKREC:  RECORD ',I9,', REJECTED, RECTYP ',
     +       A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKOBS (RECS,OBSTYP,OBSGET,NOBS,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK OBSERVATION (REPORT) TYPE, E.G. SYNOP, METAR ...
C
        INTEGER*4 RECS
        CHARACTER*8 OBSTYP, OBSGET(50)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO NO = 1, NOBS
          IF (OBSTYP.EQ.OBSGET(NO))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'y')  THEN
            WRITE (IDUNIT,7706) RECS, OBSTYP
7706        FORMAT (1X,'CKOBS:  RECORD ',I9,
     +       ', ACCEPTED, OBSTYP ',A6)
          ELSE
            WRITE (IDUNIT,7705) RECS, OBSTYP
7705        FORMAT (1X,'CKOBS:  RECORD ',I9,
     +       ', REJECTED, OBSTYP ',A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKDATE (RRLEV,RR,IDATE,IBEG,IEND,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK RECORD OR REPORT DATE
C
        CHARACTER*6 RRLEV       ! WILL CONTAIN EITHER 'RECORD' OR 'REPORT'
        INTEGER*4 RR, IDATE, IBEG, IEND
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (IDATE.LT.IBEG.OR.IDATE.GT.IEND)  ACK = 'n'
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7730)  RRLEV, RR, IDATE, IBEG, IEND
7730        FORMAT (/1X,'CKDATE: ',A6,1X,I9,', REJECTED, DATE ',I10,
     +       ' BEGIN ',I10,', END ',I10)
          ELSE
            WRITE (IDUNIT,7731)  RRLEV, RR, IDATE, IBEG, IEND
7731        FORMAT (/1X,'CKDATE: ',A6,1X,I9,', ACCEPTED, DATE ',I10,
     +       ' BEGIN ',I10,', END ',I10)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKLL(RECS,RR,LAT,LON,LATS,LATN,LONW,LONE,LLWRAP,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK GEOGRAPHIC WINDOW (LATITUDE - LONGITUDE)
C
        INTEGER*4 RECS, RR
        REAL*8 LAT, LON
        CHARACTER*1 LLWRAP
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (LAT.LT.LATS.OR.LAT.GT.LATN)  THEN
          IF (DODIAG.EQ.'y')  THEN
            WRITE (IDUNIT,7750)  RECS, RR, LAT, LON,
     &      LATS, LATN, LONW, LONE
7750        FORMAT ( 1X,'CKLL:   RECORD ',I9,', REPORT ',I9,
     +      ', REJECTED,  ',F7.1,F8.1,',  LATS',I6,' LATN',I6,
     +      ' LONW',I6,' LONE',I6)
          ENDIF
          GO TO 90   ! TO REJECT REPORT
        ENDIF
        IF (LLWRAP.EQ.'n')  THEN
          IF (LON.LT.LONW.OR.LON.GT.LONE)  THEN
            IF (DODIAG.EQ.'y')  THEN
              WRITE (IDUNIT,7750)  RECS, RR, LAT, LON,
     &        LATS, LATN, LONW, LONE
            ENDIF
            GO TO 90   ! TO REJECT REPORT
          ENDIF
        ENDIF
        IF (LLWRAP.EQ.'y')  THEN
          IF (LON.GE.0.0)  THEN
            IF (LON.LT.LONW)  THEN
              IF (DODIAG.EQ.'y')  THEN
                WRITE (IDUNIT,7750)  RECS, LAT, LON,
     &          LATS, LATN, LONW, LONE
              ENDIF
              GO TO 90   ! TO REJECT REPORT
            ENDIF
          ELSE
            IF (LON.GT.LONE)  THEN
              IF (DODIAG.EQ.'y')  THEN
                WRITE (IDUNIT,7750)  RECS, LAT, LON,
     &          LATS, LATN, LONW, LONE
              ENDIF
              GO TO 90   ! TO REJECT REPORT
            ENDIF
          ENDIF
        ENDIF
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,7751)  RECS, LAT, LON,
     &    LATS, LATN, LONW, LONE
7751      FORMAT ( 1X,'CKLL:   RECORD ',I9,', REPORT ',I9,
     +    ', ACCEPTED,  ',F7.1,F8.1,',  LATS',I6,' LATN',I6,
     +    ' LONW',I6,' LONE',I6)

        ENDIF
        RETURN
  90    CONTINUE
        ACK = 'n'
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKRAD (RECS,RR,XLAT,XLON,RADR,YLAT,YLON,NIR,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK GEOGRAPHIC CIRCLE (LATITUDE, LONGITUDE, RADIUS)
C
        INTEGER RECS, RR, RADR
        REAL*8 XLAT, XLON
        REAL YLAT(100), YLON(100)
        DATA R, DRAD / 6371.2277, 0.0174533/    ! EARTH'S RADIUS IN KM
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO  K = 1, NIR
C         DLON = ABS(XLON - YLON(K))  ! ORIGINAL (ON29), FOR 0W TO 359W
          XL = XLON + 180.0           ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          YL = YLON(K) + 180.0        ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          DLON = ABS(XL - YL)         ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          IF (DLON.GT.180.)  DLON = 360. - DLON
          DL = DLON * DRAD
C
          XL = (XLAT + 90.) * DRAD
          YL = (YLAT(K) + 90.) * DRAD
          DIST = R * ABS(ACOS(COS(XL)*COS(YL)
     +                 + SIN(XL)*SIN(YL)*COS(DL)))
          IF (DIST.LE.RADR)  ACK = 'y'
          IF (DODIAG.EQ.'y')  THEN
            IF (ACK.EQ.'n')  THEN
              WRITE (IDUNIT,7760) RECS, RR, XLAT, XLON,
     +          YLAT(K), YLON(K), DIST, RADR
7760          FORMAT (1X,'CKRAD:  RECORD ',I9,', REPORT ',I9,
     +          ', REJECTED, ',' XLAT ',F6.0,' XLON ',F6.0,' YLAT ',
     +          F8.2,' YLON ',F8.2,' DIST ',F6.0,' RADR ',I6)
            ELSE
              WRITE (IDUNIT,7761) RECS, RR, XLAT, XLON,
     +          YLAT(K), YLON(K), DIST, RADR
7761          FORMAT (1X,'CKRAD:  RECORD ',I9,', REPORT ',I9,
     +          ', ACCEPTED, ',' XLAT ',F6.0,' XLON ',F6.0,' YLAT ',
     +          F8.2,' YLON ',F8.2,' DIST ',F6.0,' RADR ',I6)
            ENDIF
          ENDIF
          EXIT
        ENDDO
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKWMO(RECS,RR,A8RPID,WMOLIST,NWMO,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK WMO STATION NUMBER (bbsss)
C
        INTEGER*4 RECS, RR
        CHARACTER*8 A8RPID              ! CHANGED FROM 6 TO 8 ON 2011.05.06
        CHARACTER*5 WMOLIST(100)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO N = 1, NWMO
          IF (A8RPID(1:5).EQ.WMOLIST(N)(1:5))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7770) RECS, RR, A8RPID
7770        FORMAT (1X,'CKWMO:  RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, STATION ',A6)
          ELSE
            WRITE (IDUNIT,7771) RECS, RR, A8RPID
7771        FORMAT (1X,'CKWMO:  RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, STATION ',A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKWBB (RECS,RR,A8RPID,WBBLIST,NWBB,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK WMO STATION BLOCK NUMBER (bb)
C
        INTEGER*4 RECS, RR
        CHARACTER*8 A8RPID              ! CHANGED FROM 5 TO 8 ON 2011.05.06
        CHARACTER*2 WBBLIST(100)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO N = 1, NWBB
          IF (A8RPID(1:2).EQ.WBBLIST(N)(1:2))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7775) RECS, RR, A8RPID
7775        FORMAT (1X,'CKWBB:  RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, STATION ',A6)
          ELSE
            WRITE (IDUNIT,7776) RECS, RR, A8RPID
7776        FORMAT (1X,'CKWBB:  RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, STATION ',A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKELEV (RECS,RR,IEL,IELO,IEHI,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK STATION ELEVATION
C
        INTEGER*4 RECS, RR
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (IEL.LT.IELO.OR.IEL.GT.IEHI)  ACK = 'n'
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7780) RECS, RR, IEL, IELO, IEHI
7780        FORMAT (1X,'CKELEV: RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, ELEV ',I6,' BOTTOM ',I6,', TOP ',I6)
          ELSE
            WRITE (IDUNIT,7781) RECS, RR, IEL, IELO, IEHI
7781        FORMAT (1X,'CKELEV: RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, ELEV ',I6,' BOTTOM ',I6,', TOP ',I6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE UP2LOW (WORD1,WORD2,NCH)
C
C       MAP UPPER CASE CHARACTERS TO LOWER CASE
C
        CHARACTER*100 WORD1, WORD2
        CHARACTER*1 CHU(26), CHL(26)
        DATA CHU /
     +  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     +  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'/
        DATA CHL /
     +  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     +  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'/
        SAVE
        DO  N = 1, NCH
          K = 13        ! (M)
          KSTEP = 7
          IDO = 0
          DO
            IDO = IDO + 1
            IF (IDO.GT.6)  EXIT
            IF (WORD1(N:N).EQ.CHU(K))  THEN
              WORD2(N:N) = CHL(K)
              GO TO 9
            ENDIF
            IF (WORD1(N:N).LT.CHU(K))  THEN
              K = K - KSTEP
            ELSE
              K = K + KSTEP
            ENDIF
            KSTEP = KSTEP * .5
            IF (KSTEP.LT.1)  KSTEP = 1
          ENDDO
   9      CONTINUE
        ENDDO
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE LOW2UP (WORD1,WORD2,NCH)
C
C       MAP LOWER CASE CHARACTERS TO UPPER CASE
C
        CHARACTER*100 WORD1, WORD2
        CHARACTER*1 CHU(26), CHL(26)
        DATA CHU /
     +  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     +  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'/
        DATA CHL /
     +  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     +  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'/
        SAVE
        DO  N = 1, NCH
          K = 13        ! (M)
          KSTEP = 7
          IDO = 0
          DO 
            IDO = IDO + 1
            IF (IDO.GT.6)  EXIT
            IF (WORD1(N:N).EQ.CHL(K))  THEN
              WORD2(N:N) = CHU(K)
              GO TO 9
            ENDIF
            IF (WORD1(N:N).LT.CHL(K))  THEN
              K = K - KSTEP
            ELSE
              K = K + KSTEP
            ENDIF
            KSTEP = KSTEP * .5
            IF (KSTEP.LT.1)  KSTEP = 1
          ENDDO
   9      CONTINUE
        ENDDO
        RETURN
        END
C ***
C ***   When modifying bufradpsfc.f, after editing, a reinstall must
C ***     be done by doing
C ***          cd .../bufrdecode/src
C ***          [edit] bufradpsfc.f
C ***          cd ../install
C ***          cat install.sh | sed s/CPLAT=linux/CPLAT=sun/ >! mk_exec.sh
C ***          chmod 700 mk_sun_exec.sh
C ***          mk_sun_exec.sh >>&! /dev/null
C ***
C ***     My bufr_bench script takes care of these procedures
C ***
C ***     Note that the install expects to be run in the .../install
C ***     directory, to find the source code in a file named
C ***     .../src/bufradpsfc.f , and a library of supporting code in
C ***     .../lib , and will write the executable in
C ***     .../exe/bufradpsfc.x .  It's default platform is "linux".  You
C ***     you may need to change that to "sun" or something.
C ***
C ***   The install creates a script named convert.csh in directory .../exe,
C ***     which runs every file found in directory .../bufrobs through
C ***     both surface data decoders - .../exe/bufradpsfc.x and
C ***     .../exe/bufrsfcship.x , leaving outputs in directory .../textobs.
C ***
C ***   Note that the published data files are in:
C ***     /datazone/dsszone/ds461.0/
C ***     A single typical ds461.0 tar file has a name like:
C ***     gdassfcobs.20100410.tar.gz
C ***
