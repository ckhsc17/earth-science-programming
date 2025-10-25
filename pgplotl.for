C*GRDATE -- get date and time as character string (Fortran90)
C+
      SUBROUTINE GRDATE(CDATE, LDATE)
      CHARACTER CDATE*(17)
      INTEGER   LDATE
C
C Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
C To receive the whole string, the CDATE should be declared
C CHARACTER*17.
C
C Arguments:
C  CDATE : receives date and time, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. This will always be 17, unless the length
C           of the string supplied is shorter.
C--
C 1989-Mar-17 - [AFT]
C 12/1993 C. T. Dum MS Power Station F32 Version
C 1996-Apr-16 - Fortran 90 version [P.A.Seeger]
C-----------------------------------------------------------------------
      CHARACTER CMON(12)*3
      INTEGER   II(8)
      DATA      CMON/'Jan','Feb','Mar','Apr','May','Jun',               &
     &               'Jul','Aug','Sep','Oct','Nov','Dec'/
C---
      CALL DATE_AND_TIME(VALUES=II)
      WRITE(CDATE,111) II(3),CMON(II(2)),II(1),II(5),II(6)
  111 FORMAT(I2,'-',A3,'-',I4,' ',I2,':',I2)
      LDATE = 17
      RETURN
      END

C*GRFLUN -- free a Fortran logical unit number (MS-DOS)
C+
      SUBROUTINE GRFLUN(LUN)
      INTEGER LUN
C
C Free a Fortran logical unit number allocated by GRGLUN. 
C
C Arguments:
C  LUN    : the logical unit number to free.
C--
C 22-Apr-1996 [PAS]
C-----------------------------------------------------------------------
      CLOSE (LUN)
      RETURN
      END

C*GRGCOM -- read with prompt from user's terminal (Fortran 90)
C+
      INTEGER FUNCTION GRGCOM(CREAD, CPROM, LREAD)
      CHARACTER CREAD*(*), CPROM*(*)
      INTEGER   LREAD
C
C Issue prompt and read a line from the user's terminal; in VMS,
C this is equivalent to LIB$GET_COMMAND.
C
C Arguments:
C  CREAD : (output) receives the string read from the terminal.
C  CPROM : (input) prompt string.
C  LREAD : (output) length of CREAD.
C
C Returns:
C  GRGCOM : 1 if successful, 0 if an error occurs (e.g., end of file).
C--
C 1989-Mar-29
ctd 3/95:len_trim (MS Fortran/Fortran 90)
C-----------------------------------------------------------------------
      INTEGER IER
C---
   11 FORMAT(A)
C---
      GRGCOM = 0
      LREAD = 0
      WRITE (*, 101, IOSTAT=IER) CPROM
  101 FORMAT(1X,A,/)
      IF (IER.EQ.0) READ (*, 11, IOSTAT=IER) CREAD
      IF (IER.EQ.0) GRGCOM = 1
      LREAD = LEN_TRIM(CREAD)
      RETURN
      END
 
C*GRGENV -- get value of PGPLOT environment parameter (Win95)
C+
      SUBROUTINE GRGENV(CNAME, CVALUE, LVALUE)
      !USE       MSFLIB
      CHARACTER CNAME*(*), CVALUE*(*)
      INTEGER   LVALUE
C
C Return the value of a PGPLOT environment parameter.
C
C Arguments:
C CNAME   : (input) the name of the parameter to evaluate.
C CVALUE  : receives the value of the parameter, truncated or extended
C           with blanks as necessary. If the parameter is undefined,
C           a blank string is returned.
C LVALUE  : receives the number of characters in CVALUE, excluding
C           trailing blanks. If the parameter is undefined, zero is
C           returned.
C--
C 1990-Mar-19 - [AFT]
C 12/93;3/95 CTD F32
C 16-Apr-1996 - Win95, F90 (MSFLIB, LEN_TRIM) [P.A.Seeger]
C-----------------------------------------------------------------------
C
      CHARACTER*80 CTMP,CTEMP
      CTMP   = 'PGPLOT_'//CNAME
      !LVALUE = GETENVQQ(CTMP(:LEN_TRIM(CTMP)),CTEMP)
      LVALUE = 0
	!print*,' Lvalue ', LVALUE
      IF(LVALUE .NE. 0) THEN
         CVALUE = CTEMP(:LVALUE)
      ELSE
         CVALUE = ' '
      END IF
      RETURN
      END

C*GRGLUN -- get a Fortran logical unit number (MS-DOS)
C+
      SUBROUTINE GRGLUN(LUN)
      INTEGER LUN
C
C Get an unused Fortran logical unit number.
C Returns a Logical Unit Number that is not currently opened.
C After GRGLUN is called, the unit should be opened to reserve
C the unit number for future calls.  Once a unit is closed, it
C becomes free and another call to GRGLUN could return the same
C number.  Also, GRGLUN will not return a number in the range 1-9
C as older software will often use these units without warning.
C
C Arguments:
C  LUN    : receives the logical unit number
C--
C 12-Feb-1989 [AFT/TJP].
C 22-Apr-1996: count upward from 11 [PAS]
C-----------------------------------------------------------------------
      INTEGER I
      LOGICAL QOPEN
C---
      I = 10
      QOPEN = .TRUE.
      DO WHILE (QOPEN)
         I = I+1
         INQUIRE (UNIT=I, OPENED=QOPEN)
      END DO
      LUN = I
      RETURN
      END

C*GRLGTR -- translate logical name (MS-DOS)
C+
      SUBROUTINE GRLGTR (CNAME)
      CHARACTER CNAME*(*)
C
C Recursive translation of a logical name.
C Up to 20 levels of equivalencing can be handled.
C This is used in the parsing of device specifications in the
C VMS implementation of PGPLOT. In other implementations, it may
C be replaced by a null routine.
C
C Argument:
C  CNAME (input/output): initially contains the name to be
C       inspected.  If an equivalence is found it will be replaced
C       with the new name. If not, the old name will be left there. The
C       escape sequence at the beginning of process-permanent file
C       names is deleted and the '_' character at the beginning of
C       device names is left in place.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER CH*1
      CH = CNAME(1:1)
      RETURN
      END

C*GROPTX -- open output text file [MS-DOS]
C+
      INTEGER FUNCTION GROPTX (UNIT, NAME, DEFNAM, MODE)
      INTEGER UNIT,MODE
      CHARACTER*(*) NAME,DEFNAM
C
C Input:
C  UNIT : Fortran unit number to use
C  NAME : name of file to create
C  DEFNAM : default file name (used to fill in missing fields for VMS)
C
C Returns:
C  0 => success; any other value => error.
C-----------------------------------------------------------------------
      INTEGER IER
      CHARACTER CH*1
      CH = DEFNAM(1:1)
      IER = MODE
      OPEN (UNIT=UNIT, FILE=NAME, STATUS='UNKNOWN', IOSTAT=IER)
      GROPTX = IER
      RETURN 
C-----------------------------------------------------------------------
      END

C*GRTRML -- get name of user's terminal (MS-DOS)
C+
      SUBROUTINE GRTRML(CTERM, LTERM)
      CHARACTER CTERM*(*)
      INTEGER   LTERM
C
C Return the device name of the user's terminal, if any.
C
C Arguments:
C  CTERM : receives the terminal name, truncated or extended with
C           blanks as necessary.
C  LTERM : receives the number of characters in CTERM, excluding
C           trailing blanks. If there is not attached terminal,
C           zero is returned.
C--
C 1989-Nov-08
C-----------------------------------------------------------------------
      CTERM = 'CON'
      LTERM = 3
      RETURN
      END

C*GRTTER -- test whether device is user's terminal (MS-DOS)
C+
      SUBROUTINE GRTTER(CDEV, QSAME)
      CHARACTER CDEV*(*)
      LOGICAL   QSAME
C
C Return a logical flag indicating whether the supplied device
C name is a name for the user's controlling terminal or not.
C (Some PGPLOT programs wish to take special action if they are
C plotting on the user's terminal.)
C
C Arguments:
C  CDEV : (input) the device name to be tested.
C  QSAME   : (output) .TRUE. is CDEV contains a valid name for the
C           user's terminal; .FALSE. otherwise.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER CTERM*64
      INTEGER   LTERM
C
      CALL GRTRML(CTERM, LTERM)
      QSAME = (CDEV.EQ.CTERM(:LTERM))
      RETURN
      END

C*GRUSER -- get user name (MS-DOS)
C+
      SUBROUTINE GRUSER(CUSER, LUSER)
      CHARACTER CUSER*(*)
      INTEGER   LUSER
C
C Return the name of the user running the program.
C
C Arguments:
C  CUSER  : receives user name, truncated or extended with
C           blanks as necessary.
C  LUSER  : receives the number of characters in VALUE, excluding
C           trailing blanks.
C--
C 1989-Mar-19 - [AFT]
C-----------------------------------------------------------------------
C
      CALL GRGENV('USER', CUSER, LUSER)
      RETURN
      END

C*GRGFIL -- find data file -- PC version
C+
      SUBROUTINE GRGFIL(TYPE, NAME)
      CHARACTER*(*) TYPE, NAME
C
C This routine encsapsulates the algorithm for finding the PGPLOT
C run-time data files.
C
C 1. The binary font file: try the following in order:
C     file specified by PGPLOT_FONT
C     file "grfont.dat" in directory specified by PGPLOT_DIR
C                       (with or without '\' appended)
C     file "grfont.dat" in directory C:\PGPLOT\
C
C 2. The color-name database: try the following in order:
C     file specified by PGPLOT_RGB
C     file "rgb.txt" in directory specified by PGPLOT_DIR
C                       (with or without '\' appended)
C     file "rgb.txt" in directory C:\PGPLOT\
C
C Arguments:
C  TYPE (input)  : either 'FONT' or 'RGB' to request the corresponding
C                  file.
C  NAME (output) : receives the file name.
C--
C  2-Dec-1994 - new routine [TJP].
C 30-Apr-1996 - PC version, default C:\PGPLOT\, '\' [PAS]
C-----------------------------------------------------------------------
      CHARACTER*(*) DEFDIR, DEFFNT, DEFRGB
      PARAMETER  (DEFDIR='c:\pgplot\')
      PARAMETER  (DEFFNT='pgfont.txt')
      PARAMETER  (DEFRGB='rgb.txt')
      CHARACTER*255 FF
      CHARACTER*16 DEFLT
      INTEGER I, L, LD
      LOGICAL TEST, DEBUG
C
C Is debug output requested?
C
      CALL GRGENV('DEBUG', FF, L)
      DEBUG = L.GT.0
C
C Which file?
C
      IF (TYPE.EQ.'FONT') THEN
         DEFLT = DEFFNT
         LD = LEN(DEFFNT)
      ELSE IF (TYPE.EQ.'RGB') THEN
         DEFLT = DEFRGB
         LD = LEN(DEFRGB)
      ELSE
         CALL GRWARN('Internal error in routine GRGFIL')
      END IF
C
C Try each possibility in turn.
C
      DO 10 I=1,4
         IF (I.EQ.1) THEN
            CALL GRGENV(TYPE, FF, L)
         ELSE IF (I.EQ.2) THEN
            CALL GRGENV('DIR', FF, L)
            IF (L.GT.0) THEN
               FF(L+1:) = DEFLT
               L = L+LD
            END IF
         ELSE IF (I.EQ.3) THEN
            CALL GRGENV('DIR', FF, L)
            IF (L.GT.0) THEN
               FF(L+1:L+1) = '\'
               FF(L+2:) = DEFLT
               L = L+1+LD
            END IF
         ELSE IF (I.EQ.4) THEN
            FF = DEFDIR//DEFLT
            L = LEN(DEFDIR)+LD
         END IF
         IF (L.GT.0) THEN
            IF (DEBUG) THEN
               CALL GRWARN('Looking for '//FF(:L))
            END IF
            INQUIRE (FILE=FF(:L), EXIST=TEST)
            IF (TEST) THEN
               NAME = FF(:L)
               RETURN
            ELSE IF (DEBUG) THEN
               CALL GRWARN('WARNING: file not found')
            END IF
         END IF
 10   CONTINUE
C
C Failed to find the file.
C
      NAME = DEFLT
C-----------------------------------------------------------------------
      END
C*GRSY00 -- initialize font definition
C+
      SUBROUTINE GRSY00
C
C This routine must be called once in order to initialize the tables
C defining the symbol numbers to be used for ASCII characters in each
C font, and to read the character digitization from a file.
C
C Arguments: none.
C
C Implicit input:
C  The file with name specified in environment variable PGPLOT_FONT
C  is read, if it is available.
C  This is a binary file containing two arrays INDEX and BUFFER.
C  The digitization of each symbol occupies a number of words in
C  the INTEGER*2 array BUFFER; the start of the digitization
C  for symbol number N is in BUFFER(INDEX(N)), where INDEX is an
C  integer array of 3000 elements. Not all symbols 1...3000 have
C  a representation; if INDEX(N) = 0, the symbol is undefined.
C
*  PGPLOT uses the Hershey symbols for two `primitive' operations:
*  graph markers and text.  The Hershey symbol set includes several
*  hundred different symbols in a digitized form that allows them to
*  be drawn with a series of vectors (polylines).
*
*  The digital representation of all the symbols is stored in common
*  block /GRSYMB/.  This is read from a disk file at run time. The
*  name of the disk file is specified in environment variable
*  PGPLOT_FONT.
*
* Modules:
*
* GRSY00 -- initialize font definition
* GRSYDS -- decode character string into list of symbol numbers
* GRSYMK -- convert marker number into symbol number
* GRSYXD -- obtain the polyline representation of a given symbol
*
* PGPLOT calls these routines as follows:
*
* Routine          Called by
*
* GRSY00          GROPEN
* GRSYDS          GRTEXT, GRLEN
* GRSYMK          GRMKER,
* GRSYXD          GRTEXT, GRLEN, GRMKER
***********************************************************************
C--
C (2-Jan-1984)
C 22-Jul-1984 - revise to use DATA statements [TJP].
C  5-Jan-1985 - make missing font file non-fatal [TJP].
C  9-Feb-1988 - change default file name to Unix name; overridden
C               by environment variable PGPLOT_FONT [TJP].
C 29-Nov-1990 - move font assignment to GRSYMK.
C  7-Nov-1994 - look for font file in PGPLOT_DIR if PGPLOT_FONT is
C               undefined [TJP].
C-----------------------------------------------------------------------
      INTEGER*2  BUFFER(27000)
      INTEGER    FNTFIL, IER, INDEX(3000), NC1, NC2, NC3
      INTEGER    L, GRTRIM
      COMMON     /GRSYMB/ NC1, NC2, INDEX, BUFFER
      CHARACTER*128 FF
	!INTEGER    BUF(13500)
C
C Read the font file. If an I/O error occurs, it is ignored; the
C effect will be that all symbols will be undefined (treated as 
C blank spaces).
C
      CALL GRGFIL('FONT', FF)
      L = GRTRIM(FF)
      IF (L.LT.1) L = 1
      CALL GRGLUN(FNTFIL)

C      OPEN (UNIT=FNTFIL, FILE=FF(1:L), FORM='UNFORMATTED',
C     2      STATUS='OLD', IOSTAT=IER)
C      IF (IER.EQ.0) READ (UNIT=FNTFIL, IOSTAT=IER) 
C     1            NC1,NC2,NC3,INDEX,BUFFER
C      IF (IER.EQ.0) CLOSE (UNIT=FNTFIL, IOSTAT=IER)

    !OPEN (UNIT=FNTFIL, FILE=FF(1:L),status='old')
	  OPEN (UNIT=FNTFIL, FILE='pgfont.txt',status='old')
	  read(FNTFIL,*)NC1,NC2,NC3
	  read(FNTFIL,*)(index(i),i=1,3000)
	  read(FNTFIL,*)(buffer(i),i=1,27000)
	  close(FNTFIL)

      CALL GRFLUN(FNTFIL)
	  ier=0
      IF (IER.NE.0) THEN
         CALL GRWARN('Unable to read font file: '//FF(:L))
      ENDIF
      RETURN
      END
C*GRAREA -- define a clipping window
C+
      SUBROUTINE GRAREA (IDENT,X0,Y0,XSIZE,YSIZE)
C
C GRPCKG: Define a rectangular window in the current plotting area. All
C graphics (except characters written with GRCHAR) will be blanked
C outside this window.  The default window is the full plotting area
C defined by default or by GRSETS.
C
C Arguments:
C
C IDENT (input, integer): the plot identifier, returned by GROPEN.
C X0, Y0 (input, real): the lower left corner of the window, in absolute
C       device coordinates.
C XSIZE, YSIZE (input, real): width and height of the window in absolute
C       coordinates; if either is negative, the window will be reset to
C       the full plotting area.
C--
C  1-Feb-1983 - [TJP].
C 25-Nov-1994 - use floating-point [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER IDENT
      REAL X0, Y0, XSIZE, YSIZE
C
      CALL GRSLCT(IDENT)
C
      IF ((XSIZE.LE.0.0) .OR. (YSIZE.LE.0.0)) THEN
          GRXMIN(IDENT) = 0
          GRXMAX(IDENT) = GRXMXA(IDENT)
          GRYMIN(IDENT) = 0
          GRYMAX(IDENT) = GRYMXA(IDENT)
      ELSE
          GRXMIN(IDENT) = MAX(X0,0.0)
          GRYMIN(IDENT) = MAX(Y0,0.0)
          GRXMAX(IDENT) = MIN(XSIZE+X0,REAL(GRXMXA(IDENT)))
          GRYMAX(IDENT) = MIN(YSIZE+Y0,REAL(GRYMXA(IDENT)))
      END IF
C
      END
C*GRBPIC -- begin picture
C+
      SUBROUTINE GRBPIC
C
C GRPCKG (internal routine). Send a "begin picture" command to the
C device driver, and send commands to set deferred attributes (color,
C line width, etc.)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL RBUF(2)
      INTEGER NBUF, LCHR
      CHARACTER*20 CHR
C
      GRPLTD(GRCIDE) = .TRUE.
      IF (GRGTYP.GT.0) THEN
C         -- begin picture
          RBUF(1) = GRXMXA(GRCIDE)
          RBUF(2) = GRYMXA(GRCIDE)
          NBUF = 2
          CALL GREXEC(GRGTYP,11,RBUF,NBUF,CHR,LCHR)
C         -- set color index
          RBUF(1) = GRCCOL(GRCIDE)
          NBUF = 1
          CALL GREXEC(GRGTYP,15,RBUF,NBUF,CHR,LCHR)
C         -- set line width
          IF (GRGCAP(GRCIDE)(5:5).EQ.'T') THEN
              RBUF(1) = ABS(GRWIDT(GRCIDE))
              NBUF = 1
              CALL GREXEC(GRGTYP,22,RBUF,NBUF,CHR,LCHR)
          END IF
C         -- set hardware dashing
          IF (GRGCAP(GRCIDE)(3:3).EQ.'D') THEN
              RBUF(1) = GRSTYL(GRCIDE)
              NBUF = 1
              CALL GREXEC(GRGTYP,19,RBUF,NBUF,CHR,LCHR)
          END IF
      END IF
C
      END
C+
***********************************************************************
*                                                                     *
*  PGPLOT Fortran Graphics Subroutine Library                         *
*                                                                     *
*  T. J. Pearson, California Institute of Technology,                 *
*  Pasadena, California 91125.                                        *
*                                                                     *
*  Routines for handling the obsolete character set                   *
*  ------------------------------------------------                   *
*  These routines are not called by PGPLOT but are called by some     *
*  old user-written programs.                                         *
***********************************************************************

******* Index of Modules **********************************************

* GRCHAR -- draw a string of characters
* GRCHR0 -- support routine for GRCHAR and GRMARK
* GRDAT2 -- character set definition (block data)
* GRGTC0 -- obtain character digitization
* GRMARK -- mark points with specified symbol

***********************************************************************
C--

C*GRCHAR -- draw a string of characters
C+
      SUBROUTINE GRCHAR (IDENT,CENTER,ORIENT,ABSXY,X0,Y0,STRING)
C
C GRPCKG: Draw a string of characters. The plot is not windowed
C in the current subarea, but in the full plotting area.
C
C Arguments:
C
C IDENT (input, integer): plot identifier, as returned by GROPEN.
C CENTER (input, logical): if .TRUE., the first character of the string
C      is centered at (X0,Y0); otherwise the bottom left corner of the
C      first character is placed at (X0,Y0).
C ORIENT (input, real): the angle in degrees that the string is to make
C      with the horizontal, increasing anticlockwise.
C ABSXY (input, logical): if .TRUE., (X0,Y0) are absolute device
C      coordinates; otherwise they are world coordinates (the scaling
C      transformation is applied).
C X0, Y0 (input, real): position of first character (see CENTER).
C STRING (input, character): the string of ASCII characters; control
C      characters 0-20 have special representations; all other
C      non-graphic characters are plotted as blank spaces.
C
C (1-Feb-1983)
C-----------------------------------------------------------------------
      CHARACTER*(*) STRING
      INTEGER  IDENT
      LOGICAL  ABSXY, CENTER
      REAL     ORIENT, X0, Y0
C
      CALL GRSLCT(IDENT)
      CALL GRCHR0(.FALSE., CENTER, ORIENT, ABSXY, X0, Y0, STRING)
      RETURN
      END
C*GRCHR0 -- support routine for GRCHAR and GRMARK
C+
      SUBROUTINE GRCHR0 (WINDOW,CENTER,ORIENT,ABSXY,X0,Y0,STRING)
C
C GRPCKG (internal routine): Support routine for GRCHAR and GRMARK.
C Draw a string of characters.
C
C Arguments:
C
C WINDOW (input, logical): if .TRUE., the plot is windowed in the
C      current window.
C CENTER (input, logical): if .TRUE., the first character of the string
C      is centered at (X0,Y0); otherwise the bottom left corner of the
C      first character is placed at (X0,Y0).
C ORIENT (input, real): the angle in degrees that the string is to make
C      with the horizontal, increasing anticlockwise.
C ABSXY (input, logical): if .TRUE., (X0,Y0) are absolute device
C      coordinates; otherwise they are world coordinates (the scaling
C      transformation is applied).
C X0, Y0 (input, real): position of first character (see CENTER).
C STRING (input, character): the string of ASCII characters; control
C      characters 0-20 have special representations; all other
C      non-graphic characters are plotted as blank spaces.
C
C (1-Mar-1983)
C-----------------------------------------------------------------------
      INTEGER  DOT, MOVE, VECSIZ
      REAL     PI
      PARAMETER (DOT = 3)
      PARAMETER (MOVE = 2)
      PARAMETER (VECSIZ = 30)
      PARAMETER (PI = 3.14159265359)
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) STRING
      CHARACTER*1   NEXT
      REAL     XMIN, XMAX, YMIN, YMAX
      INTEGER  MODE,LSTYLE,LEVEL
      INTEGER  I, J, L, CH, POINTS
      LOGICAL  ABSXY, CENTER, MORE, WINDOW
      REAL     ORIENT, X0, Y0
      REAL     ANGLE, FACTOR, BASE, FAC
      REAL     COSA, SINA
      REAL     DX, DY, XORG, YORG
      REAL     XC(VECSIZ), YC(VECSIZ), XT, YT
C
      IF (LEN(STRING).LE.0) RETURN
C
C Compute scaling and orientation.
C
      CALL GRQLS(LSTYLE)
      CALL GRSLS(1)
      ANGLE = (AMOD(ORIENT, 360.0) / 180.0) * PI
      FACTOR = GRCFAC(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
      DX = 10.0 * COSA
      DY = 10.0 * SINA
      CALL GRTXY0(ABSXY, X0, Y0, XORG, YORG)
      IF (.NOT.WINDOW) THEN
          XMIN = GRXMIN(GRCIDE)
          XMAX = GRXMAX(GRCIDE)
          YMIN = GRYMIN(GRCIDE)
          YMAX = GRYMAX(GRCIDE)
          CALL GRAREA(GRCIDE, 0.0, 0.0, 0.0, 0.0)
      END IF
C
C Plot the string of characters.
C
      MODE = MOVE
      BASE = 0.0
      FAC = 1.0
      I = 1
      LEVEL = 0
      L = LEN(STRING)
C     -- DO WHILE (I.LE.L)
   10 IF (I.LE.L) THEN
        IF (I.LT.L .AND. STRING(I:I).EQ.CHAR(92)) THEN
            CALL GRTOUP(NEXT,STRING(I+1:I+1))
            IF (NEXT.EQ.'U') THEN
                LEVEL = LEVEL+1
                BASE = BASE + 4.0*FAC
                FAC = 0.6**IABS(LEVEL)
                I = I+2
            ELSE IF (NEXT.EQ.'D') THEN
                LEVEL = LEVEL-1
                FAC = 0.6**IABS(LEVEL)
                BASE = BASE - 4.0*FAC
                I = I+2
            ELSE
                I = I+1
            END IF
        ELSE
          CH = ICHAR(STRING(I:I))
          IF (CH.GT.127 .OR. CH.LT.0) CH = ICHAR(' ')
          MORE = .TRUE.
C         -- DO WHILE (MORE)
   20     IF (MORE) THEN
            CALL GRGTC0(CH, CENTER, POINTS, XC, YC, MORE)
            DO 30 J=1,POINTS
                    XT = XC(J)*FAC
                    YT = YC(J)*FAC + BASE
                    XC(J) = XORG + COSA * XT - SINA * YT
                    YC(J) = YORG + SINA * XT + COSA * YT
   30       CONTINUE
            IF (POINTS.EQ.1) MODE = DOT
            IF (POINTS.GT.0) CALL GRVCT0(MODE,.TRUE.,POINTS,XC,YC)
            IF (POINTS.EQ.1) MODE = MOVE
          GOTO 20
          END IF
C         -- end DO WHILE
          XORG = XORG + DX*FAC
          YORG = YORG + DY*FAC
          I = I+1
        END IF
      GOTO 10
      END IF
C     -- end DO WHILE
C
C Clean up and return.
C
      IF (.NOT.WINDOW) THEN
          GRXMIN(GRCIDE) = XMIN
          GRXMAX(GRCIDE) = XMAX
          GRYMIN(GRCIDE) = YMIN
          GRYMAX(GRCIDE) = YMAX
      END IF
      CALL GRSLS(LSTYLE)
      RETURN
      END

C*GRCHSZ -- inquire default character attributes
C+
      SUBROUTINE GRCHSZ (IDENT,XSIZE,YSIZE,XSPACE,YSPACE)
C
C GRPCKG: Obtain the default character attributes.
C
C Arguments:
C
C IDENT (input, integer): the plot identifier, returned by GROPEN.
C XSIZE, YSIZE (output, real): the default character size
C      (absolute device units).
C XSPACE, YSPACE (output, real): the default character spacing
C      (absolute units); XSPACE is the distance between the lower left
C      corners of adjacent characters in a plotted string; YSPACE
C      is the corresponding vertical spacing.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IDENT
      REAL     FACTOR, XSIZE, YSIZE, XSPACE, YSPACE
C
      CALL GRSLCT(IDENT)
      FACTOR = GRCSCL(IDENT)
      XSIZE = GRCXSZ * FACTOR
      YSIZE = GRCYSZ * FACTOR
      XSPACE = 10.0 * FACTOR
      YSPACE = 13.0 * FACTOR
      END
C*GRCLIP -- clip a point against clipping rectangle
C+
      SUBROUTINE GRCLIP (X,Y,XMIN,XMAX,YMIN,YMAX,C)
      REAL X,Y
      REAL XMIN,XMAX,YMIN,YMAX
      INTEGER C
C
C GRPCKG (internal routine): support routine for the clipping algorithm;
C called from GRLIN0 only. C is a 4 bit code indicating the relationship
C between point (X,Y) and the window boundaries; 0 implies the point is
C within the window.
C
C Arguments:
C--
C (11-Feb-1983)
C Revised 20-Jun-1985 (TJP); use floating arithmetic
C Revised 12-Jun-1992 (TJP); clip exactly on the boundary
C-----------------------------------------------------------------------
C
      C = 0
      IF (X.LT.XMIN) THEN
          C = 1
      ELSE IF (X.GT.XMAX) THEN
          C = 2
      END IF
      IF (Y.LT.YMIN) THEN
          C = C+4
      ELSE IF (Y.GT.YMAX) THEN
          C = C+8
      END IF
      END
C*GRCLOS -- close graphics device
C+
      SUBROUTINE GRCLOS
C
C GRPCKG: Close the open plot on the current device. Any pending output
C is sent to the device, the device is released for other users or the
C disk file is closed, and no further plotting is allowed on the device
C without a new call to GROPEN.
C
C Arguments: none.
C--
C  1-Jun-1984 - [TJP].
C 17-Jul-1984 - ignore call if plot is not open [TJP].
C  1-Oct-1984 - reset color to default (1) and position text cursor
C               at bottom of VT screen [TJP].
C 19-Oct-1984 - add VV device [TJP].
C 22-Dec-1984 - use GRBUFL and GRIOTA parameters [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - modify END_PICTURE sequence [AFT].
C 11-Jun-1987 - remove built-ins [TJP].
C 31-Aug-1987 - do not eject blank page [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER CHR
C
C Check a plot is open.
C
      IF (GRCIDE.LT.1) RETURN
C
C Reset color to default (1). This is useful
C for VT240 terminals, which use the color tables for text.
C
      CALL GRSCI(1)
C
C Flush buffer.
C
      CALL GRTERM
C
C End picture.
C
      CALL GREPIC
C
C This plot identifier is no longer in use.
C Set state to "workstation closed".
C
      GRSTAT(GRCIDE) = 0
      GRCIDE = 0
C
C Close workstation.
C
      CALL GREXEC(GRGTYP,10,RBUF,NBUF,CHR,LCHR)
C
      END
C*GRCLPL -- clip line against clipping rectangle
C+
      SUBROUTINE GRCLPL (X0,Y0,X1,Y1,VIS)
C
C GRPCKG (internal routine): Change the end-points of the line (X0,Y0)
C (X1,Y1) to clip the line at the window boundary.  The algorithm is
C that of Cohen and Sutherland (ref: Newman & Sproull).
C
C Arguments:
C
C X0, Y0 (input/output, real): device coordinates of starting point
C       of line.
C X1, Y1 (input/output, real): device coordinates of end point of line.
C VIS (output, logical): .TRUE. if line lies wholly or partially
C       within the clipping rectangle; .FALSE. if it lies entirely
C       outside the rectangle.
C--
C 13-Jul-1984 - [TJP].
C 20-Jun-1985 - [TJP] - revise clipping algorithm.
C 28-Jun-1991 - [TJP] - use IAND().
C 12-Jun-1992 - [TJP] - clip exactly on the boundary.
C
C Caution: IAND is a non-standard intrinsic function to do bitwise AND
C of two integers. If it is not supported by your Fortran compiler, you
C will need to modify this routine or supply an IAND function.
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL  VIS
      INTEGER  C0,C1,C
      REAL     XMIN,XMAX,YMIN,YMAX
      REAL     X,Y, X0,Y0, X1,Y1
C      INTEGER IAND
C
      XMIN = GRXMIN(GRCIDE)
      YMIN = GRYMIN(GRCIDE)
      XMAX = GRXMAX(GRCIDE)
      YMAX = GRYMAX(GRCIDE)
      CALL GRCLIP(X0,Y0,XMIN,XMAX,YMIN,YMAX,C0)
      CALL GRCLIP(X1,Y1,XMIN,XMAX,YMIN,YMAX,C1)
   10 IF (C0.NE.0 .OR. C1.NE.0) THEN
          IF (IAND(C0,C1).NE.0) THEN
C             ! line is invisible
              VIS = .FALSE.
              RETURN
          END IF
          C = C0
          IF (C.EQ.0) C = C1
          IF (IAND(C,1).NE.0) THEN
C             ! crosses XMIN
              Y = Y0 + (Y1-Y0)*(XMIN-X0)/(X1-X0)
              X = XMIN
          ELSE IF (IAND(C,2).NE.0) THEN
C             ! crosses XMAX
              Y = Y0 + (Y1-Y0)*(XMAX-X0)/(X1-X0)
              X = XMAX
          ELSE IF (IAND(C,4).NE.0) THEN
C             ! crosses YMIN
              X = X0 + (X1-X0)*(YMIN-Y0)/(Y1-Y0)
              Y = YMIN
          ELSE IF (IAND(C,8).NE.0) THEN
C             ! crosses YMAX
              X = X0 + (X1-X0)*(YMAX-Y0)/(Y1-Y0)
              Y = YMAX
          END IF
          IF (C.EQ.C0) THEN
              X0 = X
              Y0 = Y
              CALL GRCLIP(X,Y,XMIN,XMAX,YMIN,YMAX,C0)
          ELSE
              X1 = X
              Y1 = Y
              CALL GRCLIP(X,Y,XMIN,XMAX,YMIN,YMAX,C1)
          END IF
      GOTO 10
      END IF
      VIS = .TRUE.
      END
C*GRCTOI -- convert character string to integer
C+
      INTEGER FUNCTION GRCTOI (S, I)
      CHARACTER*(*) S
      INTEGER I
C
C GRCTOI: attempt to read an integer from a character string, and return
C the result. No attempt is made to avoid integer overflow. A valid 
C integer is any sequence of decimal digits.
C
C Returns:
C  GRCTOI           : the value of the integer; if the first character
C                    read is not a decimal digit, the value returned
C                    is zero.
C Arguments:
C  S      (input)  : character string to be parsed.
C  I      (in/out) : on input, I is the index of the first character
C                    in S to be examined; on output, either it points
C                    to the next character after a valid integer, or
C                    it is equal to LEN(S)+1.
C
C--
C  1985 Oct  8 - New routine, based on CTOI (T. J. Pearson).
C  1997 Jun  3 - allow leading + or - sign (TJP).
C-----------------------------------------------------------------------
      INTEGER K, SIGN, X
      CHARACTER*1 DIGITS(0:9)
      DATA  DIGITS/'0','1','2','3','4','5','6','7','8','9'/
C
      X = 0
      SIGN = +1
      IF (I.GT.LEN(S)) GOTO 30
      IF (S(I:I).EQ.'+') THEN
         I = I+1
      ELSE IF (S(I:I).EQ.'-') THEN
         I = I+1
         SIGN = -1
      END IF
 10   IF (I.GT.LEN(S)) GOTO 30
      DO 20 K=0,9
         IF (S(I:I).EQ.DIGITS(K)) THEN
            X = X*10 + K
            I = I+1
            GOTO 10
         END IF
 20   CONTINUE
 30   GRCTOI = X*SIGN
      RETURN
      END
C*GRCURS -- read cursor position
C+
      INTEGER FUNCTION GRCURS (IDENT,IX,IY,IXREF,IYREF,MODE,POSN,CH)
      INTEGER IDENT, IX, IY, IXREF, IYREF, MODE, POSN
      CHARACTER*(*) CH
C
C GRPCKG: Read the cursor position and a character typed by the user.
C The position is returned in absolute device coordinates (pixels).
C GRCURS positions the cursor at the position specified, and
C allows the user to move the cursor using the joystick or
C arrow keys or whatever is available on the device. When he has
C positioned the cursor, the user types a single character on his
C keyboard; GRCURS then returns this character and the new cursor
C position.
C
C "Rubber band" feedback of cursor movement can be requested (although
C it may not be supported on some devices). If MODE=1, a line from
C the anchor point to the current cursor position is displayed as
C the cursor is moved. If MODE=2, a rectangle with vertical and
C horizontal sides and one vertex at the anchor point and the opposite
C vertex at the current cursor position is displayed as the cursor is
C moved.
C
C Returns:
C
C GRCURS (integer): 1 if the call was successful; 0 if the device
C      has no cursor or some other error occurs. 
C
C Arguments:
C
C IDENT (integer, input):  GRPCKG plot identifier (from GROPEN).
C IX    (integer, in/out): the device x-coordinate of the cursor.
C IY    (integer, in/out): the device y-coordinate of the cursor.
C IXREF (integer, input):  x-coordinate of anchor point.
C IYREF (integer, input):  y-coordinate of anchor point.
C MODE  (integer, input):  type of rubber-band feedback.
C CH    (char,    output): the character typed by the user; if the device
C      has no cursor or if some other error occurs, the value CHAR(0)
C      [ASCII NUL character] is returned.
C--
C  1-Aug-1984 - extensively revised [TJP].
C 29-Jan-1985 - add ARGS and HP2648 devices (?) [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 11-Jun-1987 - remove built-ins [TJP].
C 15-Feb-1988 - remove test for batch jobs; leave this to the device
C               handler [TJP].
C 13-Dec-1990 - remove code to abort after 10 cursor errors [TJP].
C  7-Sep-1994 - add support for rubber-band modes [TJP].
C 17-Jan-1995 - start picture if necessary [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL           RBUF(6)
      INTEGER        NBUF, LCHR, ICURS, ERRCNT
      CHARACTER*16   CHR
      CHARACTER      C
      SAVE           ERRCNT
      DATA           ERRCNT/0/
C
C Validate identifier, and select device.
C
      CALL GRSLCT(IDENT)
      CALL GRTERM
C
C Begin picture if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Make sure cursor is on view surface. (It does not
C have to be in the viewport.)
C
      IX = MAX(0,MIN(GRXMXA(GRCIDE),IX))
      IY = MAX(0,MIN(GRYMXA(GRCIDE),IY))
C
C Does the device have a cursor?
C
      C = GRGCAP(GRCIDE)(2:2)
      ICURS = 0
      IF (C.EQ.'C' .OR. C.EQ.'X') ICURS=1
C
C Device does have a cursor.
C
      IF (ICURS.GT.0) THEN
C         -- initial position of cursor
          RBUF(1) = IX
          RBUF(2) = IY
C         -- reference point for rubber band
          RBUF(3) = IXREF
          RBUF(4) = IYREF
C         -- rubber band mode
          RBUF(5) = MODE
C         -- position cursor?
          RBUF(6) = POSN
          NBUF = 6
          LCHR = 0
          CALL GREXEC(GRGTYP,17,RBUF,NBUF,CHR,LCHR)
          IX = RBUF(1)
          IY = RBUF(2)
          CH = CHR(1:1)
          GRCURS = 1
C         -- error if driver returns NUL
          IF (ICHAR(CHR(1:1)).EQ.0) GRCURS = 0
C
C Other devices are illegal.
C
      ELSE
          CALL GREXEC(GRGTYP, 1,RBUF,NBUF,CHR,LCHR)
          LCHR = INDEX(CHR,' ')
          IF (ERRCNT.LE.10) CALL 
     1        GRWARN('output device has no cursor: '//CHR(:LCHR))
          CH = CHAR(0)
          GRCURS = 0
          ERRCNT = ERRCNT+1
      END IF
C
      END

C*GRDAT2 -- character set definition (block data)
C+
      BLOCK DATA GRDAT2
C
C GRPCKG (internal routine): Block data for to define the character set.
C
C Arguments: none.
C
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INTEGER   CTD1, CTD2
      PARAMETER (CTD1 = 30)
      PARAMETER (CTD2 = 128)
C
      INTEGER   CINDX1, CINDX2
      INTEGER   CHTBL(CTD1,CTD2)
      INTEGER   SPCH00(CTD1), SPCH01(CTD1), SPCH02(CTD1), SPCH03(CTD1)
     1        , SPCH04(CTD1), SPCH05(CTD1), SPCH06(CTD1), SPCH07(CTD1)
     2        , SPCH08(CTD1), SPCH09(CTD1), SPCH10(CTD1), SPCH11(CTD1)
     3        , SPCH12(CTD1), SPCH13(CTD1), SPCH14(CTD1), SPCH15(CTD1)
     4        , SPCH16(CTD1), SPCH17(CTD1), SPCH18(CTD1), SPCH19(CTD1)
     5        , SPCH20(CTD1), SPCH21(CTD1), SPCH22(CTD1), SPCH23(CTD1)
     6        , SPCH24(CTD1), SPCH25(CTD1), SPCH26(CTD1), SPCH27(CTD1)
     7        , SPCH28(CTD1), SPCH29(CTD1), SPCH30(CTD1), SPCH31(CTD1)
     8        , SPACE (CTD1), EXCLAM(CTD1), QUOTE (CTD1), POUND (CTD1)
     9        , DOLLAR(CTD1), PERCNT(CTD1), AMPERS(CTD1), APOSTR(CTD1)
     A        , LPAREN(CTD1), RPAREN(CTD1), ASTER (CTD1), PLUS  (CTD1)
     B        , COMMA (CTD1), MINUS (CTD1), PERIOD(CTD1), SLASH (CTD1)
     C        , ZERO  (CTD1), ONE   (CTD1), TWO   (CTD1), THREE (CTD1)
     D        , FOUR  (CTD1), FIVE  (CTD1), SIX   (CTD1), SEVEN (CTD1)
     E        , EIGHT (CTD1), NINE  (CTD1), COLON (CTD1), SEMICO(CTD1)
     F        , LESS  (CTD1), EQUALS(CTD1), GREATR(CTD1), QUESTN(CTD1)
      INTEGER   ATSIGN(CTD1), AUPPER(CTD1), BUPPER(CTD1), CUPPER(CTD1)
     1        , DUPPER(CTD1), EUPPER(CTD1), FUPPER(CTD1), GUPPER(CTD1)
     2        , HUPPER(CTD1), IUPPER(CTD1), JUPPER(CTD1), KUPPER(CTD1)
     3        , LUPPER(CTD1), MUPPER(CTD1), NUPPER(CTD1), OUPPER(CTD1)
     4        , PUPPER(CTD1), QUPPER(CTD1), RUPPER(CTD1), SUPPER(CTD1)
     5        , TUPPER(CTD1), UUPPER(CTD1), VUPPER(CTD1), WUPPER(CTD1)
     6        , XUPPER(CTD1), YUPPER(CTD1), ZUPPER(CTD1), LBRACK(CTD1)
     7        , BKSLSH(CTD1), RBRACK(CTD1), CARET (CTD1), USCORE(CTD1)
     8        , ACCENT(CTD1), ALOWER(CTD1), BLOWER(CTD1), CLOWER(CTD1)
     9        , DLOWER(CTD1), ELOWER(CTD1), FLOWER(CTD1), GLOWER(CTD1)
     A        , HLOWER(CTD1), ILOWER(CTD1), JLOWER(CTD1), KLOWER(CTD1)
     B        , LLOWER(CTD1), MLOWER(CTD1), NLOWER(CTD1), OLOWER(CTD1)
     C        , PLOWER(CTD1), QLOWER(CTD1), RLOWER(CTD1), SLOWER(CTD1)
     D        , TLOWER(CTD1), ULOWER(CTD1), VLOWER(CTD1), WLOWER(CTD1)
     E        , XLOWER(CTD1), YLOWER(CTD1), ZLOWER(CTD1), LBRACE(CTD1)
     F        , ORSIGN(CTD1), RBRACE(CTD1), TILDE (CTD1), SPC127(CTD1)
      EQUIVALENCE (SPCH00, CHTBL(1,   1)), (SPCH01, CHTBL(1,   2))
     1          , (SPCH02, CHTBL(1,   3)), (SPCH03, CHTBL(1,   4))
     2          , (SPCH04, CHTBL(1,   5)), (SPCH05, CHTBL(1,   6))
     3          , (SPCH06, CHTBL(1,   7)), (SPCH07, CHTBL(1,   8))
     4          , (SPCH08, CHTBL(1,   9)), (SPCH09, CHTBL(1,  10))
     5          , (SPCH10, CHTBL(1,  11)), (SPCH11, CHTBL(1,  12))
     6          , (SPCH12, CHTBL(1,  13)), (SPCH13, CHTBL(1,  14))
     7          , (SPCH14, CHTBL(1,  15)), (SPCH15, CHTBL(1,  16))
     8          , (SPCH16, CHTBL(1,  17)), (SPCH17, CHTBL(1,  18))
     9          , (SPCH18, CHTBL(1,  19)), (SPCH19, CHTBL(1,  20))
     A          , (SPCH20, CHTBL(1,  21)), (SPCH21, CHTBL(1,  22))
     B          , (SPCH22, CHTBL(1,  23)), (SPCH23, CHTBL(1,  24))
     C          , (SPCH24, CHTBL(1,  25)), (SPCH25, CHTBL(1,  26))
     D          , (SPCH26, CHTBL(1,  27)), (SPCH27, CHTBL(1,  28))
     E          , (SPCH28, CHTBL(1,  29)), (SPCH29, CHTBL(1,  30))
     F          , (SPCH30, CHTBL(1,  31)), (SPCH31, CHTBL(1,  32))
      EQUIVALENCE (SPACE , CHTBL(1,  33)), (EXCLAM, CHTBL(1,  34))
     1          , (QUOTE , CHTBL(1,  35)), (POUND , CHTBL(1,  36))
     2          , (DOLLAR, CHTBL(1,  37)), (PERCNT, CHTBL(1,  38))
     3          , (AMPERS, CHTBL(1,  39)), (APOSTR, CHTBL(1,  40))
     4          , (LPAREN, CHTBL(1,  41)), (RPAREN, CHTBL(1,  42))
     5          , (ASTER , CHTBL(1,  43)), (PLUS  , CHTBL(1,  44))
     6          , (COMMA , CHTBL(1,  45)), (MINUS , CHTBL(1,  46))
     7          , (PERIOD, CHTBL(1,  47)), (SLASH , CHTBL(1,  48))
     8          , (ZERO  , CHTBL(1,  49)), (ONE   , CHTBL(1,  50))
     9          , (TWO   , CHTBL(1,  51)), (THREE , CHTBL(1,  52))
     A          , (FOUR  , CHTBL(1,  53)), (FIVE  , CHTBL(1,  54))
     B          , (SIX   , CHTBL(1,  55)), (SEVEN , CHTBL(1,  56))
     C          , (EIGHT , CHTBL(1,  57)), (NINE  , CHTBL(1,  58))
     D          , (COLON , CHTBL(1,  59)), (SEMICO, CHTBL(1,  60))
     E          , (LESS  , CHTBL(1,  61)), (EQUALS, CHTBL(1,  62))
     F          , (GREATR, CHTBL(1,  63)), (QUESTN, CHTBL(1,  64))
      EQUIVALENCE (ATSIGN, CHTBL(1,  65)), (AUPPER, CHTBL(1,  66))
     1          , (BUPPER, CHTBL(1,  67)), (CUPPER, CHTBL(1,  68))
     2          , (DUPPER, CHTBL(1,  69)), (EUPPER, CHTBL(1,  70))
     3          , (FUPPER, CHTBL(1,  71)), (GUPPER, CHTBL(1,  72))
     4          , (HUPPER, CHTBL(1,  73)), (IUPPER, CHTBL(1,  74))
     5          , (JUPPER, CHTBL(1,  75)), (KUPPER, CHTBL(1,  76))
     6          , (LUPPER, CHTBL(1,  77)), (MUPPER, CHTBL(1,  78))
     7          , (NUPPER, CHTBL(1,  79)), (OUPPER, CHTBL(1,  80))
     8          , (PUPPER, CHTBL(1,  81)), (QUPPER, CHTBL(1,  82))
     9          , (RUPPER, CHTBL(1,  83)), (SUPPER, CHTBL(1,  84))
     A          , (TUPPER, CHTBL(1,  85)), (UUPPER, CHTBL(1,  86))
     B          , (VUPPER, CHTBL(1,  87)), (WUPPER, CHTBL(1,  88))
     C          , (XUPPER, CHTBL(1,  89)), (YUPPER, CHTBL(1,  90))
     D          , (ZUPPER, CHTBL(1,  91)), (LBRACK, CHTBL(1,  92))
     E          , (BKSLSH, CHTBL(1,  93)), (RBRACK, CHTBL(1,  94))
     F          , (CARET , CHTBL(1,  95)), (USCORE, CHTBL(1,  96))
      EQUIVALENCE (ACCENT, CHTBL(1,  97)), (ALOWER, CHTBL(1,  98))
     1          , (BLOWER, CHTBL(1,  99)), (CLOWER, CHTBL(1, 100))
     2          , (DLOWER, CHTBL(1, 101)), (ELOWER, CHTBL(1, 102))
     3          , (FLOWER, CHTBL(1, 103)), (GLOWER, CHTBL(1, 104))
     4          , (HLOWER, CHTBL(1, 105)), (ILOWER, CHTBL(1, 106))
     5          , (JLOWER, CHTBL(1, 107)), (KLOWER, CHTBL(1, 108))
     6          , (LLOWER, CHTBL(1, 109)), (MLOWER, CHTBL(1, 110))
     7          , (NLOWER, CHTBL(1, 111)), (OLOWER, CHTBL(1, 112))
     8          , (PLOWER, CHTBL(1, 113)), (QLOWER, CHTBL(1, 114))
     9          , (RLOWER, CHTBL(1, 115)), (SLOWER, CHTBL(1, 116))
     A          , (TLOWER, CHTBL(1, 117)), (ULOWER, CHTBL(1, 118))
     B          , (VLOWER, CHTBL(1, 119)), (WLOWER, CHTBL(1, 120))
     C          , (XLOWER, CHTBL(1, 121)), (YLOWER, CHTBL(1, 122))
     D          , (ZLOWER, CHTBL(1, 123)), (LBRACE, CHTBL(1, 124))
     E          , (ORSIGN, CHTBL(1, 125)), (RBRACE, CHTBL(1, 126))
     F          , (TILDE , CHTBL(1, 127)), (SPC127, CHTBL(1, 128))
C
      COMMON /GRCS02/ CINDX1, CINDX2, CHTBL
C
      DATA CINDX1 /1/
      DATA CINDX2 /0/
C
      DATA SPCH00 /07, 34, 37, 67, 61, 01, 07, 37, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH01 /11, 34, 37, 47, 65, 63, 41, 21, 03, 05
     1           , 27, 37, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH02 /07, 34, 37, 64, 61, 01, 04, 37, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH03 /02, 04, 64, 02, 37, 31, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH04 /02, 01, 67, 02, 07, 61, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH05 /06, 34, 37, 64, 31, 04, 37, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH06 /05, 31, 37, 64, 04, 37, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH07 /04, 01, 67, 07, 61, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH08 /04, 07, 67, 01, 61, 02, 14, 54, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH09 /03, 07, 34, 67, 02, 34, 31, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH10 /06, 61, 52, 56, 16, 12, 52, 02, 01, 12
     1           , 02, 07, 16, 02, 67, 34, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH11 /02, 01, 67, 02, 07, 61, 02, 04, 64, 02
     1           , 37, 31, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH12 /05, 01, 67, 07, 61, 01, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH13 /02, 24, 44, 02, 37, 31, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH14 /02, 07, 67, 02, 01, 61, 05, 31, 64, 37
     1           , 04, 31, 01, 34, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH15 /07, 23, 43, 44, 24, 25, 45, 44, 02, 35
     1           , 33, 02, 23, 24, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH16 /27, 01, 61, 62, 02, 03, 63, 64, 04, 05
     1           , 65, 66, 06, 07, 67, 61, 51, 57, 47, 41
     2           , 31, 37, 27, 21, 11, 17, 07, 01, 00, 00/
      DATA SPCH17 /14, 21, 41, 52, 12, 03, 63, 64, 04, 05
     1           , 65, 56, 16, 27, 47, 14, 03, 05, 16, 12
     2           , 21, 27, 37, 31, 41, 47, 56, 52, 63, 65/
      DATA SPCH18 /12, 31, 42, 22, 13, 53, 64, 04, 15, 55
     1           , 46, 26, 37, 12, 64, 55, 53, 42, 46, 37
     2           , 31, 22, 26, 15, 13, 04, 00, 00, 00, 00/
      DATA SPCH19 /09, 26, 15, 13, 22, 42, 53, 55, 46, 26
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH20 /09, 27, 05, 03, 21, 41, 63, 65, 47, 27
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH21 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH22 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH23 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH24 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH25 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH26 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH27 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH28 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH29 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH30 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPCH31 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPACE  /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA EXCLAM /02, 38, 33, 01, 30, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA QUOTE  /02, 28, 26, 02, 48, 46, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA POUND  /02, 10, 18, 02, 58, 50, 02, 62, 02, 02
     1           , 06, 66, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA DOLLAR /10, 01, 51, 62, 63, 54, 14, 05, 06, 17
     1           , 67, 02, 38, 30, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA PERCNT /05, 07, 18, 27, 16, 07, 02, 01, 67, 05
     1           , 50, 61, 52, 41, 50, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA AMPERS /11, 60, 06, 07, 18, 48, 46, 02, 01, 10
     1           , 30, 63, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA APOSTR /06, 24, 46, 48, 38, 37, 47, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA LPAREN /04, 40, 22, 26, 48, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA RPAREN /04, 20, 42, 46, 28, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ASTER  /02, 01, 67, 02, 07, 61, 02, 04, 64, 02
     1           , 37, 31, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA PLUS   /02, 14, 54, 02, 36, 32, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA COMMA  /06, 20, 42, 44, 34, 33, 43, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA MINUS  /02, 14, 54, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA PERIOD /05, 20, 30, 31, 21, 20, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SLASH  /02, 01, 67, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ZERO   /09, 10, 50, 61, 67, 58, 18, 07, 01, 10
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ONE    /02, 10, 50, 03, 30, 38, 16, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA TWO    /10, 07, 18, 58, 67, 65, 54, 24, 02, 00
     1           , 60, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA THREE  /07, 07, 18, 58, 67, 65, 54, 34, 06, 54
     1           , 63, 61, 50, 10, 01, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA FOUR   /05, 50, 58, 03, 02, 72, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA FIVE   /10, 01, 10, 40, 62, 63, 45, 05, 08, 68
     1           , 67, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SIX    /11, 04, 54, 63, 61, 50, 10, 01, 06, 28
     1           , 58, 67, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SEVEN  /06, 20, 23, 67, 68, 08, 07, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA EIGHT  /16, 14, 03, 01, 10, 50, 61, 63, 54, 14
     1           , 05, 07, 18, 58, 67, 65, 54, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA NINE   /11, 01, 10, 40, 62, 67, 58, 18, 07, 05
     1           , 14, 64, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA COLON  /05, 22, 32, 33, 23, 22, 05, 26, 36, 37
     1           , 27, 26, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SEMICO /06, 10, 32, 34, 24, 23, 33, 05, 26, 36
     1           , 37, 27, 26, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA LESS   /03, 50, 14, 58, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA EQUALS /02, 12, 52, 02, 16, 56, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA GREATR /03, 10, 54, 18, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA QUESTN /07, 06, 07, 18, 58, 67, 34, 33, 01, 31
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ATSIGN /13, 54, 45, 34, 43, 54, 64, 66, 48, 28
     1           , 06, 02, 20, 50, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA AUPPER /05, 00, 05, 38, 65, 60, 02, 03, 63, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA BUPPER /06, 00, 50, 61, 63, 54, 14, 05, 08, 58
     1           , 67, 65, 54, 02, 18, 10, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA CUPPER /08, 67, 58, 28, 06, 02, 20, 50, 61, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA DUPPER /06, 00, 40, 62, 66, 48, 08, 02, 18, 10
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA EUPPER /04, 60, 00, 08, 68, 02, 34, 04, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA FUPPER /03, 00, 08, 68, 02, 34, 04, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA GUPPER /10, 67, 58, 28, 06, 02, 20, 50, 61, 64
     1           , 44, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA HUPPER /02, 00, 08, 02, 60, 68, 02, 04, 64, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA IUPPER /02, 10, 50, 02, 30, 38, 02, 18, 58, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA JUPPER /05, 01, 10, 20, 31, 38, 02, 18, 58, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA KUPPER /02, 00, 08, 02, 68, 02, 02, 24, 60, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA LUPPER /03, 08, 00, 60, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA MUPPER /05, 00, 08, 35, 68, 60, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA NUPPER /02, 00, 08, 02, 07, 61, 02, 60, 68, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA OUPPER /09, 20, 40, 62, 66, 48, 28, 06, 02, 20
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA PUPPER /07, 00, 08, 58, 67, 66, 55, 05, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA QUPPER /09, 20, 40, 62, 66, 48, 28, 06, 02, 20
     1           , 02, 33, 60, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA RUPPER /07, 00, 08, 58, 67, 66, 55, 05, 02, 15
     1           , 60, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SUPPER /12, 01, 10, 50, 61, 63, 54, 14, 05, 07
     1           , 18, 58, 67, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA TUPPER /02, 30, 38, 02, 08, 68, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA UUPPER /06, 08, 01, 10, 50, 61, 68, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA VUPPER /05, 08, 03, 30, 63, 68, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA WUPPER /05, 08, 00, 33, 60, 68, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA XUPPER /04, 00, 01, 67, 68, 04, 08, 07, 61, 60
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA YUPPER /03, 08, 35, 68, 02, 35, 30, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ZUPPER /06, 08, 68, 67, 01, 00, 60, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA LBRACK /04, 40, 20, 28, 48, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA BKSLSH /02, 07, 61, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA RBRACK /04, 20, 40, 48, 28, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA CARET  /03, 05, 38, 65, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA USCORE /02,-01,-61, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ACCENT /05, 27, 28, 38, 37, 55, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ALOWER /05, 06, 26, 35, 31, 40, 07, 31, 20, 10
     1           , 01, 02, 13, 33, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA BLOWER /02, 08, 00, 08, 02, 20, 30, 41, 44, 35
     1           , 25, 03, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA CLOWER /08, 41, 30, 10, 01, 04, 15, 35, 44, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA DLOWER /02, 48, 40, 08, 42, 20, 10, 01, 04, 15
     1           , 25, 43, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ELOWER /10, 40, 10, 01, 04, 15, 35, 44, 43, 32
     1           , 02, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA FLOWER /04, 10, 17, 28, 37, 02, 04, 24, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA GLOWER /11, 40, 10, 01, 04, 15, 35, 44,-41,-23
     1           ,-13,-02, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA HLOWER /02, 00, 08, 05, 03, 25, 35, 44, 40, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ILOWER /01, 37, 03, 25, 35, 30, 02, 20, 40, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA JLOWER /01, 37, 06, 35,-32,-23,-13,-02,-01, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA KLOWER /02, 08, 00, 02, 01, 45, 03, 40, 22, 23
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA LLOWER /02, 20, 40, 03, 30, 38, 28, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA MLOWER /06, 00, 04, 15, 25, 34, 30, 05, 34, 45
     1           , 55, 64, 60, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA NLOWER /02, 00, 05, 05, 03, 25, 35, 44, 40, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA OLOWER /09, 01, 04, 15, 35, 44, 41, 30, 10, 01
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA PLOWER /02,-03, 05, 08, 03, 25, 35, 44, 41, 30
     1           , 20, 02, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA QLOWER /02,-43, 45, 08, 43, 25, 15, 04, 01, 10
     1           , 20, 42, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA RLOWER /02, 00, 05, 04, 03, 25, 35, 44, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SLOWER /09, 00, 30, 41, 42, 33, 13, 04, 15, 45
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA TLOWER /02, 06, 26, 05, 18, 11, 20, 30, 41, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ULOWER /05, 05, 01, 10, 20, 42, 02, 40, 45, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA VLOWER /05, 05, 02, 20, 42, 45, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA WLOWER /06, 05, 01, 10, 20, 31, 35, 05, 31, 40
     1           , 50, 61, 65, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA XLOWER /02, 00, 55, 02, 05, 50, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA YLOWER /05, 05, 01, 10, 30, 41, 05, 45,-42,-33
     1           ,-23,-12, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ZLOWER /04, 05, 55, 00, 50, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA LBRACE /06, 40, 30, 21, 23, 14, 04, 05, 14, 25
     1           , 27, 38, 48, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA ORSIGN /02, 30, 38, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA RBRACE /06, 20, 30, 41, 43, 54, 64, 05, 54, 45
     1           , 47, 38, 28, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA TILDE  /04, 06, 28, 46, 68, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      DATA SPC127 /00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     1           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
     2           , 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
      END
C*GRDOT0 -- draw a dot
C+
      SUBROUTINE GRDOT0 (X,Y)
C
C GRPCKG (internal routine): Draw a single dot (pixel) at a specified
C location.
C
C Arguments:
C
C X, Y (real, input): absolute device coordinates of the dot (these
C       are rounded to the nearest integer by GRDOT0).
C--
C (1-Jun-1984)
C 22-Oct-1984 - rewrite [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  NBUF, LCHR
      REAL     X, Y, RBUF(6)
      CHARACTER CHR
C
C (X,Y) is the new current position.
C
      GRXPRE(GRCIDE) = X
      GRYPRE(GRCIDE) = Y
C
C Check window.
C
      IF (X .LT. GRXMIN(GRCIDE)) RETURN
      IF (X .GT. GRXMAX(GRCIDE)) RETURN
      IF (Y .LT. GRYMIN(GRCIDE)) RETURN
      IF (Y .GT. GRYMAX(GRCIDE)) RETURN
C
C Begin picture if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C If a "thick pen" is to be simulated, use the line-drawing routines
C instead.
C
      IF (GRWIDT(GRCIDE).GT.1) THEN
          CALL GRLIN3(X,Y,X,Y)
      ELSE
          RBUF(1)=X
          RBUF(2)=Y
          NBUF=2
          CALL GREXEC(GRGTYP,13,RBUF,NBUF,CHR,LCHR)
      END IF
      END
C*GRDOT1 -- draw dots
C+
      SUBROUTINE GRDOT1(POINTS, X, Y)
      INTEGER POINTS
      REAL X(POINTS), Y(POINTS)
C
C GRPCKG (internal routine): Draw a set of dots.
C
C Arguments:
C
C POINTS (input, integer): the number of coordinate pairs.
C X, Y (input, real arrays, dimensioned POINTS or greater): the
C       X and Y world coordinates of the points.
C--
C 14-Mar-1997 - new routine to optimize drawing many dots [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, NBUF, LCHR
      REAL     RBUF(2), XP, YP
      CHARACTER CHR
      EQUIVALENCE (XP, RBUF(1)), (YP, RBUF(2))
C
C Begin picture if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Loop for points: driver support.
C
      IF (GRWIDT(GRCIDE).LE.1) THEN
         NBUF = 2
         LCHR = 0
         DO 10 I=1,POINTS
C        -- Convert to device coordinates
            XP = X(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            YP = Y(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
C           -- Clip against viewport
            IF (XP .GE. GRXMIN(GRCIDE) .AND.
     :          XP .LE. GRXMAX(GRCIDE) .AND.
     :          YP .GE. GRYMIN(GRCIDE) .AND.
     :          YP .LE. GRYMAX(GRCIDE)) THEN
               CALL GREXEC(GRGTYP,13,RBUF,NBUF,CHR,LCHR)
            END IF
 10      CONTINUE
C
C Thick line emulation required.
C
      ELSE
         DO 20 I=1,POINTS
C        -- Convert to device coordinates
            XP = X(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            YP = Y(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
C           -- Clip against viewport
            IF (XP .GE. GRXMIN(GRCIDE) .AND.
     :          XP .LE. GRXMAX(GRCIDE) .AND.
     :          YP .GE. GRYMIN(GRCIDE) .AND.
     :          YP .LE. GRYMAX(GRCIDE)) THEN
               CALL GRLIN3(XP, YP, XP, YP)
            END IF
 20      CONTINUE
      END IF
C
C New pen position.
C
      GRXPRE(GRCIDE) = XP
      GRYPRE(GRCIDE) = YP
C
      END
C*GRDTYP -- decode graphics device type string
C+
      INTEGER FUNCTION GRDTYP (TEXT)
C
C GRPCKG (internal routine): determine graphics device type code from
C type name. It compares the argument with the table of known device
C types in common.
C
C Argument:
C
C TEXT (input, character): device type name, eg 'PRINTRONIX'; the name
C       may be abbreviated to uniqueness.
C
C Returns:
C
C GRDTYP (integer): the device type code, in the range 1 to
C       GRTMAX, zero if the type name is not recognised, or -1
C       if the type name is ambiguous.
C--
C 27-Dec-1984 - rewrite so that is doesn't have to be modified for
C               new devices [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 10-Nov-1995 - ignore drivers that report no device type [TJP].
C 30-Aug-1996 - check for an exact match; indicate if type is
C               ambiguous [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) TEXT
      INTEGER  CODE, I, L, MATCH
      REAL     RBUF(6)
      INTEGER NDEV,NBUF,LCHR
      INTEGER GRTRIM
      CHARACTER*32 CHR
C
      GRDTYP = 0
      L = GRTRIM(TEXT)
      IF (L.LT.1) RETURN
      MATCH = 0
      CODE = 0
      CALL GREXEC(0,0,RBUF,NBUF,CHR,LCHR)
      NDEV=NINT(RBUF(1))
      DO 30 I=1,NDEV
         CALL GREXEC(I, 1,RBUF,NBUF,CHR,LCHR)
         IF (LCHR.GT.0) THEN
            IF(TEXT(1:L).EQ.CHR(1:L)) THEN
               IF (CHR(L+1:L+1).EQ.' ') THEN
C                 -- exact match
                  GRDTYP = I
                  GRGTYP = GRDTYP
                  RETURN
               ELSE
                  MATCH = MATCH+1
                  CODE = I
               END IF
            END IF
         END IF
   30 CONTINUE
      IF (MATCH.EQ.0) THEN
C        -- no match
         GRDTYP = 0
      ELSE IF (MATCH.EQ.1) THEN
         GRDTYP = CODE
         GRGTYP = GRDTYP
      ELSE
         GRDTYP = -1
      END IF
C
      END
C*GREPIC -- end picture
C+
      SUBROUTINE GREPIC
C
C GRPCKG: End the current picture.
C
C Arguments: none.
C--
C 17-Nov-1994 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER CHR
C
C Check a plot is open.
C
      IF (GRCIDE.LT.1) RETURN
C
C End picture.
C
      IF (GRPLTD(GRCIDE)) THEN
            RBUF(1) = 1.
            NBUF = 1
            CALL GREXEC(GRGTYP,14,RBUF,NBUF,CHR,LCHR)
      END IF
      GRPLTD(GRCIDE) = .FALSE.
C
      END
C*GRESC -- escape routine
C+
      SUBROUTINE GRESC (TEXT)
C
C GRPCKG: "Escape" routine. The specified text is sent directly to the
C selected graphics device, with no interpretation by GRPCKG. This
C routine must be used with care; e.g., the programmer needs to know
C the device type of the currently selected device, and the instructions
C that that device can accept.
C
C Arguments: none.
C  TEXT (input, character*(*)):  text to be sent to the device.
C
C 15-May-1985 - new routine [TJP].
C 26-May-1987 - add GREXEC support [TJP].
C 19-Dec-1988 - start new page if necessary [TJP].
C  4-Feb-1997 - RBUF should be an array, not a scalar [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) TEXT
      REAL RBUF(1)
      INTEGER NBUF
C
C If no device is currently selected, do nothing.
C
      IF (GRCIDE.GT.0) THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          NBUF = 0
          CALL GREXEC(GRGTYP,23,RBUF,NBUF,TEXT,LEN(TEXT))
      END IF
      END

C*GRETXT -- erase text from graphics screen
C+
      SUBROUTINE GRETXT
C
C GRPCKG: Erase the text screen.  Some graphics devices have
C two superimposed view surfaces, of which one is used for graphics and
C the other for alphanumeric text.  This routine erases the text
C view surface without affecting the graphics view surface. It does
C nothing if there is no text view surface associated with the device.
C
C Arguments: none.
C--
C (1-Feb-1983)
C 16-Oct-1984 - add ID100 device [RSS/TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*1   CHR
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
C
      IF (GRCIDE.GE.1) THEN
          CALL GREXEC(GRGTYP,18,RBUF,NBUF,CHR,LCHR)
      END IF
C
      END
C*GRFA -- fill area (polygon)
C+
      SUBROUTINE GRFA (N,PX,PY)
      INTEGER N
      REAL PX(*), PY(*)
C
C GRPCKG: FILL AREA: fill a polygon with solid color.  The polygon
C is defined by the (x,y) world coordinates of its N vertices.  If
C this is not a function supported by the device, shading is
C accomplished by drawing horizontal lines spaced by 1 pixel.  By
C selecting color index 0, the interior of the polygon can be erased
C on devices which permit it.  The polygon need not be convex, but if
C it is re-entrant (i.e., edges intersect other than at the vertices),
C it may not be obvious which regions are "inside" the polygon.  The
C following rule is applied: for a given point, create a straight line
C starting at the point and going to infinity. If the number of
C intersections between the straight line and the polygon is odd, the
C point is within the polygon; otherwise it is outside. If the
C straight line passes a polygon vertex tangentially, the
C intersection  count is not affected. The only attribute which applies
C to FILL AREA is color index: line-width and line-style are ignored.
C There is a limitation on the complexity of the polygon: GFA will
C fail if any horizontal line intersects more than 32 edges of the
C polygon.
C
C Arguments:
C
C N (input, integer): the number of vertices of the polygon (at least
C       3).
C PX, PY (input, real arrays, dimension at least N): world coordinates
C       of the N vertices of the polygon.
C--
C 16-Jul-1984 - [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C  7-Sep-1994 - avoid driver call for capabilities [TJP].
C  1-May-1995 - fixed bug for re-entrant polygons, and optimized code
C               [A.F.Carman].
C 18-Oct-1995 - fixed bug: emulated fill failed for reversed y-axis
C               [S.C.Allendorf/TJP].
C  4-Dec-1995 - remove use of real variable as do-loop variable [TJP].
C 20-Mar-1996 - use another do loop 40 to avoid gaps between adjacent
C               polygons [RS]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER MAXSEC
      PARAMETER (MAXSEC=32)
      INTEGER I, J, NSECT, LW, LS, NBUF, LCHR, LINE
      REAL    RBUF(6)
      CHARACTER*32 CHR
      REAL    X(MAXSEC), Y, YMIN, YMAX, DY, YD, TEMP, S1, S2, T1, T2
      LOGICAL FORWD
C
      IF (GRCIDE.LT.1) RETURN
      IF (N.LT.3) THEN
          CALL GRWARN('GRFA - polygon has < 3 vertices.')
          RETURN
      END IF
C
C Devices with polygon fill capability.
C
      IF(GRGCAP(GRCIDE)(4:4).EQ.'A') THEN
         IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
         RBUF(1) = N
         CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
         DO 10 I=1,N
            RBUF(1) = PX(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            RBUF(2) = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
            CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
 10      CONTINUE
         RETURN
      END IF
C
C For other devices fill area is simulated.
C
C Save attributes.
C
      CALL GRQLS(LS)
      CALL GRQLW(LW)
      CALL GRSLS(1)
      CALL GRSLW(1)
C
C Find range of raster-lines to be shaded.
C
      YMIN = PY(1)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      YMAX = YMIN
      DO 20 I=2,N
         YD = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
         YMIN = MIN(YMIN,YD)
         YMAX = MAX(YMAX,YD)
 20   CONTINUE
      CALL GREXEC(GRGTYP, 3,RBUF,NBUF,CHR,LCHR)
      DY = ABS(RBUF(3))
C
C Find intersections of edges with current raster line.
C
      FORWD = .TRUE.
      S1 = PX(N)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
      T1 = PY(N)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
C
      DO 40 LINE = NINT(YMIN/DY),NINT(YMAX/DY)
         Y = LINE * DY
         NSECT = 0
         DO 30 I=1,N
            S2 = PX(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            T2 = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
            IF ((T1.LT.Y .AND. Y.LE.T2).OR.
     :          (T1.GE.Y .AND. Y.GT.T2)) THEN
               NSECT = NSECT+1
               IF (NSECT.GT.MAXSEC) THEN
                  CALL GRWARN('GRFA - polygon is too complex.')
                  RETURN
               END IF
               X(NSECT)=(S1+(S2-S1)*((Y-T1)/(T2-T1)))
            END IF
            S1 = S2
            T1 = T2
 30      CONTINUE
C
C Sort the intersections into increasing x order.
C
         DO 34 I=2,NSECT
            DO 32 J=1,I
               IF (X(J).GT.X(I)) THEN
                  TEMP = X(J)
                  X(J) = X(I)
                  X(I) = TEMP
               END IF
 32         CONTINUE
 34      CONTINUE
C
C Draw the horizontal line-segments.
C
         GRYPRE(GRCIDE) = Y
         IF (FORWD) THEN
            DO 36 I=1,NSECT-1,2
               GRXPRE(GRCIDE) = X(I)
               CALL GRLIN0(X(I+1),Y)
 36         CONTINUE
            FORWD = .FALSE.
         ELSE
            DO 38 I=NSECT,2,-2
               GRXPRE(GRCIDE) = X(I)
               CALL GRLIN0(X(I-1),Y)
 38         CONTINUE
            FORWD = .TRUE.
         END IF
 40   CONTINUE
C
C Restore attributes.
C
      CALL GRSLS(LS)
      CALL GRSLW(LW)
      END
C*GRFAO - format character string containing integers
C+
      SUBROUTINE GRFAO (FORMAT, L, STR, V1, V2, V3, V4)
      CHARACTER*(*) FORMAT
      INTEGER L
      CHARACTER*(*) STR
      INTEGER V1, V2, V3, V4
C
C The input string FORMAT is copied to the output string STR with
C the first occurrence of '#' replaced by the value of V1, the second
C by the value of V2, etc.  The length of the resulting string is 
C returned in L.
C-----------------------------------------------------------------------
      INTEGER I,Q,VAL,GRITOC
C
      L = 0
      Q = 0
      DO 10 I=1,LEN(FORMAT)
          IF (L.GE.LEN(STR)) RETURN
          IF (FORMAT(I:I).NE.'#') THEN
              L = L+1
              STR(L:L) = FORMAT(I:I)
          ELSE
              Q = Q+1
              VAL = 0
              IF (Q.EQ.1) VAL = V1
              IF (Q.EQ.2) VAL = V2
              IF (Q.EQ.3) VAL = V3
              IF (Q.EQ.4) VAL = V4
              L = L + GRITOC(VAL, STR(L+1:))
          END IF
   10 CONTINUE
C-----------------------------------------------------------------------
      END
C*GRGRAY -- gray-scale map of a 2D data array
C+
      SUBROUTINE GRGRAY (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   FG, BG, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM)
      REAL    FG, BG
      REAL    PA(6)
C
C This is a device-dependent support routine for PGGRAY.
C
C Draw gray-scale map of an array in current window. Array
C values between FG and BG are shaded in gray levels determined
C by linear interpolation. FG may be either less than or greater
C than BG.  Array values outside the range FG to BG are
C shaded black or white as appropriate.
C
C GRGRAY uses GRIMG0 on devices with enough color indices available.
C Note that it changes the color table to gray-scale.
C Otherwise in does a random dither with GRIMG3.
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  FG     (input)  : the array value which is to appear in
C                    foreground color.
C  BG     (input)  : the array value which is to appear in
C                    background color.
C  PA     (input)  : transformation matrix between array grid and
C                    device coordinates (see GRCONT).
C  MODE   (input)  : transfer function.
C--
C 12-Dec-1986 - Speed up plotting [J. Biretta].
C  3-Apr-1987 - Add special code for /PS, /VPS, /GR.
C  2-Sep-1987 - Adapted from PGGRAY [TJP].
C  1-Dec-1988 - Put random-number generator inline [TJP].
C  3-Apr-1989 - Use "line of pixels" primitive where available [TJP].
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C 19-Jan-1990 - Add special code for /CPS, /VCPS [DLM]
C  3-Sep-1992 - Add special code for NULL device [TJP].
C 25-Nov-1992 - Add special code for /NEXT [AFT].
C 17-Mar-1994 - Scale in device coordinates [TJP].
C 31-Aug-1994 - use GRIMG0 when appropriate [TJP].
C  7-Sep-1994 - speed up random dither [TJP].
C  8-Feb-1995 - use color ramp based on color indices 0 and 1 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I
      REAL    A0, A1, CR0, CG0, CB0, CR1, CG1, CB1
      INTRINSIC REAL
C-----------------------------------------------------------------------
C
C N.B. Arguments are assumed to be valid (checked by PGGRAY).
C
C Use GRIMG0 if this an appropriate device; first initialize the
C color table to a linear ramp between the colors assigned to color
C indices 0 and 1.
C
      IF (GRGCAP(GRCIDE)(7:7).NE.'N' .AND. MAXIND-MININD .GT. 15) THEN
         CALL GRQCR(0, CR0, CG0, CB0)
         CALL GRQCR(1, CR1, CG1, CB1)
         DO 5 I=MININD,MAXIND
            A0 = REAL(I-MININD)/REAL(MAXIND-MININD)
            A1 = 1.0 - A0
            CALL GRSCR(I, A0*CR0+A1*CR1, A0*CG0+A1*CG1, A0*CB0+A1*CB1)
 5       CONTINUE
         CALL GRIMG0(A, IDIM, JDIM, I1, I2, J1, J2,
     :               FG, BG, PA, MININD, MAXIND, MODE)
         RETURN
C
C Otherwise use random dither in current color index.
C
      ELSE
         CALL GRIMG3(A, IDIM, JDIM, I1, I2, J1, J2,
     :               FG, BG, PA, MODE)
      END IF
C-----------------------------------------------------------------------
      END

C*GRGTC0 -- obtain character digitization
C+
      SUBROUTINE GRGTC0 (CHAR,CENTER,POINTS,X,Y,MORE)
C
C GRPCKG (internal routine): obtain character digitization.
C
C (10-Feb-1983)
C-----------------------------------------------------------------------
      EXTERNAL GRDAT2
      LOGICAL CENTER
      INTEGER POINTS, CHAR
      REAL X(1)
      REAL Y(1)
      LOGICAL MORE
C
      INTEGER CINDX1, CINDX2
      INTEGER CTD1, CTD2
      PARAMETER (CTD1 = 30, CTD2 = 128)
      INTEGER CHTBL(CTD1, CTD2)
      COMMON /GRCS02/ CINDX1, CINDX2, CHTBL
C
      INTEGER I
      INTEGER COORDS
      LOGICAL TAILED
C-----------------------------------------------------------------------
      IF (CINDX2.LE.0) CINDX2 = CHAR + 1
C
C Get the next segment of the character.
C
      POINTS = CHTBL(CINDX1, CINDX2)
      IF(POINTS .EQ. 0) GO TO 240
      DO 220 I = 1, POINTS
          CINDX1 = CINDX1 + 1
          COORDS = CHTBL(CINDX1, CINDX2)
          TAILED = COORDS .LT. 0
          IF(TAILED) COORDS = IABS(COORDS)
          X(I) = FLOAT(COORDS / 10)
          Y(I) = FLOAT(MOD(COORDS, 10))
          IF(TAILED) Y(I) = - Y(I)
          IF(.NOT. CENTER) GO TO 220
          X(I) = X(I) - 3.0
          Y(I) = Y(I) - 4.0
  220     CONTINUE
  240 CONTINUE
C
C Set status and return.
C
      IF(CINDX1 .EQ. CTD1) GO TO 320
      CINDX1 = CINDX1 + 1
      IF(CHTBL(CINDX1, CINDX2) .EQ. 0) GO TO 320
      MORE = .TRUE.
      RETURN
  320 MORE = .FALSE.
      CINDX1 = 1
      CINDX2 = 0
      RETURN
      END
C*GRIMG0 -- color image of a 2D data array
C+
      SUBROUTINE GRIMG0 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM), A1, A2, PA(6)
C
C This is a support routine for PGIMAG.
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  A1     (input)  : the array value which is to appear in color
C                    index MININD.
C  A2     (input)  : the array value which is to appear in color
C                    index MAXIND.
C  PA     (input)  : transformation matrix between array grid and
C                    device coordinates.
C  MININD (input)  : minimum color index to use.
C  MAXIND (input)  : maximum color index to use.
C  MODE   (input)  : =0 for linear, =1 for logarithmic, =2 for
C                    square-root mapping of array values to color
C                    indices.
C--
C  7-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER C
C-----------------------------------------------------------------------
C
C Switch on type of device support.
C
      C = GRGCAP(GRCIDE)(7:7)
      IF (C.EQ.'Q') THEN
C         -- Image-primitive devices
          CALL GRIMG1(A, IDIM, JDIM, I1, I2, J1, J2, A1, A2, PA,
     :                MININD, MAXIND, MODE)
      ELSE IF (C.EQ.'P') THEN
C         -- Pixel-primitive devices         
          CALL GRIMG2(A, IDIM, JDIM, I1, I2, J1, J2, A1, A2, PA,
     :                MININD, MAXIND, MODE)
      ELSE IF (C.EQ.'N') THEN
C         -- Other devices
          CALL GRWARN(
     :     'images cannot be displayed on the selected device')
      ELSE
C         -- Unknown device code
          CALL GRWARN('unexpected error in routine GRIMG0')
      END IF
C-----------------------------------------------------------------------
      END
C*GRIMG1 -- image of a 2D data array (image-primitive devices)
C+
      SUBROUTINE GRIMG1 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM), A1, A2, PA(6)
C
C (This routine is called by GRIMG0.)
C--
C 7-Sep-1994  New routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER NBUF, LCHR
      REAL    RBUF(21), FAC, AV, SFAC, SFACL
      CHARACTER*1 CHR
      INTEGER  I, J, II, NXP, NYP, IV
      INTRINSIC NINT, LOG
      PARAMETER (SFAC=65000.0)
C-----------------------------------------------------------------------
C Size of image.
C
      NXP = I2 - I1 + 1
      NYP = J2 - J1 + 1
      RBUF(1) = 0.0
      RBUF(2) = NXP
      RBUF(3) = NYP
C
C Clipping rectangle.
C
      RBUF(4) = GRXMIN(GRCIDE)
      RBUF(5) = GRXMAX(GRCIDE)
      RBUF(6) = GRYMIN(GRCIDE)
      RBUF(7) = GRYMAX(GRCIDE)
C
C Image transformation matrix.
C
      FAC = PA(2)*PA(6) - PA(3)*PA(5)
      RBUF(8)  =  PA(6)/FAC
      RBUF(9)  = (-PA(5))/FAC
      RBUF(10) = (-PA(3))/FAC
      RBUF(11) =  PA(2)/FAC
      RBUF(12) = (PA(3)*PA(4) - PA(1)*PA(6))/FAC - (I1-0.5)
      RBUF(13) = (PA(5)*PA(1) - PA(4)*PA(2))/FAC - (J1-0.5)
C
C Send setup info to driver.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
      CALL GRTERM
      NBUF = 13
      LCHR = 0
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C
C Convert image array to color indices and send to driver.
C
      SFACL = LOG(1.0+SFAC)
      II = 0
      DO 20 J = J1,J2
          DO 10 I = I1,I2
              AV = A(I,J)
              IF (A2.GT.A1) THEN
                  AV = MIN(A2, MAX(A1,AV))
              ELSE
                  AV = MIN(A1, MAX(A2,AV))
              END IF
              IF (MODE.EQ.0) THEN
                IV = NINT((MININD*(A2-AV) + MAXIND*(AV-A1))/(A2-A1))
              ELSE IF (MODE.EQ.1) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :               LOG(1.0+SFAC*ABS((AV-A1)/(A2-A1)))/SFACL)
              ELSE IF (MODE.EQ.2) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :                             SQRT(ABS((AV-A1)/(A2-A1))))
              ELSE
                IV = MININD
              END IF
              II = II + 1
              RBUF(II+1) = IV
              IF (II.EQ.20) THEN
                  NBUF = II + 1
                  RBUF(1) = II
                  CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
                  II = 0
              END IF
   10     CONTINUE
   20 CONTINUE
      IF (II.GT.0) THEN
          NBUF = II + 1
          RBUF(1) = II
          CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
          II = 0
      END IF
C
C Send termination code to driver.
C
      NBUF = 1
      RBUF(1) = -1
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C-----------------------------------------------------------------------
      END
C*GRIMG2 -- image of a 2D data array (pixel-primitive devices)
C+
      SUBROUTINE GRIMG2 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM)
      REAL    A1, A2
      REAL    PA(6)
C
C (This routine is called by GRIMG0.)
C--
C 7-Sep-1994  New routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IV,IX,IX1,IX2,IY,IY1,IY2,J, NPIX, LCHR
      REAL     DEN, AV, SFAC, SFACL
      REAL     XXAA,XXBB,YYAA,YYBB,XYAA,XYBB,YXAA,YXBB,XYAAIY,YXAAIY
      REAL     BUFFER(1026)
      CHARACTER*1 CHR
      INTRINSIC NINT, LOG
      PARAMETER (SFAC=65000.0)
C-----------------------------------------------------------------------
C
C Location of current window in device coordinates.
C
      IX1 = NINT(GRXMIN(GRCIDE))+1
      IX2 = NINT(GRXMAX(GRCIDE))-1
      IY1 = NINT(GRYMIN(GRCIDE))+1
      IY2 = NINT(GRYMAX(GRCIDE))-1
C
C Transformation from array coordinates to device coordinates.
C
      DEN = PA(2)*PA(6)-PA(3)*PA(5)
      XXAA = (-PA(6))*PA(1)/DEN
      XXBB = PA(6)/DEN
      XYAA = (-PA(3))*PA(4)/DEN
      XYBB = PA(3)/DEN
      YYAA = (-PA(2))*PA(4)/DEN
      YYBB = PA(2)/DEN
      YXAA = (-PA(5))*PA(1)/DEN
      YXBB = PA(5)/DEN
C
C Start a new page if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Run through every device pixel (IX, IY) in the current window and
C determine which array pixel (I,J) it falls in.
C
      SFACL = LOG(1.0+SFAC)
      DO 120 IY=IY1,IY2
          XYAAIY = XXAA-XYAA-XYBB*IY
          YXAAIY = YYAA+YYBB*IY-YXAA
          NPIX = 0
          BUFFER(2) = IY
          DO 110 IX=IX1,IX2
            I = NINT(XYAAIY+XXBB*IX)
            IF (I.LT.I1.OR.I.GT.I2) GOTO 110
            J = NINT(YXAAIY-YXBB*IX)
            IF (J.LT.J1.OR.J.GT.J2) GOTO 110
C
C           -- determine color index IV of this pixel
C
            AV = A(I,J)
            IF (A2.GT.A1) THEN
                AV = MIN(A2, MAX(A1,AV))
            ELSE
                AV = MIN(A1, MAX(A2,AV))
            END IF
            IF (MODE.EQ.0) THEN
                IV = NINT((MININD*(A2-AV) + MAXIND*(AV-A1))/(A2-A1))
            ELSE IF (MODE.EQ.1) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :               LOG(1.0+SFAC*ABS((AV-A1)/(A2-A1)))/SFACL)
            ELSE IF (MODE.EQ.2) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :                             SQRT(ABS((AV-A1)/(A2-A1))))
            ELSE
                IV = MININD
            END IF
C
            IF (NPIX.LE.1024) THEN
C               -- drop pixels if buffer too small (to be fixed!)
                NPIX = NPIX+1
                IF (NPIX.EQ.1) BUFFER(1) = IX
                BUFFER(NPIX+2) = IV
            END IF
  110     CONTINUE
          IF (NPIX.GT.0) CALL 
     :                   GREXEC(GRGTYP, 26, BUFFER, NPIX+2, CHR, LCHR)
  120 CONTINUE
C-----------------------------------------------------------------------
      END
C*GRIMG3 -- gray-scale map of a 2D data array, using dither
C+
      SUBROUTINE GRIMG3 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   BLACK, WHITE, PA, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MODE
      REAL    A(IDIM,JDIM)
      REAL    BLACK, WHITE
      REAL    PA(6)
C--
C 2-Sep-1994 - moved from GRGRAY [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IX,IX1,IX2,IY,IY1,IY2,J
      REAL     DEN,VALUE,BW
      REAL     XXAA,XXBB,YYAA,YYBB,XYAA,XYBB,YXAA,YXBB,XYAAIY,YXAAIY
      INTEGER  M, IAA, ICC, JRAN, ILAST, JLAST, IXSTEP, IYSTEP
      REAL     RAND, RM, FAC, FACL
      PARAMETER (M=714025, IAA=1366, ICC=150889, RM=1.0/M)
      PARAMETER (FAC=65000.0)
      INTRINSIC MOD, NINT, REAL, LOG
C-----------------------------------------------------------------------
C
      IF (MODE.LT.0 .OR. MODE.GT.2) RETURN
C
C Initialize random-number generator (based on RAN2 of Press et al.,
C Numerical Recipes)
C
      JRAN = 76773
C
      IX1 = NINT(GRXMIN(GRCIDE))+1
      IX2 = NINT(GRXMAX(GRCIDE))-1
      IY1 = NINT(GRYMIN(GRCIDE))+1
      IY2 = NINT(GRYMAX(GRCIDE))-1
      DEN = PA(2)*PA(6)-PA(3)*PA(5)
C
C Calculate constants.
C
      BW   = ABS(BLACK-WHITE)
      FACL = LOG(1.0+FAC)
      XXAA = (-PA(6))*PA(1)/DEN
      XXBB = PA(6)/DEN
      XYAA = (-PA(3))*PA(4)/DEN
      XYBB = PA(3)/DEN
      YYAA = (-PA(2))*PA(4)/DEN
      YYBB = PA(2)/DEN
      YXAA = (-PA(5))*PA(1)/DEN
      YXBB = PA(5)/DEN
C
C Choose step size: at least 1/200 inch, assuming the line-width
C unit is 1/200 inch.
C
      IXSTEP = MAX(1,NINT(GRWIDT(GRCIDE)*GRPXPI(GRCIDE)/200.0))
      IYSTEP = MAX(1,NINT(GRWIDT(GRCIDE)*GRPYPI(GRCIDE)/200.0))
C
C Draw dots.
C
      ILAST = 0
      JLAST = 0
      DO 120 IY=IY1,IY2,IYSTEP
          XYAAIY = XXAA-XYAA-XYBB*IY
          YXAAIY = YYAA+YYBB*IY-YXAA
          DO 110 IX=IX1,IX2,IXSTEP
              I = NINT(XYAAIY+XXBB*IX)
              IF (I.LT.I1.OR.I.GT.I2) GOTO 110
              J = NINT(YXAAIY-YXBB*IX)
              IF (J.LT.J1.OR.J.GT.J2) GOTO 110
              IF (I.NE.ILAST .OR. J.NE.JLAST) THEN
                  ILAST = I
                  JLAST = J
                  VALUE = ABS(A(I,J)-WHITE)/BW
                  IF (MODE.EQ.0) THEN
C                     -- "linear"
                      CONTINUE
                  ELSE IF (MODE.EQ.1) THEN
C                     -- "logarithmic"
                      VALUE = LOG(1.0+FAC*VALUE)/FACL
                  ELSE IF (MODE.EQ.2) THEN
C                     -- "square root"
                      VALUE = SQRT(VALUE)
                  END IF
              END IF
              JRAN = MOD(JRAN*IAA+ICC, M)
              RAND = JRAN*RM
              IF (VALUE.GT.RAND) CALL GRDOT0(REAL(IX),REAL(IY))
  110     CONTINUE
  120  CONTINUE
C-----------------------------------------------------------------------
       END
C*GRINIT -- initialize GRPCKG
C+
      SUBROUTINE GRINIT
C
C Initialize GRPCKG and read font file. Called by GROPEN, but may be 
C called explicitly if needed.
C--
C 29-Apr-1996 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   I
      LOGICAL   INIT
      SAVE      INIT
      DATA      INIT / .TRUE. /
C
      IF (INIT) THEN
         DO 10 I=1,GRIMAX
            GRSTAT(I) = 0
 10      CONTINUE
         CALL GRSY00
         INIT = .FALSE.
      END IF
      RETURN
      END
C*GRINQFONT -- inquire current font [obsolete]
C
      SUBROUTINE GRINQFONT (IF)
      INTEGER IF
      CALL GRQFNT(IF)
      END

C*GRINQLI -- *obsolete routine*
C+
      SUBROUTINE GRINQLI (INTEN)
C
C GRPCKG: obtain the line intensity of the current graphics device.
C Obsolete routine.
C Argument:
C
C INTEN (integer, output): always returns 1.
C--
C (1-Feb-1983; revised 16-Aug-1987).
C-----------------------------------------------------------------------
      INTEGER  INTEN
C
      INTEN = 1
      END

C*GRINQPEN -- *obsolete routine*
C+
      SUBROUTINE GRINQPEN (IP)
C
C GRPCKG: obtain the pen number of the current graphics device.
C Obsolete routine.
C Argument:
C
C IP (integer, output): always receives 1.
C--
C 16-Aug-1987 - [TJP].
C-----------------------------------------------------------------------
      INTEGER  IP
C
      IP = 1
      END
C*GRITOC - convert integer to character string
C+
      INTEGER FUNCTION GRITOC(INT, STR)
      INTEGER INT
      CHARACTER*(*) STR
C
C Convert integer INT into (decimal) character string in STR.
C-----------------------------------------------------------------------
      CHARACTER*10 DIGITS
      INTEGER D, I, INTVAL, J, L
      CHARACTER K
      DATA DIGITS /'0123456789'/
C
      INTVAL = ABS(INT)
      I = 0
C
C Generate digits in reverse order.
C
  10  CONTINUE
          I = I+1
          D = 1 + MOD(INTVAL, 10)
          STR(I:I) = DIGITS(D:D)
          INTVAL = INTVAL/10
          IF (I.LT.LEN(STR) .AND. INTVAL.NE.0) GOTO 10
C
C Add minus sign if necessary.
C
      IF (INT.LT.0 .AND. I.LT.LEN(STR)) THEN
          I = I+1
          STR(I:I) = '-'
      END IF
      GRITOC = I
C
C Reverse string in place.
C
      L = I/2
      DO 20 J=1,L
          K = STR(I:I)
          STR(I:I) = STR(J:J)
          STR(J:J) = K
          I = I-1
   20 CONTINUE
C-----------------------------------------------------------------------
      END
C*GRLDEV -- list supported device types
C+
      SUBROUTINE GRLDEV
C
C Support routine for PGLDEV.
C
C Arguments: none
C--
C  5-Aug-1986 [AFT]
C 13-Dec-1990 Change warnings to messages [TJP].
C 18-Jan-1993 Display one per line [TJP].
C 13-Jan-1995 Change message [TJP].
C 10-Nov-1995 Ignore device types of zero length [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I,NDEV,NBUF,LCHR
      REAL    RBUF(6)
      CHARACTER*72 CHR
      CHARACTER*72 TEXT
C---
      CALL GRMSG('Device types available:')
C--- First obtain number of devices.
      CALL GREXEC(0,0,RBUF,NBUF,CHR,LCHR)
      NDEV=NINT(RBUF(1))
C
      DO 10 I=1,NDEV
         CALL GREXEC(I, 1,RBUF,NBUF,CHR,LCHR)
         IF (LCHR.GT.0) THEN
            TEXT(1:1) = '/'
            TEXT(2:) = CHR(:LCHR)
            CALL GRMSG(TEXT)
         END IF
 10   CONTINUE
C
      END
C*GRLEN -- inquire plotted length of character string
C+
      SUBROUTINE GRLEN (STRING, D)
C
C GRPCKG: length of text string (absolute units)
C--
C (3-Mar-1983)
C 19-Jan-1988 - remove unused label [TJP].
C  9-Sep-1989 - standardize [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL UNUSED
      INTEGER XYGRID(300)
      INTEGER LIST(256)
      CHARACTER*(*) STRING
      REAL FACTOR, COSA, SINA, DX, D, RATIO, FNTBAS, FNTFAC
      INTEGER I, IFNTLV, LX, NLIST
      INTRINSIC ABS, LEN
C
      D = 0.0
      IF (LEN(STRING).LE.0) RETURN
C-----------------------------------------------------------------------
C               Compute scaling and orientation
C-----------------------------------------------------------------------
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      COSA = FACTOR
      SINA = 0.0
      FNTBAS = 0.0
      FNTFAC = 1.0
      IFNTLV = 0
C
C               Convert string to symbol numbers:
C               \u and \d escape sequences are converted to -1,-2
C
      CALL GRSYDS(LIST,NLIST,STRING,GRCFNT(GRCIDE))
C
C               Plot the string of characters
C
      DO 380 I = 1,NLIST
          IF (LIST(I).LT.0) THEN
              IF (LIST(I).EQ.-1) THEN
                  IFNTLV = IFNTLV+1
                  FNTBAS = FNTBAS + 16.0*FNTFAC
                  FNTFAC = 0.6**ABS(IFNTLV)
              ELSE IF (LIST(I).EQ.-2) THEN
                  IFNTLV = IFNTLV-1
                  FNTFAC = 0.6**ABS(IFNTLV)
                  FNTBAS = FNTBAS - 16.0*FNTFAC
              END IF
              GOTO 380
          END IF
          CALL GRSYXD(LIST(I),XYGRID,UNUSED)
          LX = XYGRID(5)-XYGRID(4)
          DX = COSA*LX*RATIO
          D = D + DX*FNTFAC
  380 CONTINUE
C
      END
C*GRLIN0 -- draw a line
C+
      SUBROUTINE GRLIN0 (XP,YP)
C
C GRPCKG (internal routine): draw a line from the current position to a
C specified position, which becomes the new current position. This
C routine takes care of clipping at the viewport boundary, dashed and
C thick lines.
C
C Arguments:
C
C XP, YP (input, real): absolute device coordinates of the end-point of
C       the line.
C--
C 13-Jul-1984
C  7-May-1985 - add MIN/MAX kluge to prevent integer overflow [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL  VIS
      REAL     XP,YP, X0,Y0, X1,Y1
C
C End-points of line are (X0,Y0), (X1,Y1).
C
      X0 = GRXPRE(GRCIDE)
      Y0 = GRYPRE(GRCIDE)
      X1 = MIN(2E9,MAX(-2E9,XP))
      Y1 = MIN(2E9,MAX(-2E9,YP))
      GRXPRE(GRCIDE) = X1
      GRYPRE(GRCIDE) = Y1
C
C Change the end-points of the line (X0,Y0) - (X1,Y1)
C to clip the line at the window boundary.
C
      CALL GRCLPL(X0,Y0,X1,Y1,VIS)
      IF (.NOT.VIS) RETURN
C
C Draw the line in the appropriate style.
C
      IF (GRDASH(GRCIDE)) THEN
C         ! dashed line
         CALL GRLIN1(X0,Y0,X1,Y1,.FALSE.)
      ELSE IF (GRWIDT(GRCIDE).GT.1) THEN
C         ! heavy line
         CALL GRLIN3(X0,Y0,X1,Y1)
      ELSE
C         ! full line
         CALL GRLIN2(X0,Y0,X1,Y1)
      END IF
      END
C*GRLIN1 -- draw a dashed line
C+
      SUBROUTINE GRLIN1 (X0,Y0,X1,Y1,RESET)
C
C GRPCKG : dashed line. Generate a visible dashed line between points
C (X0,Y0) and (X1,Y1) according to the dash pattern stored in common.
C If RESET = .TRUE., the pattern will start from the beginning.
C Otherwise, it will continue from its last position.
C     DASHED LINE PATTERN ARRAY CONTAINING LENGTHS OF
C          MARKS AND SPACES IN UNIT CUBE: GRPATN(*)
C     OFFSET IN CURRENT PATTERN SEGMENT: GRPOFF
C     CURRENT PATTERN SEGMENT NUMBER: GRIPAT
C     NUMBER OF PATTERN SEGMENTS: 8
C--
C (1-Feb-1983)
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      REAL ADJUST, ARG1, ARG2, ALFARG
      REAL SCALE, SEGLEN, X1, X0, Y1, Y0, DS, DSOLD
      REAL ALPHA1, ALPHA2, XP, YP, XQ, YQ
      LOGICAL RESET
      INTEGER THICK
      INTRINSIC ABS, MIN, MOD, REAL, SQRT
C
      ADJUST(ARG1,ARG2,ALFARG) = ALFARG*(ARG2 - ARG1) + ARG1
C
      THICK = GRWIDT(GRCIDE)
      SCALE = SQRT(REAL(ABS(THICK)))
      IF (RESET) THEN
          GRPOFF(GRCIDE) = 0.0
          GRIPAT(GRCIDE) = 1
      END IF
      SEGLEN = SQRT((X1-X0)**2 + (Y1-Y0)**2)
      IF (SEGLEN .EQ. 0.0) RETURN
      DS = 0.0
C
C       Repeat until (ALPHA2 .GE. 1.0)
C
C       Line segments matching the pattern segments are determined
C       by finding values (ALPHA1,ALPHA2) defining the start and end
C       of the segment in the parametric equation (1-ALPHA)*P1 + ALPHA*P2
C       defining the line.  DS measures the progress along the line
C       segment and defines the starting ALPHA1.  The ending ALPHA2
C       is computed from the end of the current pattern mark or space
C       or the segment end, whichever comes first.
C
   10 DSOLD = DS
      ALPHA1 = DS/SEGLEN
      ALPHA2 = MIN(1.0,(DS+SCALE*GRPATN(GRCIDE,GRIPAT(GRCIDE))-
     1           GRPOFF(GRCIDE))/SEGLEN)
      IF (MOD(GRIPAT(GRCIDE),2) .NE. 0) THEN
          XP = ADJUST(X0,X1,ALPHA1)
          YP = ADJUST(Y0,Y1,ALPHA1)
          XQ = ADJUST(X0,X1,ALPHA2)
          YQ = ADJUST(Y0,Y1,ALPHA2)
          IF (THICK.GT.1) THEN
              CALL GRLIN3(XP,YP,XQ,YQ)
          ELSE
              CALL GRLIN2(XP,YP,XQ,YQ)
          END IF
      END IF
      DS = ALPHA2*SEGLEN
      IF (ALPHA2 .GE. 1.0) THEN
          GRPOFF(GRCIDE) = GRPOFF(GRCIDE) + DS - DSOLD
          RETURN
      END IF
      GRIPAT(GRCIDE) = MOD(GRIPAT(GRCIDE),8) + 1
      GRPOFF(GRCIDE) = 0.0
      GO TO 10
      END
C*GRLIN2 -- draw a normal line
C+
      SUBROUTINE GRLIN2 (X0,Y0,X1,Y1)
C
C GRPCKG : plot a visible line segment in absolute coords from
C (X0,Y0) to (X1,Y1).  The endpoints of the line segment are rounded
C to the nearest integer and passed to the appropriate device-specific
C routine. It is assumed that the entire line-segment lies within the
C view surface, and that the physical device coordinates are
C non-negative.
C--
C (1-Jun-1984)
C 19-Oct-1984 - rewritten for speed [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    X0,Y0,X1,Y1
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER CHR
C
C- If this is first thing plotted then set something plotted flag
C- and for a GREXEC device call BEGIN_PICTURE.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C---
      RBUF(1)=X0
      RBUF(2)=Y0
      RBUF(3)=X1
      RBUF(4)=Y1
      NBUF=4
C     WRITE(*,'(A,4F10.5)') 'GRLIN2',RBUF(1), RBUF(2), RBUF(3), RBUF(4)
      CALL GREXEC(GRGTYP,12,RBUF,NBUF,CHR,LCHR)
C
      END
C*GRLIN3 -- draw a thick line (multiple strokes)
C+
      SUBROUTINE GRLIN3 (X0,Y0,X1,Y1)
C
C GRPCKG: draw a heavy line from (X0,Y0) to (X1,Y1) by making multiple
C strokes.  In order to simulate a thick pen, the line drawn has
C circular, rather than square, end points.  If this is not done,
C thick letters and other figures have an abnormal and unpleasant
C appearance.
C
C Vocabulary:
C
C LINEWT: the number of strokes required to draw the line; if
C       this is odd, one stroke will lie along the requested vector.
C       The nominal line thickness is (LINEWT-1)*0.005 in.
C RSQURD: the square of the semi-line thickness.
C (DX,DY): the vector length of the line.
C (VX,VY): a vector of length 1 pixel in the direction of the line.
C (VY,-VX): a vector of length 1 pixel perpendicular to (VX,VY).
C OFF: the offset parallel to (VY,-VX) of the K'th stroke.
C (VXK,VYK): the vector increment of the K'th stroke to allow for the
C       semi-circular terminal on the line.
C (PXK,PYK): the vector offset of the K'th stroke perpendicular to the
C       line vector.
C--
C (1-Feb-1983)
C 23-Nov-1994 - change algorithm so that the unit of line-width is
C               0.005 inch instead of 1 pixel [TJP].
C March 1995 - added ABS to prevent domain error in SQRT (CTD)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  K,LINEWT
      REAL     DX,DY, HK, OFF, PXK,PYK, RSQURD, VLEN,VX,VY,VXK,VYK
      REAL     X0,X1,Y0,Y1
      REAL     XS0,XS1, YS0,YS1, SPIX,SPIY
      LOGICAL  VIS
C
C Determine number of strokes and line thickness.
C
      LINEWT = GRWIDT(GRCIDE)
      RSQURD = ((LINEWT-1)**2)*0.25
C
C Determine the vectors (VX,VY), (VY,-VX). If the line-length is zero,
C pretend it is a very short horizontal line.
C
      DX = X1 - X0
      DY = Y1 - Y0
      VLEN = SQRT(DX**2 + DY**2)
      SPIX = GRPXPI(GRCIDE)*0.005
      SPIY = GRPYPI(GRCIDE)*0.005
C
      IF (VLEN .EQ. 0.0) THEN
          VX = SPIX
          VY = 0.0
      ELSE
          VX = DX/VLEN*SPIX
          VY = DY/VLEN*SPIY
      END IF
C
C Draw LINEWT strokes. We have to clip again in case thickening the
C line has taken us outside the window.
C
      OFF = (LINEWT-1)*0.5
      DO 10 K=1,LINEWT
          PXK = VY*OFF
          PYK = -(VX*OFF)
          HK  = SQRT(ABS(RSQURD - OFF**2))
          VXK = VX*HK
          VYK = VY*HK
          XS1 = X1+PXK+VXK
          YS1 = Y1+PYK+VYK
          XS0 = X0+PXK-VXK
          YS0 = Y0+PYK-VYK
          CALL GRCLPL(XS1,YS1,XS0,YS0,VIS)
          IF (VIS) CALL GRLIN2(XS1, YS1, XS0, YS0)
          OFF = OFF - 1.0
   10 CONTINUE
      END
C*GRLINA -- draw a line (absolute, world coordinates)
C+
      SUBROUTINE GRLINA (X,Y)
C
C GRPCKG: draw line from current position to a specified position.
C
C Arguments:
C
C X, Y (real, input): world coordinates of the end-point of the line.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     X,Y
C
      IF (GRCIDE.GE.1) THEN
C         WRITE (*,'(A,2F10.5)') 'GRLINA', X, Y
          CALL GRLIN0( X * GRXSCL(GRCIDE) + GRXORG(GRCIDE),
     1                 Y * GRYSCL(GRCIDE) + GRYORG(GRCIDE) )
      END IF
      END

C*GRLINR -- draw a line (relative, world coordinates)
C+
      SUBROUTINE GRLINR (DX,DY)
C
C GRPCKG: draw a line from the current position by a specified
C relative displacement.
C
C Arguments:
C
C DX, DY (real, input): the displacement in world coordinates: the pen
C       position is incremented by DX in x and DY in y.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     DX,DY
C
      IF (GRCIDE.GE.1) THEN
          CALL GRLIN0( DX * GRXSCL(GRCIDE) + GRXPRE(GRCIDE),
     1                 DY * GRYSCL(GRCIDE) + GRYPRE(GRCIDE) )
      END IF
      END

C*GRMARK -- mark points with specified symbol
C+
      SUBROUTINE GRMARK (IDENT,CENTER,SYMBOL,ABSXY,POINTS,X,Y)
C
C GRPCKG: mark a sequence of points with a specified symbol. The
C plot is windowed in the current subarea.
C
C Arguments:
C
C IDENT (integer, input): plot identifier from GROPEN.
C CENTER (input, logical): if .TRUE. the symbol is centered on the point,
C      otherwise the bottom left corner is placed at the point.
C SYMBOL (byte or integer, input): code number of symbol in range 0-127
C      (ASCII character or special symbol); if SYMBOL is outside this
C      range, nothing is plotted.
C ABSXY (logical, input): if .TRUE. (X,Y) are absolute (device)
C      coordinates; otherwise they are world coordinates and the
C      scaling transformation is applied.
C POINTS (integer, input): the number of points; if POINTS is less than
C      or equal to 0, nothing is plotted.
C X,Y (real arrays, dimension at least POINTS, input): the coordinate
C      pairs; if POINTS=1, these may be scalars instead of arrays.
C
C (9-Mar-1983)
C-----------------------------------------------------------------------
      INTEGER  SYMBOL
      CHARACTER*1 MARK
      INTEGER  I, IDENT, POINTS
      LOGICAL  ABSXY, CENTER
      REAL     X(*), Y(*)
C-----------------------------------------------------------------------
      IF (POINTS.LE.0 .OR. SYMBOL.LT.0 .OR. SYMBOL.GT.127) RETURN
      CALL GRSLCT(IDENT)
      MARK = CHAR(SYMBOL)
      DO 10 I=1,POINTS
          CALL GRCHR0(.TRUE., CENTER, 0.0, ABSXY, X(I), Y(I), MARK)
   10 CONTINUE
C-----------------------------------------------------------------------
      END
      SUBROUTINE GRMCUR (ICH, ICX, ICY)
      INTEGER ICH, ICX, ICY
C
C Cursor movement:
C Input: ICH character code
C In/Out: ICX, ICY cursor position
C-----------------------------------------------------------------------
      INTEGER STEP
      SAVE STEP
      DATA STEP /4/
C
C     Up arrow or keypad 8:
      IF (ICH.EQ.-1 .OR. ICH.EQ.-28) THEN
          ICY = ICY+STEP
C     Down arrow or keypad 2:
      ELSE IF (ICH.EQ.-2 .OR. ICH.EQ.-22) THEN
          ICY = ICY-STEP
C     Right arrow or keypad 6:
      ELSE IF (ICH.EQ.-3 .OR. ICH.EQ.-26) THEN
          ICX = ICX+STEP
C     Left arrow or keypad 4:
      ELSE IF (ICH.EQ.-4 .OR. ICH.EQ.-24) THEN
          ICX = ICX-STEP
C     Keypad 7 (left and up):
      ELSE IF (ICH.EQ.-27) THEN
          ICX = ICX-STEP
          ICY = ICY+STEP
C     Keypad 9 (right and up):
      ELSE IF (ICH.EQ.-29) THEN
          ICX = ICX+STEP
          ICY = ICY+STEP
C     Keypad 3 (right and down):
      ELSE IF (ICH.EQ.-23) THEN
          ICX = ICX+STEP
          ICY = ICY-STEP
C     Keypad 1 (left and down):
      ELSE IF (ICH.EQ.-21) THEN
          ICX = ICX-STEP
          ICY = ICY-STEP
C     PF1:
      ELSE IF (ICH.EQ.-11) THEN
          STEP = 1
C     PF2:
      ELSE IF (ICH.EQ.-12) THEN
          STEP = 4
C     PF3:
      ELSE IF (ICH.EQ.-13) THEN
          STEP = 16
C     PF4:
      ELSE IF (ICH.EQ.-14) THEN
          STEP = 64
      END IF
      END
C*GRMKER -- draw graph markers
C+
      SUBROUTINE GRMKER (SYMBOL,ABSXY,N,X,Y)
C
C GRPCKG: Draw a graph marker at a set of points in the current
C window. Line attributes (color, intensity, and  thickness)
C apply to markers, but line-style is ignored. After the call to
C GRMKER, the current pen position will be the center of the last
C marker plotted.
C
C Arguments:
C
C SYMBOL (input, integer): the marker number to be drawn. Numbers
C       0-31 are special marker symbols; numbers 32-127 are the
C       corresponding ASCII characters (in the current font). If the
C       number is >127, it is taken to be a Hershey symbol number.
C       If -ve, a regular polygon is drawn.
C ABSXY (input, logical): if .TRUE., the input corrdinates (X,Y) are
C       taken to be absolute device coordinates; if .FALSE., they are
C       taken to be world coordinates.
C N (input, integer): the number of points to be plotted.
C X, Y (input, real arrays, dimensioned at least N): the (X,Y)
C       coordinates of the points to be plotted.
C--
C (19-Mar-1983)
C 20-Jun-1985 - revise to window markers whole [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C  1-Aug-1988 - add direct use of Hershey number [TJP].
C 15-Dec-1988 - standardize [TJP].
C 17-Dec-1990 - add polygons [PAH/TJP].
C 12-Jun-1992 - [TJP]
C 22-Sep-1992 - add support for hardware markers [TJP].
C  1-Sep-1994 - suppress driver call [TJP].
C 15-Feb-1994 - fix bug (expanding viewport!) [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  SYMBOL
      INTEGER  C 
      LOGICAL  ABSXY, UNUSED, VISBLE
      INTEGER  I, K, LSTYLE, LX, LY, LXLAST, LYLAST, N, SYMNUM, NV
      INTEGER  XYGRID(300)
      REAL     ANGLE, COSA, SINA, FACTOR, RATIO, X(*), Y(*)
      REAL     XCUR, YCUR, XORG, YORG
      REAL     THETA, XOFF(40), YOFF(40), XP(40), YP(40)
      REAL     XMIN, XMAX, YMIN, YMAX
      REAL     XMINX, XMAXX, YMINX, YMAXX
      REAL     RBUF(4)
      INTEGER  NBUF,LCHR
      CHARACTER*32 CHR
C
C Check that there is something to be plotted.
C
      IF (N.LE.0) RETURN
C
C Check that a device is selected.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRMKER - no graphics device is active.')
          RETURN
      END IF
C
      XMIN = GRXMIN(GRCIDE)
      XMAX = GRXMAX(GRCIDE)
      YMIN = GRYMIN(GRCIDE)
      YMAX = GRYMAX(GRCIDE)
      XMINX = XMIN-0.01
      XMAXX = XMAX+0.01
      YMINX = YMIN-0.01
      YMAXX = YMAX+0.01
C
C Does the device driver do markers (only markers 0-31 at present)?
C
      IF (GRGCAP(GRCIDE)(10:10).EQ.'M' .AND.
     :     SYMBOL.GE.0 .AND. SYMBOL.LE.31) THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C         -- symbol number
          RBUF(1) = SYMBOL
C          -- scale factor
          RBUF(4) = GRCFAC(GRCIDE)/2.5
          NBUF = 4
          LCHR = 0
          DO 10 K=1,N
C             -- convert to device coordinates
              CALL GRTXY0(ABSXY, X(K), Y(K), XORG, YORG)
C             -- is the marker visible?
              CALL GRCLIP(XORG, YORG, XMINX, XMAXX, YMINX, YMAXX, C)
              IF (C.EQ.0) THEN
                  RBUF(2) = XORG
                  RBUF(3) = YORG
                  CALL GREXEC(GRGTYP,28,RBUF,NBUF,CHR,LCHR)
              END IF
   10     CONTINUE
          RETURN
      END IF
C
C Otherwise, draw the markers here.
C
C Save current line-style, and set style "normal".
C
      CALL GRQLS(LSTYLE)
      CALL GRSLS(1)
C
C Save current viewport, and open the viewport to include the full
C view surface.
C
      CALL GRAREA(GRCIDE, 0.0, 0.0, 0.0, 0.0)
C
C Compute scaling and orientation.
C
      ANGLE = 0.0
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
C
C Convert the supplied marker number SYMBOL to a symbol number and
C obtain the digitization.
C
      IF (SYMBOL.GE.0) THEN
          IF (SYMBOL.GT.127) THEN
              SYMNUM = SYMBOL
          ELSE
              CALL GRSYMK(SYMBOL,GRCFNT(GRCIDE),SYMNUM)
          END IF
          CALL GRSYXD(SYMNUM, XYGRID, UNUSED)
C
C Positive symbols.
C
      DO 380 I=1,N
          CALL GRTXY0(ABSXY, X(I), Y(I), XORG, YORG)
          CALL GRCLIP(XORG, YORG, XMINX, XMAXX, YMINX, YMAXX, C)
          IF (C.NE.0) GOTO 380
          VISBLE = .FALSE.
          K = 4
          LXLAST = -64
          LYLAST = -64
  320       K = K+2
            LX = XYGRID(K)
            LY = XYGRID(K+1)
            IF (LY.EQ.-64) GOTO 380
            IF (LX.EQ.-64) THEN
                VISBLE = .FALSE.
            ELSE
                IF ((LX.NE.LXLAST) .OR. (LY.NE.LYLAST)) THEN
                    XCUR = XORG + (COSA*LX - SINA*LY)*RATIO
                    YCUR = YORG + (SINA*LX + COSA*LY)
                    IF (VISBLE) THEN
                        CALL GRLIN0(XCUR,YCUR)
                    ELSE
                        GRXPRE(GRCIDE) = XCUR
                        GRYPRE(GRCIDE) = YCUR
                    END IF
                END IF
                VISBLE = .TRUE.
                LXLAST = LX
                LYLAST = LY
            END IF
            GOTO 320
  380 CONTINUE
C
C Negative symbols.
C
      ELSE
C         ! negative symbol: filled polygon of radius 8
          NV = MIN(31,MAX(3,ABS(SYMBOL)))
          DO 400 I=1,NV
              THETA = 3.14159265359*(REAL(2*(I-1))/REAL(NV)+0.5) - ANGLE
              XOFF(I) = COS(THETA)*FACTOR*RATIO/GRXSCL(GRCIDE)*8.0
              YOFF(I) = SIN(THETA)*FACTOR/GRYSCL(GRCIDE)*8.0
  400     CONTINUE
          DO 420 K=1,N
              CALL GRTXY0(ABSXY, X(K), Y(K), XORG, YORG)
              CALL GRCLIP(XORG, YORG, XMINX, XMAXX, YMINX, YMAXX, C)
              IF (C.EQ.0) THEN
                  DO 410 I=1,NV
                      XP(I) = X(K)+XOFF(I)
                      YP(I) = Y(K)+YOFF(I)
  410             CONTINUE
                  CALL GRFA(NV, XP, YP)
              END IF
  420     CONTINUE
      END IF
C
C Set current pen position.
C
      GRXPRE(GRCIDE) = XORG
      GRYPRE(GRCIDE) = YORG
C
C Restore the viewport and line-style, and return.
C
      GRXMIN(GRCIDE) = XMIN
      GRXMAX(GRCIDE) = XMAX
      GRYMIN(GRCIDE) = YMIN
      GRYMAX(GRCIDE) = YMAX
      CALL GRSLS(LSTYLE)
C
      END
C*GRMOVA -- move pen (absolute, world coordinates)
C+
      SUBROUTINE GRMOVA (X,Y)
C
C GRPCKG: move the pen to a specified location.
C
C Arguments:
C
C X, Y (real, input): world coordinates of the new pen position.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     X,Y
C
      IF (GRCIDE.GE.1) THEN
C         WRITE (*,'(A,2F10.5)') 'GRMOVA', X, Y
          GRXPRE(GRCIDE) = X * GRXSCL(GRCIDE) + GRXORG(GRCIDE)
          GRYPRE(GRCIDE) = Y * GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      END IF
      END

C*GRMOVR -- move pen (relative, world coordinates)
C+
      SUBROUTINE GRMOVR (DX,DY)
C
C GRPCKG: move the pen through a specified displacement.
C
C Arguments:
C
C DX, DY (real, input): the displacement in world coordinates: the pen
C       position is incremented by DX in x and DY in y.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     DX,DY
C
      IF (GRCIDE.GE.1) THEN
          GRXPRE(GRCIDE) = GRXPRE(GRCIDE) + DX*GRXSCL(GRCIDE)
          GRYPRE(GRCIDE) = GRYPRE(GRCIDE) + DY*GRYSCL(GRCIDE)
      END IF
      END
C*GRMSG -- issue message to user
C+
      SUBROUTINE GRMSG (TEXT)
      CHARACTER*(*) TEXT
C
C Display a message on standard output.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C  8-Nov-1994 [TJP].
C-----------------------------------------------------------------------
      INTEGER   GRTRIM
C
      IF (TEXT.NE.' ') THEN
          WRITE (*, '(1X,A)') TEXT(1:GRTRIM(TEXT))
      END IF
      END
C*GROPEN -- open device for graphics
C+
      INTEGER FUNCTION GROPEN (TYPE,DUMMY,FILE,IDENT)
      INTEGER   TYPE, DUMMY, IDENT
      CHARACTER*(*) FILE
C
C GRPCKG: assign a device and prepare for plotting.  GROPEN must be
C called before all other calls to GRPCKG routines.
C
C Returns:
C
C GROPEN (output, integer): 1 => success, any other value
C       indicates a failure (usually the value returned will
C       be a VMS error code). In the event of an error, a
C       message will be sent to the standard error unit.
C
C Arguments:
C
C TYPE (input, integer): default device type (integer code).
C DUMMY (input, integer): not used at present.
C FILE (input, character): plot specifier, of form 'device/type'.
C IDENT (output, integer): plot identifier to be used in later
C       calls to GRPCKG.
C
C  1-Jun-1984 - [TJP].
C  2-Jul-1984 - change to call GRSLCT [TJP].
C 13-Jul-1984 - add device initialization [TJP].
C 23-Jul-1984 - add /APPEND qualifier.
C 19-Oct-1984 - add VV device [TJP].
C 26-Dec-1984 - obtain default file name from common [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 12-Oct-1986 - fix bug causing GREXEC to erase screen [AFT].
C  3-Jun-1987 - remove declaration of exit handler [TJP].
C 15-Dec-1988 - standardize [TJP].
C 25-Jun-1989 - remove code that removes spaces from the device name 
C               [TJP].
C 26-Nov-1990 - [TJP].
C  5-Jan-1993 - [TJP].
C  1-Sep-1994 - store device capabilities in common for later use [TJP].
C 17-Apr-1995 - zero-length string fix [TJP].
C  6-Jun-1995 - explicitly initialize GRSTAT [TJP].
C 29-Apr-1996 - moved initialization into GRINIT [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   IER, FTYPE, NBUF, LCHR
      INTEGER   GRPARS, GRTRIM
      REAL      RBUF(6)
      LOGICAL   APPEND
      CHARACTER*128 FFILE,CHR
C
C Initialize GRPCKG; read font file (if necessary).
C
      CALL GRINIT
C
C Allocate an identifier.
C
      IDENT = 1
   10 IF (GRSTAT(IDENT).NE.0) THEN
          IDENT = IDENT+1
          IF (IDENT.GT.GRIMAX) THEN
              CALL GRWARN('Too many active plots.')
              GROPEN = -1
              IDENT = 0
              RETURN
          END IF
      GOTO 10
      END IF
C
C Validate the device specification.
C
      IER = GRPARS(FILE,FFILE,FTYPE,APPEND)
      IF (IER.NE.1) THEN
          CHR = 'Invalid device specification: '
          CHR(31:) = FILE
          CALL GRWARN(CHR)
          GROPEN = -1
          RETURN
      END IF
      IF (FTYPE.EQ.0) FTYPE = TYPE
      IF (1.LE.FTYPE) THEN
          GRTYPE(IDENT) = FTYPE
          GRGTYP = FTYPE
      ELSE
          CHR = 'Device type omitted or invalid: '
          CHR(33:) = FILE
          CALL GRWARN(CHR)
          GROPEN = -1
          RETURN
      END IF
C
C Install the file name, or assign default.
C
      IF (FFILE.EQ.' ') THEN
          CALL GREXEC(GRGTYP, 5,RBUF,NBUF,FFILE,LCHR)
      END IF
      GRFILE(IDENT) = FFILE
      GRFNLN(IDENT) = MAX(1,GRTRIM(GRFILE(IDENT)))
C
C Open workstation.
C
      RBUF(3)=0
      IF (APPEND) RBUF(3)=1
      NBUF=3
      CALL GREXEC(GRGTYP, 9,RBUF,NBUF, GRFILE(IDENT),GRFNLN(IDENT))
      GRUNIT(IDENT)=RBUF(1)
      GROPEN=RBUF(2)
      IF (GROPEN.NE.1) RETURN
      GRPLTD(IDENT) = .FALSE.
      GRSTAT(IDENT) = 1
      CALL GRSLCT(IDENT)
C
C Install the default plot parameters
C
C--- Inquire color-index range.
      CALL GREXEC(GRGTYP, 2,RBUF,NBUF,CHR,LCHR)
      GRMNCI(IDENT)=RBUF(5)
      GRMXCI(IDENT)=RBUF(6)
C--- Inquire resolution.
      CALL GREXEC(GRGTYP, 3,RBUF,NBUF,CHR,LCHR)
      GRPXPI(IDENT)=RBUF(1)
      GRPYPI(IDENT)=RBUF(2)
C--- Inquire default character size.
      CALL GREXEC(GRGTYP, 7,RBUF,NBUF,CHR,LCHR)
      GRCSCL(IDENT) = RBUF(1)
      GRCFAC(IDENT) = RBUF(1)
C--- Inquire default plot size.
      CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
      GRXMXA(IDENT) = RBUF(2)
      GRYMXA(IDENT) = RBUF(4)
      GRXMIN(IDENT) = RBUF(1)
      GRXMAX(IDENT) = RBUF(2)
      GRYMIN(IDENT) = RBUF(3)
      GRYMAX(IDENT) = RBUF(4)
C--- Inquire device capabilities.
      GRGCAP(IDENT) = 'NNNNNNNNNNN'
      CALL GREXEC(GRGTYP, 4,RBUF,NBUF,CHR,LCHR)
      IF (LCHR.GT.LEN(GRGCAP(IDENT))) LCHR = LEN(GRGCAP(IDENT))
      GRGCAP(IDENT)(1:LCHR) = CHR(:LCHR)
C--- Current pen position.
      GRXPRE(IDENT) = 0.0
      GRYPRE(IDENT) = 0.0
C--- GRSETS has not been called.
      GRADJU(IDENT) = .FALSE.
C---Default scaling.
      CALL GRTRN0(0.0, 0.0, 1.0, 1.0)
C
C Default attributes.
C  text font (normal)
C  color (white)
C  line-style (full)
C  line-width (minimum)
C  marker number (dot)
C
      GRCFNT(IDENT) = 1
      GRCCOL(IDENT) = 1
      GRSTYL(IDENT) = 1
      GRWIDT(IDENT) = 1
      GRCMRK(IDENT) = 1
      GRDASH(IDENT) = .FALSE.
C
      GROPEN = 1
C
      END
C*GRPAGE -- end picture
C+
      SUBROUTINE GRPAGE
C
C GRPCKG: Advance the plotting area to a new page. For video devices,
C this amounts to erasing the screen; for hardcopy devices, the plot
C buffer is written to the output file followed by a form-feed to
C advance the paper to the start of the next page.
C
C Arguments: none.
C--
C  3-Jun-1983 - [TJP].
C 18-Feb-1984 - remove unnecessary 'T' initialization of VT125, and add
C               S(G1) for Rainbow REGIS [TJP].
C  1-Jun-1984 - add type GMFILE [TJP].
C  2-Jul-1984 - change initialization of VT125 for color [TJP].
C 13-Jul-1984 - move initialization of VT125 and Grinnell to GROPEN
C               [TJP].
C 19-Oct-1984 - add VV device [TJP].
C 29-Jan-1985 - add HP2648 terminal [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - fix GREXEC end picture sequence [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C 11-Feb-1992 - update veiew surface size: it may have changed! [TJP].
C  5-Jan-1993 - but only if GRSETS has not been called! [TJP]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      INTEGER NBUF,LCHR
      REAL    RBUF(6)
C
      CHARACTER CHR
C
C Flush the buffer.
C
      CALL GRTERM
C
C Erase the text screen (if there is one).
C
      CALL GRETXT
C
C End picture.
C
      CALL GREPIC
C
C Update the view surface size: it may have changed (on windowing 
C devices)
C
      IF (.NOT.GRADJU(GRCIDE)) THEN
          CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
          GRXMXA(GRCIDE) = RBUF(2)
          GRYMXA(GRCIDE) = RBUF(4)
      END IF
C
      END
C*GRPARS -- parse device specification string
C+
      INTEGER FUNCTION GRPARS (SPEC,DEV,TYPE,APPEND)
      CHARACTER*(*) SPEC, DEV
      INTEGER  TYPE
      LOGICAL  APPEND
C
C GRPCKG: decode a device-specification; called by GROPEN.
C
C Returns:
C  GRPARS (output): 1 if the device-specification is
C       acceptable; any other value indicates an error.
C
C Arguments:
C  SPEC (input): the device specification.
C  DEV  (output):  device name or file spec.
C  TYPE (output): device type (integer code); 0 if no device
C       type is specified.
C  APPEND (output): .TRUE. if /APPEND specified, .FALSE. otherwise.
C--
C 23-Jul-1984 - [TJP].
C 19-Feb-1988 - allow device part to be quoted [TJP].
C 30-Mar-1989 - remove logical translation of device and type [TJP].
C 17-Jun-1991 - ignore comments after ' (' [TJP].
C 19-Dec-1994 - rewritten to scan backwards [TJP].
C  6-Jun-1995 - correct a zero-length string problem [TJP].
C-----------------------------------------------------------------------
      CHARACTER*32  CTYPE, UPPER
      CHARACTER*6   APPSTR
      CHARACTER*256 DESCR
      INTEGER       GRDTYP, GRTRIM
      INTEGER       L, LC, LS
      DATA          APPSTR/'APPEND'/
C
C Default results.
C
      DEV = ' '
      TYPE = 0
      APPEND = .FALSE.
      GRPARS = 1
      CTYPE = ' '
C
C Null string is acceptable.
C
      IF (LEN(SPEC).LT.1) RETURN
      IF (SPEC.EQ.' ') RETURN
C
C On systems where it is possible, perform a "logical name" translation.
C
      DESCR = SPEC
      CALL GRLGTR(DESCR)
C
C Discard trailing blanks: L is length of remainder.
C
      L = GRTRIM(DESCR)
C
C Find last slash in string (position LS or 0).
C
      LS = L
 20   IF (DESCR(LS:LS).NE.'/') THEN
         LS = LS-1
         IF (LS.GT.0) GOTO 20
      END IF
C
C Check for /APPEND qualifier; if present, look again for type.
C
      IF (LS.GT.0) THEN
         CTYPE = DESCR(LS+1:L)
         CALL GRTOUP(UPPER,CTYPE)
         CTYPE = UPPER
         IF (CTYPE.EQ.APPSTR) THEN
            APPEND = .TRUE.
            L = LS-1
            LS = L
 30         IF (DESCR(LS:LS).NE.'/') THEN
               LS = LS-1
               IF (LS.GT.0) GOTO 30
            END IF
         ELSE
            APPEND = .FALSE.
         END IF
      END IF
C
C If LS=0 there is no type field: use PGPLOT_TYPE.
C
      IF (LS.EQ.0) THEN
         CALL GRGENV('TYPE', CTYPE, LC)
      ELSE
         CTYPE = DESCR(LS+1:L)
         LC = L-LS
         L = LS-1
      END IF
C
C Check for allowed type.
C
      IF (LC.GT.0) THEN
         CALL GRTOUP(UPPER,CTYPE)
         CTYPE = UPPER
         TYPE = GRDTYP(CTYPE)
         IF (TYPE.EQ.0) CALL GRWARN('Unrecognized device type')
         IF (TYPE.EQ.-1) CALL GRWARN('Device type is ambiguous')
      ELSE
         TYPE = 0
         CALL GRWARN('Device type omitted')
      END IF
      IF (TYPE.EQ.0) GRPARS = GRPARS+2
C
C Remove quotes from device if necessary.
C
      IF (L.GE.1) THEN
         IF (DESCR(1:1).EQ.'"' .AND. DESCR(L:L).EQ.'"') THEN
            DEV = DESCR(2:L-1)
            L = L-2
         ELSE
            DEV = DESCR(1:L)
         END IF
      END IF
C
C      write (*,*) 'Device = [', DEV(1:L), ']'
C      write (*,*) 'Type   = [', CTYPE, ']', TYPE
C      write (*,*) 'APPEND = ', APPEND
C
      END
C*GRPIXL -- solid-fill multiple rectangular areas
C+
      SUBROUTINE GRPIXL (IA, IDIM, JDIM, I1, I2, J1, J2, 
     1                   X1, X2, Y1, Y2)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X1, X2, Y1, Y2
C
C Determine the size of each rectangular element. If it is equal
C to the device pen width and the device supports pixel primitives,
C use pixel primitives. Otherwise, if the size is smaller than the
C device pen width emulate pixel output by plotting points. If the
C size is larger than the device pen width, emulate by outputting
C solid-filled rectangles.
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X1, Y1 (input)  : world coordinates of one corner of the output
C                    region
C  X2, Y2 (input)  : world coordinates of the opposite corner of the
C                    output region
C--
C 18-Jan-1991 - [Ge van Geldorp]
C 31-Mar-1993 - Include color PostScript GRPXPS [Remko Scharroo]
C  4-Apr-1993 - New version of GRPXPS incorporated
C  4-Aug-1993 - Debugging
C  7-Sep-1994 - Revised for v5.0 [TJP].
C 24-Jan-1996 - GRXMIN etc changed to REAL as required in grpckg1.inc [RS]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(3)
      INTEGER NBUF, LCHR
      CHARACTER*32 CHR
      REAL    XLL, YLL, XUR, YUR
      REAL    XMIN, YMIN, XMAX, YMAX, XPIX, YPIX
      REAL    WIDTH, XSIZE, YSIZE
      INTEGER IL, IR, JB, JT
C
      IF (GRCIDE.LT.1) RETURN
C
C Convert to device coordinates
C
      CALL GRTXY0(.FALSE., X1, Y1, XLL, YLL)
      CALL GRTXY0(.FALSE., X2, Y2, XUR, YUR)
      XMIN = MIN(XLL,XUR)
      XMAX = MAX(XLL,XUR)
      YMIN = MIN(YLL,YUR)
      YMAX = MAX(YLL,YUR)
C
C Check if completely outside clipping region
C
      IF (XMAX .LT. GRXMIN(GRCIDE) .OR. GRXMAX(GRCIDE) .LT. XMIN .OR.
     1    YMAX .LT. GRYMIN(GRCIDE) .OR. GRYMAX(GRCIDE) .LT. YMIN)
     2   RETURN
C
C Don't paint "pixels" completely before left clipping boundary
C
      XPIX = XMAX - XMIN
      YPIX = YMAX - YMIN
      IF (XMIN .LT. GRXMIN(GRCIDE)) THEN
         IL = I1 + (GRXMIN(GRCIDE) - XMIN) * (I2 - I1 + 1) / XPIX
         XMIN = XMIN + (XPIX * (IL - I1)) / (I2 - I1 + 1)
      ELSE
         IL = I1
      ENDIF
C
C Don't paint "pixels" completely after right clipping boundary
C
      IF (GRXMAX(GRCIDE) .LT. XMAX) THEN
         IR = I2 - (XMAX - GRXMAX(GRCIDE)) * (I2 - I1 + 1) / XPIX + 1
         XMAX = XMIN + (XPIX * (IR - I1 + 1)) /
     1                 (I2 - I1 + 1)
      ELSE
         IR = I2
      ENDIF
C
C Don't paint "pixels" completely under bottom clipping boundary
C
      IF (YMIN .LT. GRYMIN(GRCIDE)) THEN
         JB = J1 + (GRYMIN(GRCIDE) - YMIN) * (J2 - J1 + 1) / YPIX
         YMIN = YMIN + (YPIX * (JB - J1)) / (J2 - J1 + 1)
      ELSE
         JB = J1
      ENDIF
C
C Don't paint "pixels" completely above top clipping boundary
C
      IF (GRYMAX(GRCIDE) .LT. YMAX) THEN
         JT = J2 - (YMAX - GRYMAX(GRCIDE)) * (J2 - J1 + 1) / YPIX + 1
         YMAX = YMIN + (YPIX * (JT - J1 + 1)) /
     1                 (J2 - J1 + 1)
      ELSE
         JT = J2
      ENDIF
C
C If device accepts image primitives, use GRPXPS
C
      IF (GRGCAP(GRCIDE)(7:7).EQ.'Q') THEN
         CALL GRPXPS(IA, IDIM, JDIM, IL, IR, JB, JT,
     1		     XMIN,XMAX,YMIN,YMAX)
         RETURN
      ENDIF
C
C Check against pen width
C
      CALL GREXEC(GRGTYP, 3, RBUF, NBUF, CHR, LCHR)
      WIDTH = RBUF(3)
      XSIZE = (I2 - I1 + 1) * WIDTH
      YSIZE = (J2 - J1 + 1) * WIDTH
      XPIX = XMAX - XMIN + 1
      YPIX = YMAX - YMIN + 1
C
C Use rectangles if "pixel" is too large
C
      IF (XPIX .GT. XSIZE + 0.5 * WIDTH .OR.
     1    YPIX .GT. YSIZE + 0.5 * WIDTH) THEN
*     write (6,*) 'GRPXRE'
         CALL GRPXRE(IA, IDIM, JDIM, IL, IR, JB, JT,
     1		     XMIN, XMAX, YMIN, YMAX)
C
C Use either pixel primitives or points
C
      ELSE
C
C Clip pixels lying more than 50% outside clipping boundaries
C
         IF (XMIN .LT. GRXMIN(GRCIDE) - 0.5 * WIDTH) THEN
            XMIN = XMIN + XPIX / (IR - IL + 1)
            IL = IL + 1
         ENDIF
         IF (GRXMAX(GRCIDE) + 0.5 * WIDTH .LT. XMAX) THEN
            XMAX = XMAX - XPIX / (IR - IL + 1)
            IR = IR - 1
         ENDIF
         IF (YMIN .LT. GRYMIN(GRCIDE) - 0.5 * WIDTH) THEN
            YMIN = YMIN + YPIX / (JT - JB + 1)
            JB = JB + 1
         ENDIF
         IF (GRYMAX(GRCIDE) + 0.5 * WIDTH .LT. YMAX) THEN
            YMAX = YMAX - YPIX / (JT - JB + 1)
            JT = JT - 1
         ENDIF
C
C Recalculate size
C
         XSIZE = (IR - IL + 1) * WIDTH
         YSIZE = (JT - JB + 1) * WIDTH
         XPIX = XMAX - XMIN + 1
         YPIX = YMAX - YMIN + 1
C
C Use pixel primitives if available and possible
C
         IF (GRGCAP(GRCIDE)(7:7) .EQ. 'P' .AND. 
     1       XSIZE - 0.5 * WIDTH .LE. XPIX .AND.
     2       YSIZE - 0.5 * WIDTH .LE. YPIX) THEN
*     write (6,*) 'GRPXPX'
            CALL GRPXPX(IA, IDIM, JDIM, IL, IR, JB, JT, XMIN, YMIN)
C
C Otherwise, use points
C
         ELSE
*     write (6,*) 'GRPXPO'
            CALL GRPXPO(IA, IDIM, JDIM, IL, IR, JB, JT,
     1		     XMIN, XMAX, YMIN, YMAX)
         ENDIF
      ENDIF
      END
C*GRPOCL -- polygon clip
C+
      SUBROUTINE GRPOCL (N,PX,PY, EDGE, VAL, MAXOUT, NOUT, QX, QY)
      INTEGER N, NOUT, EDGE, MAXOUT
      REAL    PX(*), PY(*), QX(*), QY(*)
      REAL    VAL
C
C Clip a polygon against a rectangle: Sutherland-Hodgman algorithm.
C this routine must be called four times to clip against each of the
C edges of the rectangle in turn.      
C
C Arguments:
C
C N (input, integer): the number of vertices of the polygon (at least
C       3).
C PX, PY (input, real arrays, dimension at least N): world coordinates
C       of the N vertices of the input polygon.
C EDGE (input, integer):
C     1: clip against left edge,   X > XMIN=VAL
C     2: clip against right edge,  X < XMAX=VAL
C     3: clip against bottom edge, Y > YMIN=VAL
C     4: clip against top edge,    Y < YMIN=VAL
C VAL  (input, real): coordinate value of current edge.
C MAXOUT (input, integer): maximum number of vertices allowed in
C     output polygon (dimension of QX, QY).
C NOUT (output, integer): the number of vertices in the clipped polygon.
C QX, QY (output, real arrays, dimension at least MAXOUT): world
C       coordinates of the NOUT vertices of the output polygon.
C--
C 19-Sep-1994 - [TJP].
C 27-Feb-1996 - fix bug: overflow if coordinates are large [TJP].
C 11-Jul-1996 - fix bug: left and bottom edges disappeared when precisely
C               on edge [Remko Scharroo]
C-----------------------------------------------------------------------
      INTEGER I
      REAL FX, FY, SX, SY
C
      NOUT = 0
      DO 100 I=1,N
         IF (I.EQ.1) THEN
C           -- save first point
            FX = PX(I)
            FY = PY(I)
         ELSE IF ((EDGE.EQ.1 .OR.EDGE.EQ.2) .AND.
     :            (SIGN(1.0,PX(I)-VAL).NE.SIGN(1.0,SX-VAL))) THEN
C           -- SP intersects this edge: output vertex at intersection
            NOUT = NOUT+1
            IF (NOUT.LE.MAXOUT) THEN
               QX(NOUT) = VAL
               QY(NOUT) = SY + (PY(I)-SY)*((VAL-SX)/(PX(I)-SX))
            END IF
         ELSE IF ((EDGE.EQ.3 .OR.EDGE.EQ.4) .AND.
     :            (SIGN(1.0,PY(I)-VAL).NE.SIGN(1.0,SY-VAL))) THEN
C           -- SP intersects this edge: output vertex at intersection
            NOUT = NOUT+1
            IF (NOUT.LE.MAXOUT) THEN
               QX(NOUT) = SX + (PX(I)-SX)*((VAL-SY)/(PY(I)-SY))
               QY(NOUT) = VAL
            END IF
         END IF
         SX = PX(I)
         SY = PY(I)
         IF ((EDGE.EQ.1.AND.SX.GE.VAL) .OR.
     :       (EDGE.EQ.2.AND.SX.LE.VAL) .OR.
     :       (EDGE.EQ.3.AND.SY.GE.VAL) .OR.
     :       (EDGE.EQ.4.AND.SY.LE.VAL)) THEN
C           -- output visible vertex S
            NOUT = NOUT + 1
            IF (NOUT.LE.MAXOUT) THEN
                QX(NOUT) = SX
                QY(NOUT) = SY
            END IF
         END IF
 100  CONTINUE
C      -- Does SF intersect edge?
      IF ((EDGE.EQ.1 .OR. EDGE.EQ.2) .AND.
     :    (SIGN(1.0,SX-VAL).NE.SIGN(1.0,FX-VAL))) THEN
         NOUT = NOUT+1
         IF (NOUT.LE.MAXOUT) THEN
            QX(NOUT) = VAL
            QY(NOUT) = SY + (FY-SY)*((VAL-SX)/(FX-SX))
         END IF
      ELSE IF ((EDGE.EQ.3 .OR. EDGE.EQ.4) .AND.
     :         (SIGN(1.0,SY-VAL).NE.SIGN(1.0,FY-VAL))) THEN
         NOUT = NOUT+1
         IF (NOUT.LE.MAXOUT) THEN
            QY(NOUT) = VAL
            QX(NOUT) = SX + (FX-SX)*((VAL-SY)/(FY-SY))
         END IF
      END IF
C
      END
C*GRPROM -- prompt user before clearing screen
C+
      SUBROUTINE GRPROM
C
C If the program is running under control of a terminal, display
C message and wait for the user to type <CR> before proceeding.
C
C Arguments:
C  none
C--
C 18-Aug-1994
C-----------------------------------------------------------------------
      INTEGER IER, L, GRGCOM
      CHARACTER*16 JUNK
C
      IER = GRGCOM(JUNK, 'Type <RETURN> for next page: ', L)
      END
C*GRPXPO -- Emulate pixel operations using points
C+
      SUBROUTINE GRPXPO (IA, IDIM, JDIM, I1, I2, J1, J2, 
     1                   X1, X2, Y1, Y2)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X1, X2, Y1, Y2
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X1, X2 (input)  : the horizontal range of the output region
C  Y1, Y2 (input)  : the vertical range of the output region
C--
C 16-Jan-1991 - [GvG]
C 28-Jun-1991
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER LW
      INTEGER I, J
      INTEGER ICOL, LSTCOL
C
C Save attributes
C
      CALL GRQLW(LW)
      CALL GRQCI(ICOL)
      CALL GRSLW(1)
      LSTCOL = ICOL
      DO 20 J = J1, J2
         DO 10 I = I1, I2
C
C Color changed?
C
            IF (IA(I, J) .NE. LSTCOL) THEN
               CALL GRSCI(IA(I, J))
               LSTCOL = IA(I, J)
            ENDIF
C
C Output dot
C
            CALL GRDOT0(X1 + (X2 - X1) * (I - I1 + 0.5) / (I2 - I1 + 1),
     1                  Y1 + (Y2 - Y1) * (J - J1 + 0.5) / (J2 - J1 + 1))
  10     CONTINUE
  20  CONTINUE
C
C Restore attributes
C
      CALL GRSCI(ICOL)
      CALL GRSLW(LW)
      END
C*GRPXPS -- pixel dump for color or grey PostScript.
C+
      SUBROUTINE GRPXPS (IA, IDIM, JDIM, I1, I2, J1, J2,
     :                   XMIN, XMAX, YMIN, YMAX)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL XMIN, XMAX, YMIN, YMAX
C
C This routine is called by GRPIXL.
C--
C  4-Apr-93 - Created from GRGRAY by Remko Scharroo - DUT/SSRT
C  8-Apr-93 - Bugs fixed.
C  6-Jul-94 - Aligned with PGPLOT V4.9H
C  7-Sep-94 - updated for V5.0 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, J, NXP, NYP, NBUF, LCHR, II
      REAL     DX,DY,RBUF(32)
      CHARACTER*32 CHR
C-----------------------------------------------------------------------
      NXP = I2 - I1 + 1
      NYP = J2 - J1 + 1
C
C Build an image transformation matrix.
C
      DX = (XMAX-XMIN)/NXP
      DY = (YMAX-YMIN)/NYP
      RBUF(1) = 0
      RBUF(2) = NXP
      RBUF(3) = NYP
      RBUF(4) = GRXMIN(GRCIDE)
      RBUF(5) = GRXMAX(GRCIDE)
      RBUF(6) = GRYMIN(GRCIDE)
      RBUF(7) = GRYMAX(GRCIDE)
      RBUF(8) = 1.0/DX
      RBUF(9) = 0.0
      RBUF(10) = 0.0
      RBUF(11) = 1.0/DY
      RBUF(12) = (-XMIN)/DX
      RBUF(13) = (-YMIN)/DY
C
C Send setup info to driver.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
      CALL GRTERM
      NBUF = 13
      LCHR = 0
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C
C Send the array of color indices to the driver.
C
      II = 0
      DO 20 J=J1,J2
         DO 10 I=I1,I2
            II = II + 1
            RBUF(II+1) = IA(I,J)
            IF (II.EQ.20) THEN
               NBUF = II+1
               RBUF(1) = II
               CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
               II = 0
            END IF
 10      CONTINUE
 20   CONTINUE
      IF (II.GT.0) THEN
         NBUF = II+1
         RBUF(1) = II
         CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
         II = 0
      END IF
C
C Send termination code to driver.
C
      NBUF = 1
      RBUF(1) = -1
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C-----------------------------------------------------------------------
      END

C*GRPXPX -- Perform pixel operations using pixel primitive
C+
      SUBROUTINE GRPXPX (IA, IDIM, JDIM, I1, I2, J1, J2, X, Y)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X, Y
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X, Y   (input)  : the lower left corner of the output region
C                    (device coordinates)
C--
C 16-Jan-1991 - [GvG]
*  4-Aug-1993 - Debugged by Remko Scharroo
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER     NSIZE
      PARAMETER   (NSIZE = 1280)
      REAL        RBUF(NSIZE + 2)
      REAL        WIDTH
      INTEGER     IC1, IC2
      INTEGER     I, J, L
      INTEGER     NBUF, LCHR
      CHARACTER*1 CHR

      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Get allowable color range and pixel width
C
      CALL GRQCOL(IC1, IC2)
      CALL GREXEC(GRGTYP, 3, RBUF, NBUF, CHR, LCHR)
      WIDTH = RBUF(3)
      DO 30 J = J1, J2
C
C Compute Y coordinate for this line
C
         RBUF(2) = Y + (J - J1) * WIDTH
         I = I1
  10        L = 1
C
C Compute left X coordinate for this line segment
C
            RBUF(1) = X + (I - I1) * WIDTH
C
C Check color index
C
  20           IF (IA(I, J) .LT. IC1 .OR. IC2 .LT. IA(I, J)) THEN
                  RBUF(L + 2) = 1
               ELSE
                  RBUF(L + 2) = IA(I, J)
               ENDIF
               L = L + 1
               I = I + 1
C
C Still room in segment and something left?
C
            IF (L .LE. NSIZE .AND. I .LE. I2) GOTO 20
C
C Output segment
C
*           NBUF = L + 2 ! wrong ! should be: (RS)
            NBUF = L + 1
            CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C
C Something left?
C
         IF (I .LE. I2) GOTO 10
  30  CONTINUE

      END
C*GRPXRE -- Emulate pixel operations using rectangles
C+
      SUBROUTINE GRPXRE (IA, IDIM, JDIM, I1, I2, J1, J2, 
     1                   X1, X2, Y1, Y2)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X1, X2, Y1, Y2
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X1, X2 (input)  : the horizontal range of the output region
C  Y1, Y2 (input)  : the vertical range of the output region
C--
C 18-Jan-1991 - [GvG]
C-----------------------------------------------------------------------
      REAL YB, YT
      INTEGER I, J, ICOL, LSTCOL
C
C Save color attribute
C
      CALL GRQCI(ICOL)
      LSTCOL = ICOL
      DO 20 J = J1, J2
C
C Compute Y range for this index
C
         YB = Y1 + ((Y2 - Y1) * (J - J1)) / (J2 - J1 + 1)
         YT = Y1 + ((Y2 - Y1) * (J - J1 + 1)) / (J2 - J1 + 1)
         DO 10 I = I1, I2
C
C Need to change color?
C
            IF (IA(I, J) .NE. LSTCOL) THEN
               CALL GRSCI(IA(I, J))
               LSTCOL = IA(I, J)
            ENDIF
C
C Output rectangle
C
            CALL GRREC0(X1 + ((X2 - X1) * (I - I1)) / (I2 - I1 + 1), YB,
     1                  X1 + ((X2 - X1) * (I - I1 + 1)) / (I2 - I1 + 1),
     2                  YT)

  10     CONTINUE
  20  CONTINUE
C
C Restore color attribute
C
      CALL GRSCI(ICOL)
      END
C*GRQCAP -- inquire device capabilities
C+
      SUBROUTINE GRQCAP (STRING)
      CHARACTER*(*) STRING
C
C GRPCKG: obtain the "device capabilities" string from the device
C driver for the current device.
C
C Arguments:
C
C STRING (output, CHARACTER*(*)): receives the device capabilities
C       string.
C--
C 26-Nov-92: new routine [TJP].
C  1-Sep-94: get from common instead of driver [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQCAP - no graphics device is active.')
          STRING = 'NNNNNNNNNN'
      ELSE
          STRING = GRGCAP(GRCIDE)
      END IF
C
      END
C*GRQCI -- inquire current color index
C+
      SUBROUTINE GRQCI (C)
C
C GRPCKG: obtain the color index of the current graphics device.
C
C Argument:
C
C C (integer, output): receives the current color index (0-255).
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  C
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQCI - no graphics device is active.')
          C = 1
      ELSE
          C = GRCCOL(GRCIDE)
      END IF
      END
C*GRQCOL -- inquire color capability
C+
      SUBROUTINE GRQCOL (CI1, CI2)
      INTEGER  CI1, CI2
C
C Query the range of color indices available on the current device.
C
C Argument:
C  CI1    (output) : the minimum available color index. This will be
C                    either 0 if the device can write in the
C                    background color, or 1 if not.
C  CI2    (output) : the maximum available color index. This will be
C                    1 if the device has no color capability, or a
C                    larger number (e.g., 3, 7, 15, 255).
C--
C 31-May-1989 - new routine [TJP].
C  1-Sep-1994 - avoid driver call [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
C Error if no workstation is open.
C
      IF (GRCIDE.LT.1) THEN
          CI1 = 0
          CI2 = 0
      ELSE
          CI1 = GRMNCI(GRCIDE)
          CI2 = GRMXCI(GRCIDE)
      END IF
C
      END
C*GRQCR -- inquire color representation
C+
      SUBROUTINE GRQCR (CI, CR, CG, CB)
      INTEGER  CI
      REAL     CR, CG, CB
C
C Return the color representation (red, green, blue intensities) 
C currently associated with the specified color index. This may be
C different from that requested on some devices.
C
C Arguments:
C
C CI (integer, input): color index.
C CR, CG, CB (real, output): red, green, and blue intensities,
C       in range 0.0 to 1.0.
C--
C  7-Sep-1994 - rewrite [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   NBUF, LCHR, K
      REAL      RBUF(6)
      CHARACTER CHR
C
      CR = 1.0
      CG = 1.0
      CB = 1.0
      K  = CI
      IF (GRCIDE.LT.1) THEN
C         -- no device open: return white
          CALL GRWARN('GRQCR: no plot device is open.')
      ELSE IF (GRGCAP(GRCIDE)(9:9).NE.'Y') THEN
C         -- devices that don't allow query color representation:
C            return black for ci 0, white for all others
          IF (K.EQ.0) THEN
             CR = 0.0
             CG = 0.0
             CB = 0.0
          END IF
      ELSE
C         -- query device driver; treat invalid ci as 1
          IF (K.LT.GRMNCI(GRCIDE) .OR. CI.GT.GRMXCI(GRCIDE)) THEN
             CALL GRWARN('GRQCR: invalid color index.')
             K = 1
          END IF
          RBUF(1) = K
          NBUF = 1
          LCHR = 0
          CALL GREXEC(GRGTYP,29,RBUF,NBUF,CHR,LCHR)
          IF (NBUF.LT.4) THEN
             CALL GRWARN('GRSCR: device driver error')
          ELSE
              CR = RBUF(2)
              CG = RBUF(3)
              CB = RBUF(4)
          END IF
      END IF
C
      END

C*GRQDEV -- inquire current device
C+
      SUBROUTINE GRQDEV (DEVICE, L)
      CHARACTER*(*) DEVICE
      INTEGER L
C
C Obtain the name of the current graphics device or file.
C
C Argument:
C  DEVICE (output): receives the device name of the
C       currently active device.
C  L (output): number of characters in DEVICE, excluding trailing
C       blanks.
C--
C 19-Feb-1988
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          DEVICE = '?'
          L = 1
      ELSE
          DEVICE = GRFILE(GRCIDE)
          L = GRFNLN(GRCIDE)
          IF (L.GT.LEN(DEVICE)) L = LEN(DEVICE)
      END IF
      END

C*GRQDT -- inquire current device and type
C+
      SUBROUTINE GRQDT (DEVICE)
C
C GRPCKG: obtain the name and type of the current graphics device.
C
C Argument:
C
C DEVICE (output, character): receives the device name and type of the
C       currently active device in the form 'device/type'; this is a
C       valid string for input to GROPEN.
C--
C  1-Feb-1983
C 19-Feb-1988 - add quotes if necessary.
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) DEVICE
      CHARACTER*14 TYPE
      LOGICAL   JUNK
      INTEGER   L
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQDT - no graphics device is active.')
          DEVICE = '/NULL'
      ELSE
          CALL GRQTYP(TYPE,JUNK)
          L = GRFNLN(GRCIDE)
          IF (L.LE.0) THEN
              DEVICE = '/'//TYPE
          ELSE IF (INDEX(GRFILE(GRCIDE)(1:L), '/').EQ.0) THEN
              DEVICE = GRFILE(GRCIDE)(1:L)//'/'//TYPE
          ELSE
              DEVICE = '"'//GRFILE(GRCIDE)(1:L)//'"/'//TYPE
          END IF
      END IF
      END
C*GRQFNT -- inquire current font
C+
      SUBROUTINE GRQFNT (IF)
C
C GRPCKG: obtain the font number of the current graphics device.
C
C Argument:
C
C IF (integer, output): receives the current font number (1-3).
C--
C (19-Mar-1983)
C 15-Dec-1988 - change name [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IF
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQFNT - no graphics device is active.')
          IF = 1
      ELSE
          IF = GRCFNT(GRCIDE)
      END IF
      END

C*GRQLS -- inquire current line-style
C+
      SUBROUTINE GRQLS (ISTYLE)
      INTEGER  ISTYLE
C
C GRPCKG: obtain the line-style of the current graphics device.
C
C Argument:
C  ISTYLE (output): receives the current line-style code.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQLS - no graphics device is active.')
          ISTYLE = 1
      ELSE
          ISTYLE = GRSTYL(GRCIDE)
      END IF
      END
C*GRQLW -- inquire current line width
C+
      SUBROUTINE GRQLW (IWIDTH)
      INTEGER  IWIDTH
C
C GRPCKG: obtain the line-width of the current graphics device.
C
C Argument:
C  IWIDTH (output): receives the current line-width.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQLW - no graphics device is active.')
          IWIDTH = 1
      ELSE
          IWIDTH = ABS(GRWIDT(GRCIDE))
      END IF
      END
C*GRQPOS -- return current pen position (absolute, world coordinates)
C+
      SUBROUTINE GRQPOS(X,Y)
C
C GRQPOS: returns the current pen position in absolute, world
C coordinates.
C
C Arguments:
C
C X, Y (real, output): world coordinates of the pen position.
C--
C  1-Mar-1991 - new routine  [JM].
C-----------------------------------------------------------------------
      REAL     X,Y
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.GE.1) THEN
          X = (GRXPRE(GRCIDE) - GRXORG(GRCIDE)) / GRXSCL(GRCIDE)
          Y = (GRYPRE(GRCIDE) - GRYORG(GRCIDE)) / GRYSCL(GRCIDE)
      END IF
      END
C*GRQTXT -- get text bounding box
C+
      SUBROUTINE GRQTXT (ORIENT,X0,Y0,STRING, XBOX, YBOX)
C
C GRPCKG: get the bounding box of a string drawn by GRTEXT.
C--
C 12-Sep-1993 - [TJP].
C  8-Nov-1994 - return something even if string is blank [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL UNUSED, VISBLE, PLOT
      INTEGER XYGRID(300)
      INTEGER LIST(256)
      CHARACTER*(*) STRING
      REAL XBOX(4), YBOX(4)
      REAL ANGLE, FACTOR, FNTBAS, FNTFAC, COSA, SINA, DX, DY, XORG, YORG
      REAL ORIENT, RATIO, X0, Y0, RLX, RLY
      REAL XG, YG, XGMIN, XGMAX, YGMIN, YGMAX
      INTEGER I, IFNTLV,NLIST,LX,LY, K, LXLAST,LYLAST
      INTRINSIC ABS, COS, LEN, MAX, MIN, SIN
C
C Default return values.
C
      DO 10 I=1,4
         XBOX(I) = X0
         YBOX(I) = Y0
 10   CONTINUE
C
C Check that there is something to be plotted.
C
      IF (LEN(STRING).LE.0) RETURN
C
C Check that a device is selected.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQTXT - no graphics device is active.')
          RETURN
      END IF
C
      XORG = GRXPRE(GRCIDE)
      YORG = GRYPRE(GRCIDE)
C
C Compute scaling and orientation.
C
      ANGLE = ORIENT*(3.14159265359/180.)
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
      XORG = X0
      YORG = Y0
C
C Convert the string to a list of symbol numbers; to prevent overflow
C of array LIST, the length of STRING is limited to 256 characters.
C
      CALL GRSYDS(LIST,NLIST,STRING(1:MIN(256,LEN(STRING))),
     1             GRCFNT(GRCIDE))
C
C Run through the string of characters, getting bounding box
C in character coordinates. (XG, YG) is the starting point
C of the current character. The x/y limits of the bbox are
C XGMIN...XGMAX, YGMIN...YGMAX.
C
      FNTBAS = 0.0
      FNTFAC = 1.0
      IFNTLV = 0
      DX = 0.0
      DY = 0.0
      XG = 0.0
      YG = 0.0
      XGMIN = 1E30
      XGMAX = -1E30
      YGMIN = 1E30
      YGMAX = -1E30
      PLOT  = .FALSE.
      DO 380 I=1,NLIST
          IF (LIST(I).LT.0) THEN
              IF (LIST(I).EQ.-1) THEN
C                 ! up
                  IFNTLV = IFNTLV+1
                  FNTBAS = FNTBAS + 16.0*FNTFAC
                  FNTFAC = 0.75**ABS(IFNTLV)
              ELSE IF (LIST(I).EQ.-2) THEN
C                 ! down
                  IFNTLV = IFNTLV-1
                  FNTFAC = 0.75**ABS(IFNTLV)
                  FNTBAS = FNTBAS - 16.0*FNTFAC
              ELSE IF (LIST(I).EQ.-3) THEN
C                 ! backspace
                  XG = XG - DX*FNTFAC
              END IF
              GOTO 380
          END IF
          CALL GRSYXD(LIST(I),XYGRID,UNUSED)
          VISBLE = .FALSE.
          DX = XYGRID(5)-XYGRID(4)
          K = 4
          LXLAST = -64
          LYLAST = -64
  320     K = K+2
          LX = XYGRID(K)
          LY = XYGRID(K+1)
          IF (LY.EQ.-64) GOTO 330
          IF (LX.EQ.-64) THEN
              VISBLE = .FALSE.
          ELSE
              RLX = (LX - XYGRID(4))*FNTFAC
              RLY = (LY - XYGRID(2))*FNTFAC + FNTBAS
              IF ((LX.NE.LXLAST) .OR. (LY.NE.LYLAST)) THEN
                  XGMIN = MIN(XGMIN,XG+RLX)
                  XGMAX = MAX(XGMAX,XG+RLX)
                  YGMIN = MIN(YGMIN,RLY)
                  YGMAX = MAX(YGMAX,RLY)
                  PLOT = .TRUE.
              END IF
              VISBLE = .TRUE.
              LXLAST = LX
              LYLAST = LY
          END IF
          GOTO 320
  330     XG = XG + DX*FNTFAC
  380 CONTINUE
C
C Check whether anything was plotted.
C
      IF (.NOT.PLOT) RETURN
C
C Expand the box a bit to allow for line-width.
C
      XGMIN = XGMIN - 5.0
      XGMAX = XGMAX + 5.0
      YGMIN = YGMIN - 4.0
      YGMAX = YGMAX + 4.0
C
C Convert bounding box to device coordinates.
C
C     WRITE (*,*) XGMIN, XGMAX, YGMIN, YGMAX
      XBOX(1) = XORG + (COSA*XGMIN - SINA*YGMIN)*RATIO
      YBOX(1) = YORG + (SINA*XGMIN + COSA*YGMIN)
      XBOX(2) = XORG + (COSA*XGMIN - SINA*YGMAX)*RATIO
      YBOX(2) = YORG + (SINA*XGMIN + COSA*YGMAX)
      XBOX(3) = XORG + (COSA*XGMAX - SINA*YGMAX)*RATIO
      YBOX(3) = YORG + (SINA*XGMAX + COSA*YGMAX)
      XBOX(4) = XORG + (COSA*XGMAX - SINA*YGMIN)*RATIO
      YBOX(4) = YORG + (SINA*XGMAX + COSA*YGMIN)
C
      END
C*GRQTYP -- inquire current device type
C+
      SUBROUTINE GRQTYP (TYPE,INTER)
      CHARACTER*(*) TYPE
      LOGICAL INTER
C
C GRPCKG: obtain the device type of the currently selected graphics
C device, and determine whether or not it is an interactive device.
C
C Arguments:
C
C TYPE (output, CHARACTER*(*)): receives the device type, as a
C       character string, eg 'PRINTRONIX', 'TRILOG', 'VERSATEC',
C       'TEK4010', 'TEK4014', 'GRINNELL', or 'VT125'.  The character
C       string should have a length of at least 8 to ensure that the
C       type is unique.
C INTER (output, LOGICAL): receives the value .TRUE. if the device is
C       interactive, .FALSE. otherwise.
C--
C (23-May-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C 18-Jan-1993 - return type only, not description [TJP].
C  1-Sep-1994 - get capabilities from common [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER*32 CHR
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQTYP - no graphics device is active.')
          TYPE = 'NULL'
          INTER = .FALSE.
      ELSE
          CALL GREXEC(GRGTYP, 1,RBUF,NBUF,CHR,LCHR)
          LCHR = INDEX(CHR,' ')
          TYPE = CHR(:LCHR)
          INTER = (GRGCAP(GRCIDE)(1:1).EQ.'I')
      END IF
C
      END
C*GRQUIT -- report a fatal error and abort execution
C+
      SUBROUTINE GRQUIT (TEXT)
      CHARACTER*(*) TEXT
C
C Report a fatal error (via GRWARN) and exit program.
C This routine should be called in the event of an unrecoverable 
C PGPLOT error.
C
C Argument:
C  TEXT (input): text of message to be sent to GRWARN.
C--
C 12-Nov-1994
C-----------------------------------------------------------------------
C
      CALL GRWARN(TEXT)
      CALL GRWARN('Fatal error in PGPLOT library: program terminating.')
      STOP 
      END
C*GRREC0 -- fill a rectangle (device coordinates)
C+
      SUBROUTINE GRREC0 (X0,Y0,X1,Y1)
      REAL X0, Y0, X1, Y1
C
C GRPCKG: Fill a rectangle with solid color.  The rectangle
C is defined by the (x,y) device coordinates of its lower left and
C upper right corners; the edges are parallel to the coordinate axes.
C X0 is guaranteed to be <= X1 and Y0 <= Y1. The rectangle possible
C extends beyond the clipping boundaries
C
C Arguments:
C
C X0, Y0 (input, real): device coordinates of one corner of the 
C       rectangle.
C X1, Y1 (input, real): device coordinates of the opposite corner of 
C       the rectangle.
C--
C 23-Mar-1988 - [TJP].
C 18-Jan-1991 - Code moved from GRRECT to GRREC0 so that it can also be
C               used by GRPXRE.
C  1-Sep-1994 - suppress driver call [TJP].
C  4-Dec-1995 - avoid use of real variable as do-loop index [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER*32 CHR
      REAL    XMIN, YMIN, XMAX, YMAX, Y, DY
      INTEGER LS, LW, I, NLINES
C
C Clip
C
      XMIN = X0
      XMAX = X1
      YMIN = Y0
      YMAX = Y1
      IF (XMIN .LT. GRXMIN(GRCIDE)) XMIN = GRXMIN(GRCIDE)
      IF (XMAX .GT. GRXMAX(GRCIDE)) XMAX = GRXMAX(GRCIDE)
      IF (YMIN .LT. GRYMIN(GRCIDE)) YMIN = GRYMIN(GRCIDE)
      IF (YMAX .GT. GRYMAX(GRCIDE)) YMAX = GRYMAX(GRCIDE)
      IF (XMIN .GT. XMAX) RETURN
      IF (YMIN .GT. YMAX) RETURN
C
C Use hardware rectangle fill if available.
C
      IF (GRGCAP(GRCIDE)(6:6).EQ.'R') THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          RBUF(1) = XMIN
          RBUF(2) = YMIN
          RBUF(3) = XMAX
          RBUF(4) = YMAX
          CALL GREXEC(GRGTYP,24,RBUF,NBUF,CHR,LCHR)
          RETURN
C
C Else use hardware polygon fill if available.
C
      ELSE IF (GRGCAP(GRCIDE)(4:4).EQ.'A') THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          RBUF(1) = 4
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMIN
          RBUF(2) = YMIN
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMAX
          RBUF(2) = YMIN
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMAX
          RBUF(2) = YMAX
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMIN
          RBUF(2) = YMAX
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RETURN
      END IF
C
C For other devices fill area is simulated.
C
C Save attributes.
C
      CALL GRQLS(LS)
      CALL GRQLW(LW)
      CALL GRSLS(1)
      CALL GRSLW(1)
      CALL GREXEC(GRGTYP, 3,RBUF,NBUF,CHR,LCHR)
      DY = RBUF(3)
C
C Draw horizontal raster lines.
C
      NLINES = ABS((YMAX-YMIN)/DY)
      Y = YMIN - DY/2.0
      DO 40 I=1,NLINES
         Y = Y + DY
         GRXPRE(GRCIDE) = XMIN
         GRYPRE(GRCIDE) = Y
         CALL GRLIN0(XMAX,Y)
   40 CONTINUE
C
C Restore attributes.
C
      CALL GRSLS(LS)
      CALL GRSLW(LW)
      END

C*GRRECT -- fill a rectangle
C+
      SUBROUTINE GRRECT (X0,Y0,X1,Y1)
      REAL X0, Y0, X1, Y1
C
C GRPCKG: Fill a rectangle with solid color.  The rectangle
C is defined by the (x,y) world coordinates of its lower left and upper 
C right corners; the edges are parallel to the coordinate axes.
C
C Arguments:
C
C X0, Y0 (input, real): world coordinates of one corner of the 
C       rectangle.
C X1, Y1 (input, real): world coordinates of the opposite corner of the 
C       rectangle.
C--
C 23-Mar-1988 - [TJP].
C 18-Jan-1991 - Code moved from GRRECT to GRREC0 so that it can also be
C               used by GRPXRE
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    XLL, YLL, XUR, YUR
      REAL    XMIN, YMIN, XMAX, YMAX
C
      IF (GRCIDE.LT.1) RETURN
C
C Convert to device coordinates and clip.
C
      CALL GRTXY0(.FALSE.,X0,Y0,XLL,YLL)
      CALL GRTXY0(.FALSE.,X1,Y1,XUR,YUR)
      XMIN = MIN(XLL,XUR)
      XMAX = MAX(XLL,XUR)
      YMIN = MIN(YLL,YUR)
      YMAX = MAX(YLL,YUR)
C
C Do the real work
C
      CALL GRREC0(XMIN,YMIN,XMAX,YMAX)
      END
C*GRSCI -- set color index
C+
      SUBROUTINE GRSCI (IC)
C
C GRPCKG: Set the color index for subsequent plotting. Calls to GRSCI
C are ignored for monochrome devices. The default color index is 1,
C usually white on a black background for video displays or black on a
C white background for printer plots. The color index is an integer in
C the range 0 to a device-dependent maximum. Color index 0 corresponds
C to the background color; lines may be "erased" by overwriting them
C with color index 0.
C
C Color indices 0-7 are predefined as follows: 0 = black (background
C color), 1 = white (default), 2 = red, 3 = green, 4 = blue, 5 = cyan
C (blue + green), 6 = magenta (red + blue), 7 = yellow (red + green).
C The assignment of colors to color indices can be changed with
C subroutine GRSCR (set color representation).
C
C Argument:
C
C IC (integer, input): the color index to be used for subsequent
C       plotting on the current device (in range 0-255). If the
C       index exceeds the device-dependent maximum, the result is
C       device-dependent.
C--
C 11-Apr-1983 - [TJP].
C  3-Jun-1984 - add GMFILE device [TJP].
C 13-Jun-1984 - add code for TK4100 devices [TJP].
C  2-Jul-1984 - add code for RETRO and VT125 (REGIS) devices [TJP].
C  2-Oct-1984 - change REGIS to improve VT240 behavior [TJP].
C 22-Dec-1984 - add PRTX, TRILOG, VERS and VV devices [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - delays setting color if picture not open [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C 31-May-1989 - add check for valid color index [TJP].
C  1-Sep-1994 - use common data [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IC, COLOR, IC1, IC2, NBUF,LCHR
      REAL     RBUF(6)
      CHARACTER*1 CHR
C
C Error if no workstation is open.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSCI - no graphics device is active.')
          RETURN
      END IF
C
C Use color index 1 if out of range.
C
      IC1 = GRMNCI(GRCIDE)
      IC2 = GRMXCI(GRCIDE)
      COLOR = IC
      IF (COLOR.LT.IC1 .OR. COLOR.GT.IC2) COLOR = 1
C
C If no change to color index is requested, take no action.
C
      IF (COLOR.EQ.GRCCOL(GRCIDE)) RETURN
C
C If the workstation is in "picture open" state, send command to
C driver.
C
      IF (GRPLTD(GRCIDE)) THEN
          RBUF(1) = COLOR
          CALL GREXEC(GRGTYP,15,RBUF,NBUF,CHR,LCHR)
      END IF
C
C Set the current color index.
C
      GRCCOL(GRCIDE)=COLOR
C
      END
C*GRSCR -- set color representation
C+
      SUBROUTINE GRSCR (CI, CR, CG, CB)
      INTEGER  CI
      REAL     CR, CG, CB
C
C GRPCKG: SET COLOUR REPRESENTATION -- define the colour to be
C associated with a colour index.  Ignored for devices which do not
C support variable colour or intensity.  On monochrome output
C devices (e.g. VT125 terminals with monochrome monitors), the
C monochrome intensity is computed from the specified Red, Green, Blue
C intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television
C systems, NTSC encoding.  Note that most devices do not have an
C infinite range of colours or monochrome intensities available;
C the nearest available colour is used.
C
C Arguments:
C
C CI (integer, input): colour index. If the colour index is outside the
C       range available on the device, the call is ignored. Colour
C       index 0 applies to the background colour.
C CR, CG, CB (real, input): red, green, and blue intensities,
C       in range 0.0 to 1.0.
C--
C 20-Feb-1984 - [TJP].
C  5-Jun-1984 - add GMFILE device [TJP].
C  2-Jul-1984 - add REGIS device [TJP].
C  2-Oct-1984 - change use of map tables in Regis [TJP].
C 11-Nov-1984 - add code for /TK [TJP].
C  1-Sep-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C 31-Aug-1994 - suppress call of begin picture [TJP].
C  1-Sep-1994 - use common data [TJP].
C 26-Jul-1995 - fix bug: some drivers would ignore a change to the
C               current color [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   NBUF, LCHR
      REAL      RBUF(6)
      CHARACTER CHR
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSCR - Specified workstation is not open.')
      ELSE IF (CR.LT.0.0 .OR. CG.LT.0.0 .OR. CB.LT.0.0 .OR.
     1    CR.GT.1.0 .OR. CG.GT.1.0 .OR. CB.GT.1.0) THEN
          CALL GRWARN('GRSCR - Colour is outside range [0,1].')
      ELSE IF (CI.GE.GRMNCI(GRCIDE) .AND. CI.LE.GRMXCI(GRCIDE)) THEN
C         IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          RBUF(1)=CI
          RBUF(2)=CR
          RBUF(3)=CG
          RBUF(4)=CB
          NBUF=4
          CALL GREXEC(GRGTYP,21,RBUF,NBUF,CHR,LCHR)
C         -- If this is the current color, reselect it in the driver.
          IF (CI.EQ.GRCCOL(GRCIDE)) THEN
             RBUF(1) = CI
             CALL GREXEC(GRGTYP,15,RBUF,NBUF,CHR,LCHR)
          END IF
      END IF
C
      END
C GRSCRL -- scroll pixels in viewport
C+
      SUBROUTINE GRSCRL (DX, DY)
      INTEGER DX, DY
C
C Shift the pixels in the viewport by DX and DY in device coordinates.
C--
C 24-Feb-97: new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER NBUF, LCHR
      REAL RBUF(6)
      CHARACTER*8 CHR
C
C Do nothing if device is not open or not in appropriate state.
C
      IF (GRCIDE.LT.1) RETURN
      IF (.NOT.GRPLTD(GRCIDE)) RETURN
C
C If device has scroll capability, use it. The arguments in
C RBUF are: (1..4) current viewport in device coordinates; 
C (5..6) scroll displacement in world coordinates.
C
      IF (GRGCAP(GRCIDE)(11:11).EQ.'S') THEN
         RBUF(1) = NINT(GRXMIN(GRCIDE))
         RBUF(2) = NINT(GRYMIN(GRCIDE))
         RBUF(3) = NINT(GRXMAX(GRCIDE))
         RBUF(4) = NINT(GRYMAX(GRCIDE))
         RBUF(5) = DX
         RBUF(6) = DY
         NBUF = 6
         LCHR = 0
         CALL GREXEC(GRGTYP,30,RBUF,NBUF,CHR,LCHR)
C
C Otherwise, report an error.
C
      ELSE
         CALL GRWARN('Device does not support scrolling')
      END IF
      END

C*GRSETC -- set character size
C+
      SUBROUTINE GRSETC (IDENT,XSIZE)
C
C GRPCKG : change the character size (user-callable routine).
C
C Input:   IDENT : plot identifier
C          XSIZE : the new character width. The character height
C                  and spacing will be scaled by the same factor.
C                  If XSIZE is negative or zero, the character size
C                  will be set to the default size.
C--
C (1-Feb-1983)
C 16-Sep-1985 - add code for metafile output (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER IDENT
      REAL XSIZE
C
C Record the new size (GRCFAC).
C
      CALL GRSLCT(IDENT)
      IF (XSIZE.LE.0.0) THEN
          GRCFAC(IDENT) = 1.0
      ELSE
          GRCFAC(IDENT) = XSIZE / GRCXSZ
      END IF
C
      END
C*GRSETFONT -- set text font [obsolete]
C
      SUBROUTINE GRSETFONT (IF)
      INTEGER IF
      CALL GRSFNT(IF)
      END
C*GRSETLI -- *obsolete routine*
C+
      SUBROUTINE GRSETLI (IN)
C
C GRPCKG: Set the line intensity for subsequent plotting on the current
C device. *** OBSOLETE ROUTINE *** Intensity is now set with GRSCI
C and GRSCR. For compatibility, GRSETLI now sets color zero if its
C argument is 0, and resets the previous color if its argument is
C non-zero.
C
C Argument:
C
C IN (integer, input): the intensity to be used for subsequent
C       plotting on the current device (in range 0-3).
C--
C 11-Apr-1983 - [TJP].
C 12-Jul-1984 - modify to call GRSCI [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IN, OLDCOL(GRIMAX)
      DATA     OLDCOL /GRIMAX*1/
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSETLI - no graphics device is active.')
      ELSE IF (IN.EQ.0) THEN
          OLDCOL(GRCIDE) = GRCCOL(GRCIDE)
          CALL GRSCI(0)
      ELSE
          CALL GRSCI(OLDCOL(GRCIDE))
      END IF
      END

C*GRSETPEN -- *obsolete routine*
C+
      SUBROUTINE GRSETPEN
C
C GRPCKG: Set the pen number for subsequent plotting.  Obsolete
C routine: ignored.
C-----------------------------------------------------------------------
      CALL GRWARN('GRSETPEN is an obsolete routine.')
      END
C*GRSETS -- change size of view surface
C+
      SUBROUTINE GRSETS (IDENT,XSIZE,YSIZE)
C
C GRPCKG : change size of plotting area. The requested dimensions
C will be reduced to the absolute maximum of the plot device if
C necessary.
C
C Arguments:
C
C IDENT (input, integer): plot identifier from GROPEN.
C XSIZE (input, real): new x dimension of plot area (absolute
C               units); if less than zero, the default dimension
C               will be used.
C YSIZE (input, real): new y dimension of plot area (absolute
C               units); if less than zero, the default dimension
C               will be used.
C--
C (1-Feb-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C  5-Jan-1993 - set GRADJU [TJP].
C------------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, IDENT, J, IX, IY, NBUF,LCHR
      REAL     RBUF(6)
      CHARACTER CHR
      REAL     XSIZE,YSIZE
C
      CALL GRSLCT(IDENT)
C     write (*,*) 'GRSETS: old size', GRXMXA(IDENT), GRYMXA(IDENT)
      CALL GRPAGE
      IF ((XSIZE .LT. 0.0) .OR. (YSIZE .LT. 0.0)) THEN
          CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
          GRXMXA(IDENT) = RBUF(2)
          GRYMXA(IDENT) = RBUF(4)
      ELSE
          I = NINT(XSIZE)
          J = NINT(YSIZE)
          CALL GREXEC(GRGTYP, 2,RBUF,NBUF,CHR,LCHR)
          IX=RBUF(2)
          IY=RBUF(4)
          IF (IX.GT.0) I = MIN(I,IX)
          IF (IY.GT.0) J = MIN(J,IY)
          GRXMXA(IDENT) = I
          GRYMXA(IDENT) = J
      END IF
C     write (*,*) 'GRSETS: new size', GRXMXA(IDENT), GRYMXA(IDENT)
      GRXMIN(IDENT) = 0
      GRXMAX(IDENT) = GRXMXA(IDENT)
      GRYMIN(IDENT) = 0
      GRYMAX(IDENT) = GRYMXA(IDENT)
      GRADJU(IDENT) = .TRUE.
C
      END
C*GRSFNT -- set text font
C+
      SUBROUTINE GRSFNT (IF)
      INTEGER IF
C
C GRPCKG: Set the font for subsequent text plotting.
C The default font is 1 ("Normal" font); others available are 2
C ("Roman"), 3 ("Italic"), and 4 ("Script").
C
C Argument:
C  IF (input): the font number to be used for subsequent
C       text plotting on the current device (in range 1-4).
C--
C 19-Mar-1983 - [TJP].
C  4-Jun-1984 - add code for GMFILE device [TJP].
C 15-Dec-1988 - change name [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER    I
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSFNT - no graphics device is active.')
          RETURN
      END IF
C
C Set software font index.
C
      IF (IF.LT.1 .OR. IF.GT.4) THEN
          CALL GRWARN('Illegal font selected: font 1 used.')
          I = 1
      ELSE
          I = IF
      END IF
C
C Ignore request if no change is to be made.
C
      IF (IF.EQ.GRCFNT(GRCIDE)) RETURN
C
C Save font setting.
C
      GRCFNT(GRCIDE) = I
C
      END

C*GRSIZE -- inquire device size and resolution
C+
      SUBROUTINE GRSIZE (IDENT,XSZDEF,YSZDEF,XSZMAX,YSZMAX,
     1                   XPERIN,YPERIN)
C
C GRPCKG : obtain device parameters (user-callable routine).
C--
C (1-Feb-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER IDENT
      REAL XSZDEF, YSZDEF, XSZMAX, YSZMAX, XPERIN, YPERIN
      INTEGER NBUF,LCHR
      REAL    RBUF(6)
      CHARACTER CHR
C
      CALL GRSLCT(IDENT)
      CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
      XSZDEF = RBUF(2)
      YSZDEF = RBUF(4)
      CALL GREXEC(GRGTYP, 2,RBUF,NBUF,CHR,LCHR)
      XSZMAX = RBUF(2)
      YSZMAX = RBUF(4)
      XPERIN = GRPXPI(GRCIDE)
      YPERIN = GRPYPI(GRCIDE)
C
      END
C*GRSKPB -- skip blanks in character string
C+
      SUBROUTINE GRSKPB (S, I)
      CHARACTER*(*) S
      INTEGER I
C
C GRSKPB: increment I so that it points to the next non-blank
C character in string S.  'Blank' characters are space and tab (ASCII 
C character value 9).
C
C Arguments:
C  S      (input)  : character string to be parsed.
C  I      (in/out) : on input, I is the index of the first character
C                    in S to be examined; on output, either it points
C                    to the next non-blank character, or it is equal
C                    to LEN(S)+1 (if all the rest of the string is 
C                    blank).
C--
C  1985 Oct 8 - New routine, based on SKIPBL (T. J. Pearson).
C-----------------------------------------------------------------------
C
   10 IF (I.GT.LEN(S)) RETURN
      IF (S(I:I).NE.' ' .AND. S(I:I).NE.CHAR(9)) RETURN
      I = I+1
      GOTO 10
      END

C*GRSLCT -- select active output device
C+
      SUBROUTINE GRSLCT (IDENT)
C
C GRPCKG: Check that IDENT is a valid plot identifier, and select the
C corresponding plot as the current plot. All subsequent plotting will
C be directed to this device until the assignment is changed by another
C call to GRSLCT.
C
C Argument:
C
C IDENT (input, integer): the identifier of the plot to be selected, as
C       returned by GROPEN.
C--
C (1-Feb-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C  4-Jun-1987 - skip action if no change in ID [TJP].
C 26-Nov-1990 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     RBUF(6)
      INTEGER  IDENT, NBUF,LCHR
      CHARACTER CHR
C
      IF ((IDENT.LE.0) .OR. (IDENT.GT.GRIMAX) .OR.
     1    (GRSTAT(IDENT).EQ.0)) THEN
            CALL GRWARN('GRSLCT - invalid plot identifier.')
      ELSE IF (IDENT.EQ.GRCIDE) THEN
          RETURN
      ELSE
          GRCIDE = IDENT
          GRGTYP = GRTYPE(IDENT)
          RBUF(1)= GRCIDE
          RBUF(2)= GRUNIT(GRCIDE)
          NBUF   = 2
          CALL GREXEC(GRGTYP, 8,RBUF,NBUF,CHR,LCHR)
      END IF
      END
C*GRSLS -- set line style
C+
      SUBROUTINE GRSLS (IS)
      INTEGER IS
C
C GRPCKG: Set the line style for subsequent plotting on the current
C device. The different line styles are generated in hardware on
C some devices and by GRPCKG software for the other devices. Five
C different line styles are available, with the following codes:
C 1 (full line), 2 (dashed), 3 (dot-dash-dot-dash), 4 (dotted),
C 5 (dash-dot-dot-dot). The default is 1 (normal full line). Line
C style is ignored when drawing characters, which are always drawn with
C a full line.
C
C Argument:
C
C IS (input, integer): the line-style code for subsequent plotting on
C       the current device (in range 1-5).
C--
C  9-Feb-1983 - [TJP].
C  3-Jun-1984 - add GMFILE device [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C 19-Jan-1987 - fix bug in GREXEC call [TJP].
C 16-May-1989 - fix bug for hardware line dash [TJP].
C  1-Sep-1994 - do not call driver to get size and capabilities [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I, L, IDASH, NBUF,LCHR
      REAL    RBUF(6),TMP
      CHARACTER*10 CHR
      REAL PATERN(8,5)
C
      DATA PATERN/ 8*10.0,
     1             8*10.0,
     2             8.0, 6.0, 1.0, 6.0, 8.0, 6.0, 1.0, 6.0,
     3             1.0, 6.0, 1.0, 6.0, 1.0, 6.0, 1.0, 6.0,
     4             8.0, 6.0, 1.0, 6.0, 1.0, 6.0, 1.0, 6.0 /
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSLS - no graphics device is active.')
          RETURN
      END IF
C
      I = IS
      IF (I.LT.1 .OR. I.GT.5) THEN
          CALL GRWARN('GRSLS - invalid line-style requested.')
          I = 1
      END IF
C
C Inquire if hardware dash is available.
C
      IDASH=0
      IF(GRGCAP(GRCIDE)(3:3).EQ.'D') IDASH=1
C
C Set up for hardware dash.
C
      IF(IDASH.NE.0) THEN
          GRDASH(GRCIDE) = .FALSE.
          IF (GRPLTD(GRCIDE)) THEN
              RBUF(1)=I
              NBUF=1
              CALL GREXEC(GRGTYP,19,RBUF,NBUF,CHR,LCHR)
          END IF
C
C Set up for software dash.
C
      ELSE
          IF (I.EQ.1) THEN
              GRDASH(GRCIDE) = .FALSE.
          ELSE
              GRDASH(GRCIDE) = .TRUE.
              GRIPAT(GRCIDE) = 1
              GRPOFF(GRCIDE) = 0.0
              TMP = GRYMXA(GRCIDE)/1000.
              DO 10 L=1,8
                  GRPATN(GRCIDE,L) = PATERN(L,I)*TMP
   10         CONTINUE
          END IF
      END IF
      GRSTYL(GRCIDE) = I
      END
C*GRSLW -- set line width
C+
      SUBROUTINE GRSLW (IW)
      INTEGER IW
C
C GRPCKG: Set the line width for subsequent plotting on the current
C device. If the hardware does not support thick lines, they are
C simulated by tracing each line with multiple strokes offset in the
C direction perpendicular to the line. The line width is specified by
C the number of strokes to be used, which must be in the range 1-201.
C The actual line width obtained depends on the device resolution.
C If the hardware does support thick lines, the width of the line
C is approximately 0.005 inches times the value of argument IW.
C
C Argument:
C
C IW (integer, input): the number of strokes to be used for subsequent
C       plotting on the current device (in range 1-201).
C--
C  1-Feb-1983 [TJP].
C  3-Jun-1984 [TJP] - add GMFILE device.
C 28-Aug-1984 [TJP] - correct bug in GMFILE: redundant SET_LINEWIDTH
C                     commands were not being filtered out.
C 26-May-1987 [TJP] - add GREXEC support.
C 11-Jun-1987 [TJP] - remove built-in devices.
C 31-May-1989 [TJP] - increase maximum width from 21 to 201.
C  1-Sep-1994 [TJP] 
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I, ITHICK
      REAL    RBUF(1)
      INTEGER NBUF,LCHR
      CHARACTER*32 CHR
C
C Check that graphics is active.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSLW - no graphics device is active.')
          RETURN
      END IF
C
C Check that requested line-width is valid.
C
      I = IW
      IF (I.LT.1 .OR. I.GT.201) THEN
          CALL GRWARN('GRSLW - invalid line-width requested.')
          I = 1
      END IF
C
C Ignore the request if the linewidth is unchanged.
C
      IF (I.EQ.ABS(GRWIDT(GRCIDE))) RETURN
C
C Inquire if hardware supports thick lines.
C
      ITHICK = 0
      IF (GRGCAP(GRCIDE)(5:5).EQ.'T') ITHICK = 1
C
C For devices with hardware support of thick lines, send the
C appropriate commands to the device driver, and give the "current
C linewidth" parameter a negative value to suppress software linewidth
C emulation.
C
      IF (ITHICK.EQ.1 .AND. GRPLTD(GRCIDE)) THEN
          RBUF(1) = I
          CALL GREXEC(GRGTYP,22,RBUF,NBUF,CHR,LCHR)
      END IF
C
C Save the current linewidth.
C
      GRWIDT(GRCIDE) = I
      IF (ITHICK.EQ.1) GRWIDT(GRCIDE) = -I
C
      END
C*GRSYDS -- decode character string into list of symbol numbers
C+
      SUBROUTINE GRSYDS (SYMBOL, NSYMBS, TEXT, FONT)
      INTEGER SYMBOL(*), NSYMBS, FONT
      CHARACTER*(*) TEXT
C
C Given a character string, this routine returns a list of symbol
C numbers to be used to plot it. It is responsible for interpreting
C all escape sequences.  Negative `symbol numbers' are inserted in the
C list to represent pen movement. The following escape sequences are
C defined (the letter following the \ may be either upper or lower 
C case):
C
C \u       :      up one level (returns -1)
C \d       :      down one level (returns -2)
C \b       :      backspace (returns -3)
C \A       :      (upper case only) Angstrom symbol, roman font
C \x       :      multiplication sign
C \.       :      centered dot
C \\       :      \, returns the code for backslash
C \gx      :      greek letter corresponding to roman letter x
C \fn      :      switch to Normal font
C \fr      :      switch to Roman font
C \fi      :      switch to Italic font
C \fs      :      switch to Script font
C \mn or \mnn :   graph marker number n or nn (1 or 2 digits)
C \(nnn)   :      Hershey symbol number nnn (any number of digits)
C
C Arguments:
C  SYMBOL (output) : receives the list of symbol numers.
C  NSYMBS (output) : receives the actual number of symbols specified
C                    by the string; it is assumed that the dimension of
C                    SYMBOL is big enough (not less than LEN(TEXT)).
C  TEXT   (input)  : the text string to be decoded.
C  FONT   (input)  : the font number (1..4) to be used for decoding the
C                    string (this can be overridden by an escape
C                    sequence within the string).
C--
C  3-May-1983 - [TJP].
C 13-Jun-1984 - add \A [TJP].
C 15-Dec-1988 - standardize [TJP].
C 29-Nov-1990 - add \m escapes [TJP].
C 27-Nov-1991 - add \x escape [TJP].
C 27-Jul-1995 - extend for 256-character set [TJP]
C  7-Nov-1995 - add \. escape [TJP].
C-----------------------------------------------------------------------
      CHARACTER*8  FONTS
      CHARACTER*48 GREEK
      PARAMETER (FONTS = 'nrisNRIS')
      PARAMETER (GREEK = 'ABGDEZYHIKLMNCOPRSTUFXQW' //
     1                   'abgdezyhiklmncoprstufxqw' )
      INTEGER  CH, IG, J, LENTXT, IFONT, MARK
C
C Initialize parameters.
C
      IFONT = FONT
      LENTXT = LEN(TEXT)
      NSYMBS = 0
      J = 0
C
C Get next character; treat non-printing characters as spaces.
C
  100 J = J+1
      IF (J.GT.LENTXT) RETURN
      CH = ICHAR(TEXT(J:J))
      IF (CH.LT.0)   CH = 32
      IF (CH.GT.303) CH = 32
C
C Test for escape sequence (\)
C
      IF (CH.EQ.92) THEN
          IF ((LENTXT-J).GE.1) THEN
            IF (TEXT(J+1:J+1).EQ.CHAR(92)) THEN
                J = J+1
            ELSE IF (TEXT(J+1:J+1).EQ.'u' .OR.
     1                     TEXT(J+1:J+1).EQ.'U') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = -1
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'d' .OR.
     1                     TEXT(J+1:J+1).EQ.'D') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = -2
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'b' .OR.
     1                     TEXT(J+1:J+1).EQ.'B') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = -3
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'A') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 2078
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'x') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 2235
                IF (IFONT.EQ.1) SYMBOL(NSYMBS) = 727
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'.') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 2236
                IF (IFONT.EQ.1) SYMBOL(NSYMBS) = 729
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'(') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 0
                J = J+2
C               -- DO WHILE ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9')
   90           IF ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9') THEN
                  SYMBOL(NSYMBS) = SYMBOL(NSYMBS)*10 +
     1                      ICHAR(TEXT(J:J)) - ICHAR('0')
                   J = J+1
                GOTO 90
                END IF
C               -- end DO WHILE
                IF (TEXT(J:J).NE.')') J = J-1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'m' .OR.
     1               TEXT(J+1:J+1).EQ.'M') THEN
                MARK = 0
                J = J+2
                IF ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9') THEN
                    MARK = MARK*10 + ICHAR(TEXT(J:J)) - ICHAR('0')
                    J = J+1
                END IF
                IF ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9') THEN
                    MARK = MARK*10 + ICHAR(TEXT(J:J)) - ICHAR('0')
                    J = J+1
                END IF
                J = J-1
                NSYMBS = NSYMBS + 1
                CALL GRSYMK(MARK, IFONT, SYMBOL(NSYMBS))
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'f' .OR.
     1               TEXT(J+1:J+1).EQ.'F') THEN
                IFONT = INDEX(FONTS, TEXT(J+2:J+2))
                IF (IFONT.GT.4) IFONT = IFONT-4
                IF (IFONT.EQ.0) IFONT = 1
                J = J+2
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'g' .OR.
     1               TEXT(J+1:J+1).EQ.'G') THEN
                IG = INDEX(GREEK, TEXT(J+2:J+2))
                NSYMBS = NSYMBS + 1
                CALL GRSYMK(255+IG, IFONT, SYMBOL(NSYMBS))
                J = J+2
                GOTO 100
            END IF
          END IF
      END IF
C
C Decode character.
C
      NSYMBS = NSYMBS + 1
      CALL GRSYMK(CH, IFONT, SYMBOL(NSYMBS))
      GOTO 100
      END
C*GRSYMK -- convert character number into symbol number
C+
      SUBROUTINE GRSYMK (CODE, FONT, SYMBOL)
      INTEGER CODE, FONT, SYMBOL
C
C This routine returns the Hershey symbol number (SYMBOL) corresponding
C to ASCII code CODE in font FONT.
C
C Characters 0-31 are the same in all fonts, and are the standard
C graph markers. Characters 32-127 are standard representations of
C the ASCII codes. Characters 128-255 are reserved for the upper
C half of the ISO Latin-1 character set. Characters 256-303 are
C used for the greek alphabet.
C
C Arguments:
C  CODE   (input)  : the extended ASCII code number.
C  FONT   (input)  : the font to be used 31 (range 1-4).
C  SYMBOL (output) : the number of the symbol to be plotted.
C--
C 24-Apr-1986.
C 15-Dec-1988 - standardize [TJP].
C 29-Nov-1990 - eliminate common block [TJP].
C 27-Nov-1991 - correct code for backslash [TJP].
C 27-Jul-1995 - extend for 256-character set; add some defaults for
C               ISO Latin-1 (full glyph set not available) [TJP].
C-----------------------------------------------------------------------
      INTEGER   I, K, HERSH(0:303,4)
      SAVE      HERSH
C
C Special characters (graph markers).
C
      DATA (HERSH(  0,K),K=1,4) / 841, 841, 841, 841/
      DATA (HERSH(  1,K),K=1,4) / 899, 899, 899, 899/
      DATA (HERSH(  2,K),K=1,4) / 845, 845, 845, 845/
      DATA (HERSH(  3,K),K=1,4) / 847, 847, 847, 847/
      DATA (HERSH(  4,K),K=1,4) / 840, 840, 840, 840/
      DATA (HERSH(  5,K),K=1,4) / 846, 846, 846, 846/
      DATA (HERSH(  6,K),K=1,4) / 841, 841, 841, 841/
      DATA (HERSH(  7,K),K=1,4) / 842, 842, 842, 842/
      DATA (HERSH(  8,K),K=1,4) /2284,2284,2284,2284/
      DATA (HERSH(  9,K),K=1,4) /2281,2281,2281,2281/
      DATA (HERSH( 10,K),K=1,4) / 735, 735, 735, 735/
      DATA (HERSH( 11,K),K=1,4) / 843, 843, 843, 843/
      DATA (HERSH( 12,K),K=1,4) / 844, 844, 844, 844/
      DATA (HERSH( 13,K),K=1,4) / 852, 852, 852, 852/
      DATA (HERSH( 14,K),K=1,4) / 866, 866, 866, 866/
      DATA (HERSH( 15,K),K=1,4) / 868, 868, 868, 868/
      DATA (HERSH( 16,K),K=1,4) / 851, 851, 851, 851/
      DATA (HERSH( 17,K),K=1,4) / 850, 850, 850, 850/
      DATA (HERSH( 18,K),K=1,4) / 856, 856, 856, 856/
      DATA (HERSH( 19,K),K=1,4) / 254, 254, 254, 254/
      DATA (HERSH( 20,K),K=1,4) / 900, 900, 900, 900/
      DATA (HERSH( 21,K),K=1,4) / 901, 901, 901, 901/
      DATA (HERSH( 22,K),K=1,4) / 902, 902, 902, 902/
      DATA (HERSH( 23,K),K=1,4) / 903, 903, 903, 903/
      DATA (HERSH( 24,K),K=1,4) / 904, 904, 904, 904/
      DATA (HERSH( 25,K),K=1,4) / 905, 905, 905, 905/
      DATA (HERSH( 26,K),K=1,4) / 906, 906, 906, 906/
      DATA (HERSH( 27,K),K=1,4) / 907, 907, 907, 907/
      DATA (HERSH( 28,K),K=1,4) /2263,2263,2263,2263/
      DATA (HERSH( 29,K),K=1,4) /2261,2261,2261,2261/
      DATA (HERSH( 30,K),K=1,4) /2262,2262,2262,2262/
      DATA (HERSH( 31,K),K=1,4) /2264,2264,2264,2264/
C
C US-ASCII (ISO Latin-1 lower half).
C
C   32:39 space exclam quotdbl numbersign
C         dollar percent ampersand quoteright
      DATA (HERSH( 32,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH( 33,K),K=1,4) / 714,2214,2764,2764/
      DATA (HERSH( 34,K),K=1,4) / 717,2217,2778,2778/
      DATA (HERSH( 35,K),K=1,4) / 733,2275,2275,2275/
      DATA (HERSH( 36,K),K=1,4) / 719,2274,2769,2769/
      DATA (HERSH( 37,K),K=1,4) /2271,2271,2271,2271/
      DATA (HERSH( 38,K),K=1,4) / 734,2272,2768,2768/
      DATA (HERSH( 39,K),K=1,4) / 716,2216,2777,2777/
C   40:47 parenleft parenright asterisk plus
C         comma minus period slash
      DATA (HERSH( 40,K),K=1,4) / 721,2221,2771,2771/
      DATA (HERSH( 41,K),K=1,4) / 722,2222,2772,2772/
      DATA (HERSH( 42,K),K=1,4) / 728,2219,2773,2773/
      DATA (HERSH( 43,K),K=1,4) / 725,2232,2775,2775/
      DATA (HERSH( 44,K),K=1,4) / 711,2211,2761,2761/
      DATA (HERSH( 45,K),K=1,4) / 724,2231,2774,2774/
      DATA (HERSH( 46,K),K=1,4) / 710,2210,2760,2760/
      DATA (HERSH( 47,K),K=1,4) / 720,2220,2770,2770/
C   48:55 zero one two three four five six seven
      DATA (HERSH( 48,K),K=1,4) / 700,2200,2750,2750/
      DATA (HERSH( 49,K),K=1,4) / 701,2201,2751,2751/
      DATA (HERSH( 50,K),K=1,4) / 702,2202,2752,2752/
      DATA (HERSH( 51,K),K=1,4) / 703,2203,2753,2753/
      DATA (HERSH( 52,K),K=1,4) / 704,2204,2754,2754/
      DATA (HERSH( 53,K),K=1,4) / 705,2205,2755,2755/
      DATA (HERSH( 54,K),K=1,4) / 706,2206,2756,2756/
      DATA (HERSH( 55,K),K=1,4) / 707,2207,2757,2757/
C   56:63 eight nine colon semicolon less equal greater question
      DATA (HERSH( 56,K),K=1,4) / 708,2208,2758,2758/
      DATA (HERSH( 57,K),K=1,4) / 709,2209,2759,2759/
      DATA (HERSH( 58,K),K=1,4) / 712,2212,2762,2762/
      DATA (HERSH( 59,K),K=1,4) / 713,2213,2763,2763/
      DATA (HERSH( 60,K),K=1,4) /2241,2241,2241,2241/
      DATA (HERSH( 61,K),K=1,4) / 726,2238,2776,2776/
      DATA (HERSH( 62,K),K=1,4) /2242,2242,2242,2242/
      DATA (HERSH( 63,K),K=1,4) / 715,2215,2765,2765/
C   64:71 at A B C D E F G
      DATA (HERSH( 64,K),K=1,4) /2273,2273,2273,2273/
      DATA (HERSH( 65,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH( 66,K),K=1,4) / 502,2002,2052,2552/
      DATA (HERSH( 67,K),K=1,4) / 503,2003,2053,2553/
      DATA (HERSH( 68,K),K=1,4) / 504,2004,2054,2554/
      DATA (HERSH( 69,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH( 70,K),K=1,4) / 506,2006,2056,2556/
      DATA (HERSH( 71,K),K=1,4) / 507,2007,2057,2557/
C   72:79 H I J K L M N O
      DATA (HERSH( 72,K),K=1,4) / 508,2008,2058,2558/
      DATA (HERSH( 73,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH( 74,K),K=1,4) / 510,2010,2060,2560/
      DATA (HERSH( 75,K),K=1,4) / 511,2011,2061,2561/
      DATA (HERSH( 76,K),K=1,4) / 512,2012,2062,2562/
      DATA (HERSH( 77,K),K=1,4) / 513,2013,2063,2563/
      DATA (HERSH( 78,K),K=1,4) / 514,2014,2064,2564/
      DATA (HERSH( 79,K),K=1,4) / 515,2015,2065,2565/
C   80:87 P Q R S T U V W
      DATA (HERSH( 80,K),K=1,4) / 516,2016,2066,2566/
      DATA (HERSH( 81,K),K=1,4) / 517,2017,2067,2567/
      DATA (HERSH( 82,K),K=1,4) / 518,2018,2068,2568/
      DATA (HERSH( 83,K),K=1,4) / 519,2019,2069,2569/
      DATA (HERSH( 84,K),K=1,4) / 520,2020,2070,2570/
      DATA (HERSH( 85,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH( 86,K),K=1,4) / 522,2022,2072,2572/
      DATA (HERSH( 87,K),K=1,4) / 523,2023,2073,2573/
C   88:95 X Y Z bracketleft 
C         backslash bracketright asciicircum underscore
      DATA (HERSH( 88,K),K=1,4) / 524,2024,2074,2574/
      DATA (HERSH( 89,K),K=1,4) / 525,2025,2075,2575/
      DATA (HERSH( 90,K),K=1,4) / 526,2026,2076,2576/
      DATA (HERSH( 91,K),K=1,4) /2223,2223,2223,2223/
      DATA (HERSH( 92,K),K=1,4) / 804, 804, 804, 804/
      DATA (HERSH( 93,K),K=1,4) /2224,2224,2224,2224/
      DATA (HERSH( 94,K),K=1,4) / 718,2218,2779,2779/
      DATA (HERSH( 95,K),K=1,4) / 590, 590, 590, 590/
C   96:103 quoteleft a b c d e f g
      DATA (HERSH( 96,K),K=1,4) /2249,2249,2249,2249/
      DATA (HERSH( 97,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH( 98,K),K=1,4) / 602,2102,2152,2652/
      DATA (HERSH( 99,K),K=1,4) / 603,2103,2153,2653/
      DATA (HERSH(100,K),K=1,4) / 604,2104,2154,2654/
      DATA (HERSH(101,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(102,K),K=1,4) / 606,2106,2156,2656/
      DATA (HERSH(103,K),K=1,4) / 607,2107,2157,2657/
C  104:111 h i j k l m n o
      DATA (HERSH(104,K),K=1,4) / 608,2108,2158,2658/
      DATA (HERSH(105,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(106,K),K=1,4) / 610,2110,2160,2660/
      DATA (HERSH(107,K),K=1,4) / 611,2111,2161,2661/
      DATA (HERSH(108,K),K=1,4) / 612,2112,2162,2662/
      DATA (HERSH(109,K),K=1,4) / 613,2113,2163,2663/
      DATA (HERSH(110,K),K=1,4) / 614,2114,2164,2664/
      DATA (HERSH(111,K),K=1,4) / 615,2115,2165,2665/
C  112:119 p q r s t u v w
      DATA (HERSH(112,K),K=1,4) / 616,2116,2166,2666/
      DATA (HERSH(113,K),K=1,4) / 617,2117,2167,2667/
      DATA (HERSH(114,K),K=1,4) / 618,2118,2168,2668/
      DATA (HERSH(115,K),K=1,4) / 619,2119,2169,2669/
      DATA (HERSH(116,K),K=1,4) / 620,2120,2170,2670/
      DATA (HERSH(117,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(118,K),K=1,4) / 622,2122,2172,2672/
      DATA (HERSH(119,K),K=1,4) / 623,2123,2173,2673/
C  120:127 x y z braceleft bar braceright asciitilde -
      DATA (HERSH(120,K),K=1,4) / 624,2124,2174,2674/
      DATA (HERSH(121,K),K=1,4) / 625,2125,2175,2675/
      DATA (HERSH(122,K),K=1,4) / 626,2126,2176,2676/
      DATA (HERSH(123,K),K=1,4) /2225,2225,2225,2225/
      DATA (HERSH(124,K),K=1,4) / 723,2229,2229,2229/
      DATA (HERSH(125,K),K=1,4) /2226,2226,2226,2226/
      DATA (HERSH(126,K),K=1,4) /2246,2246,2246,2246/
      DATA (HERSH(127,K),K=1,4) / 699,2199,2199,2199/
C
C ISO Latin-1 upper half.
C
C  128:135 - - - - - - - -
      DATA (HERSH(128,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(129,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(130,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(131,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(132,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(133,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(134,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(135,K),K=1,4) / 699,2199,2199,2199/
C  136:143 - - - - - - - -
      DATA (HERSH(136,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(137,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(138,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(139,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(140,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(141,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(142,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(143,K),K=1,4) / 699,2199,2199,2199/
C   144:151 dotlessi grave acute circumflex tilde - breve dotaccent
      DATA (HERSH(144,K),K=1,4) / 699,2182,2196,2199/
      DATA (HERSH(145,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(146,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(147,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(148,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(149,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(150,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(151,K),K=1,4) / 699,2199,2199,2199/
C   152:159 dieresis - ring - - - - -
      DATA (HERSH(152,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(153,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(154,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(155,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(156,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(157,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(158,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(159,K),K=1,4) / 699,2199,2199,2199/
C   160:167 space exclamdown cent sterling currency yen brokenbar section
      DATA (HERSH(160,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(161,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(162,K),K=1,4) / 910, 910, 910, 910/
      DATA (HERSH(163,K),K=1,4) / 272, 272, 272, 272/
      DATA (HERSH(164,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(165,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(166,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(167,K),K=1,4) /2276,2276,2276,2276/
C   168:175 - copyright - - - - registered -
      DATA (HERSH(168,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(169,K),K=1,4) / 274, 274, 274, 274/
      DATA (HERSH(170,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(171,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(172,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(173,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(174,K),K=1,4) / 273, 273, 273, 273/
      DATA (HERSH(175,K),K=1,4) / 699,2199,2199,2199/
C   176:183 degree plusminus twosuperior threesuperior
C           acute mu paragraph periodcentered
      DATA (HERSH(176,K),K=1,4) / 718,2218,2779,2779/
      DATA (HERSH(177,K),K=1,4) /2233,2233,2233,2233/
      DATA (HERSH(178,K),K=1,4) / 702,2202,2752,2752/
      DATA (HERSH(179,K),K=1,4) / 703,2203,2753,2753/
      DATA (HERSH(180,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(181,K),K=1,4) / 638,2138,2138,2138/
      DATA (HERSH(182,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(183,K),K=1,4) / 729, 729, 729, 729/
C   184:191 cedilla onesuperior ordmasculine guillemotright
C           onequarter onehalf threequarters questiondown
      DATA (HERSH(184,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(185,K),K=1,4) / 701,2201,2751,2751/
      DATA (HERSH(186,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(187,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(188,K),K=1,4) / 270, 270, 270, 270/
      DATA (HERSH(189,K),K=1,4) / 261, 261, 261, 261/
      DATA (HERSH(190,K),K=1,4) / 271, 271, 271, 271/
      DATA (HERSH(191,K),K=1,4) / 699,2199,2199,2199/
C   192:199 Agrave Aacute Acircumflex Atilde Aring AE Ccedilla
      DATA (HERSH(192,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(193,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(194,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(195,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(196,K),K=1,4) / 501,2001,2051,2551/
      DATA (HERSH(197,K),K=1,4) / 501,2078,2051,2551/
      DATA (HERSH(198,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(199,K),K=1,4) / 503,2003,2053,2553/
C   200:207 Egrave Eacute Ecircumflex Edieresis 
C           Igrave Iacute Icircumflex Idieresis
      DATA (HERSH(200,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(201,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(202,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(203,K),K=1,4) / 505,2005,2055,2555/
      DATA (HERSH(204,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH(205,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH(206,K),K=1,4) / 509,2009,2059,2559/
      DATA (HERSH(207,K),K=1,4) / 509,2009,2059,2559/
C   208:215 Eth Ntilde Ograve Oacute 
C           Ocircumflex Otilde Odieresis multiply
      DATA (HERSH(208,K),K=1,4) / 504,2004,2054,2554/
      DATA (HERSH(209,K),K=1,4) / 514,2014,2064,2564/
      DATA (HERSH(210,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(211,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(212,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(213,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(214,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(215,K),K=1,4) /2235,2235,2235,2235/
C   216:223 Oslash Ugrave Uacute Ucircumflex
C           Udieresis Yacute Thorn germandbls
      DATA (HERSH(216,K),K=1,4) / 515,2015,2065,2565/
      DATA (HERSH(217,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(218,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(219,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(220,K),K=1,4) / 521,2021,2071,2571/
      DATA (HERSH(221,K),K=1,4) / 525,2025,2075,2575/
      DATA (HERSH(222,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(223,K),K=1,4) / 699,2199,2199,2199/
C   224:231 agrave aacute acircumflex atilde aring ae ccedilla
      DATA (HERSH(224,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(225,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(226,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(227,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(228,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(229,K),K=1,4) / 601,2101,2151,2651/
      DATA (HERSH(230,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(231,K),K=1,4) / 603,2103,2153,2653/
C   232:239 egrave eacute ecircumflex edieresis 
C           igrave iacute icircumflex idieresis
      DATA (HERSH(232,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(233,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(234,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(235,K),K=1,4) / 605,2105,2155,2655/
      DATA (HERSH(236,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(237,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(238,K),K=1,4) / 609,2109,2159,2659/
      DATA (HERSH(239,K),K=1,4) / 609,2109,2159,2659/
C   240:247 eth ntilde ograve oacute 
C           ocircumflex otilde odieresis divide
      DATA (HERSH(240,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(241,K),K=1,4) / 614,2114,2164,2664/
      DATA (HERSH(242,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(243,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(244,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(245,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(246,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(247,K),K=1,4) /2237,2237,2237,2237/
C   248:255 oslash ugrave uacute ucircumflex
C           udieresis yacute thorn ydieresis
      DATA (HERSH(248,K),K=1,4) / 615,2115,2165,2665/
      DATA (HERSH(249,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(250,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(251,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(252,K),K=1,4) / 621,2121,2171,2671/
      DATA (HERSH(253,K),K=1,4) / 625,2125,2175,2675/
      DATA (HERSH(254,K),K=1,4) / 699,2199,2199,2199/
      DATA (HERSH(255,K),K=1,4) / 625,2125,2175,2675/
C
C Greek alphabet.
C
      DATA (HERSH(256,K),K=1,4) / 527,2027,2027,2027/
      DATA (HERSH(257,K),K=1,4) / 528,2028,2028,2028/
      DATA (HERSH(258,K),K=1,4) / 529,2029,2029,2029/
      DATA (HERSH(259,K),K=1,4) / 530,2030,2030,2030/
      DATA (HERSH(260,K),K=1,4) / 531,2031,2031,2031/
      DATA (HERSH(261,K),K=1,4) / 532,2032,2032,2032/
      DATA (HERSH(262,K),K=1,4) / 533,2033,2033,2033/
      DATA (HERSH(263,K),K=1,4) / 534,2034,2034,2034/
      DATA (HERSH(264,K),K=1,4) / 535,2035,2035,2035/
      DATA (HERSH(265,K),K=1,4) / 536,2036,2036,2036/
      DATA (HERSH(266,K),K=1,4) / 537,2037,2037,2037/
      DATA (HERSH(267,K),K=1,4) / 538,2038,2038,2038/
      DATA (HERSH(268,K),K=1,4) / 539,2039,2039,2039/
      DATA (HERSH(269,K),K=1,4) / 540,2040,2040,2040/
      DATA (HERSH(270,K),K=1,4) / 541,2041,2041,2041/
      DATA (HERSH(271,K),K=1,4) / 542,2042,2042,2042/
      DATA (HERSH(272,K),K=1,4) / 543,2043,2043,2043/
      DATA (HERSH(273,K),K=1,4) / 544,2044,2044,2044/
      DATA (HERSH(274,K),K=1,4) / 545,2045,2045,2045/
      DATA (HERSH(275,K),K=1,4) / 546,2046,2046,2046/
      DATA (HERSH(276,K),K=1,4) / 547,2047,2047,2047/
      DATA (HERSH(277,K),K=1,4) / 548,2048,2048,2048/
      DATA (HERSH(278,K),K=1,4) / 549,2049,2049,2049/
      DATA (HERSH(279,K),K=1,4) / 550,2050,2050,2050/
      DATA (HERSH(280,K),K=1,4) / 627,2127,2127,2127/
      DATA (HERSH(281,K),K=1,4) / 628,2128,2128,2128/
      DATA (HERSH(282,K),K=1,4) / 629,2129,2129,2129/
      DATA (HERSH(283,K),K=1,4) / 630,2130,2130,2130/
      DATA (HERSH(284,K),K=1,4) / 684,2184,2184,2184/
      DATA (HERSH(285,K),K=1,4) / 632,2132,2132,2132/
      DATA (HERSH(286,K),K=1,4) / 633,2133,2133,2133/
      DATA (HERSH(287,K),K=1,4) / 685,2185,2185,2185/
      DATA (HERSH(288,K),K=1,4) / 635,2135,2135,2135/
      DATA (HERSH(289,K),K=1,4) / 636,2136,2136,2136/
      DATA (HERSH(290,K),K=1,4) / 637,2137,2137,2137/
      DATA (HERSH(291,K),K=1,4) / 638,2138,2138,2138/
      DATA (HERSH(292,K),K=1,4) / 639,2139,2139,2139/
      DATA (HERSH(293,K),K=1,4) / 640,2140,2140,2140/
      DATA (HERSH(294,K),K=1,4) / 641,2141,2141,2141/
      DATA (HERSH(295,K),K=1,4) / 642,2142,2142,2142/
      DATA (HERSH(296,K),K=1,4) / 643,2143,2143,2143/
      DATA (HERSH(297,K),K=1,4) / 644,2144,2144,2144/
      DATA (HERSH(298,K),K=1,4) / 645,2145,2145,2145/
      DATA (HERSH(299,K),K=1,4) / 646,2146,2146,2146/
      DATA (HERSH(300,K),K=1,4) / 686,2186,2186,2186/
      DATA (HERSH(301,K),K=1,4) / 648,2148,2148,2148/
      DATA (HERSH(302,K),K=1,4) / 649,2149,2149,2149/
      DATA (HERSH(303,K),K=1,4) / 650,2150,2150,2150/
C
      IF ((CODE.LT.0) .OR. (CODE.GT.303)) THEN
          I = 1
      ELSE
          I = CODE
      END IF
      SYMBOL = HERSH(I,FONT)
C
      END
C*GRSYXD -- obtain the polyline representation of a given symbol
C+
      SUBROUTINE GRSYXD (SYMBOL, XYGRID, UNUSED)
      INTEGER SYMBOL
      INTEGER XYGRID(300)
      LOGICAL UNUSED
C
C Return the digitization coordinates of a character. Each character is
C defined on a grid with X and Y coordinates in the range (-49,49), 
C with the origin (0,0) at the center of the character.  The coordinate
C system is right-handed, with X positive to the right, and Y positive
C upward.  
C
C Arguments:
C  SYMBOL (input)  : symbol number in range (1..3000).
C  XYGRID (output) : height range, width range, and pairs of (x,y)
C                    coordinates returned.  Height range = (XYGRID(1),
C                    XYGRID(3)).  Width range = (XYGRID(4),XYGRID(5)).
C                    (X,Y) = (XYGRID(K),XYGRID(K+1)) (K=6,8,...).
C  UNUSED (output) : receives .TRUE. if SYMBOL is an unused symbol
C                    number. A character of normal height and zero width
C                    is returned. Receives .FALSE. if SYMBOL is a 
C                    valid symbol number.
C 
C The height range consists of 3 values: (minimum Y, baseline Y,
C maximum Y).  The first is reached by descenders on lower-case g, p,
C q, and y.  The second is the bottom of upper-case letters.  The third
C is the top of upper-case letters.  A coordinate pair (-64,0) requests
C a pen raise, and a pair (-64,-64) terminates the coordinate list. It
C is assumed that movement to the first coordinate position will be
C done with the pen raised - no raise command is explicitly included to
C do this. 
C--
C  7-Mar-1983.
C 15-Dec-1988 - standardize.
C-----------------------------------------------------------------------
      INTEGER*2    BUFFER(27000)
      INTEGER      INDEX(3000), IX, IY, K, L, LOCBUF
      INTEGER      NC1, NC2
      COMMON       /GRSYMB/ NC1, NC2, INDEX, BUFFER
C
C Extract digitization.
C
      IF (SYMBOL.LT.NC1 .OR. SYMBOL.GT.NC2) GOTO 3000
      L = SYMBOL - NC1 + 1
      LOCBUF = INDEX(L)
      IF (LOCBUF .EQ. 0) GOTO 3000
      XYGRID(1) = BUFFER(LOCBUF)
      LOCBUF = LOCBUF + 1
      K = 2
      IY = -1
C     -- DO WHILE (IY.NE.-64)
  100 IF (IY.NE.-64) THEN
          IX = BUFFER(LOCBUF)/128
          IY = BUFFER(LOCBUF) - 128*IX - 64
          XYGRID(K) = IX - 64
          XYGRID(K+1) = IY
          K = K + 2
          LOCBUF = LOCBUF + 1
      GOTO 100
      END IF
C     -- end DO WHILE
      UNUSED = .FALSE.
      RETURN
C
C Unimplemented character.
C
3000  XYGRID(1) = -16
      XYGRID(2) =  -9
      XYGRID(3) = +12
      XYGRID(4) =   0
      XYGRID(5) =   0
      XYGRID(6) = -64
      XYGRID(7) = -64
      UNUSED = .TRUE.
      RETURN
      END

C*GRTERM -- flush buffer to output device
C+
      SUBROUTINE GRTERM
C
C GRPCKG: flush the buffer associated with the current plot. GRTERM
C should be called only when it is necessary to make sure that all the
C graphics created up to this point in the program are visible on the
C device, e.g., before beginning a dialog with the user. GRTERM has no
C effect on hardcopy devices.
C
C Arguments: none.
C--
C  6-Oct-1983
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C 31-Dec-1985 - do not send CAN code to true Tek [TJP/PCP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER NBUF,LCHR
      REAL    RBUF(6)
      CHARACTER CHR
C
      IF (GRCIDE.GE.1) THEN
          CALL GREXEC(GRGTYP,16,RBUF,NBUF,CHR,LCHR)
      END IF
      END
C*GRTEXT -- draw text
C+
      SUBROUTINE GRTEXT (CENTER,ORIENT,ABSXY,X0,Y0,STRING)
C
C GRPCKG: Write a text string using the high-quality character set.
C The text is NOT windowed in the current viewport, but may extend over
C the whole view surface.  Line attributes (color, intensity thickness)
C apply to text, but line-style is ignored.  The current pen position
C after a call to GRTEXT is undefined.
C
C Arguments:
C
C STRING (input, character): the character string to be plotted. This
C       may include standard escape-sequences to represent non-ASCII
C       characters and special commands. The number of characters in
C       STRING (i.e., LEN(STRING)) should not exceed 256.
C--
C (3-May-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C  6-Sep-1989 - standardize [TJP].
C 20-Apr-1995 - Verbose PS file support.  If PGPLOT_PS_VERBOSE_TEXT is
C               defined, text strings in PS files are preceded by a 
C               comment with the text of the string plotted as vectors
C               [TJP after D.S.Briggs].
C  4-Feb-1997 - grexec requires an RBUF array, not a scalar [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL ABSXY,UNUSED,VISBLE,CENTER
      INTEGER XYGRID(300)
      INTEGER LIST(256)
      CHARACTER*(*) STRING
      REAL ANGLE, FACTOR, FNTBAS, FNTFAC, COSA, SINA, DX, DY, XORG, YORG
      REAL XCUR, YCUR, ORIENT, RATIO, X0, Y0, RLX, RLY
      REAL XMIN, XMAX, YMIN, YMAX
      REAL RBUF(6)
      INTEGER I, IFNTLV,NLIST,LX,LY, K, LXLAST,LYLAST, LSTYLE
      INTEGER SLEN, GRTRIM
      INTRINSIC ABS, COS, LEN, MIN, SIN
      CHARACTER DEVTYP*14, STEMP*258
      LOGICAL DEVINT, VTEXT
C
C Check that there is something to be plotted.
C
      IF (LEN(STRING).LE.0) RETURN
C
C Check that a device is selected.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRTEXT - no graphics device is active.')
          RETURN
      END IF
C
C Save current line-style, and set style "normal".
C
      CALL GRQLS(LSTYLE)
      CALL GRSLS(1)
C
C Put device dependent code here or at end
C
      VTEXT = .FALSE.
      CALL GRQTYP (DEVTYP, DEVINT)
      IF ((DEVTYP.EQ.'PS').OR.(DEVTYP.EQ.'VPS').OR.
     1    (DEVTYP.EQ.'CPS').OR.(DEVTYP.EQ.'VCPS')) THEN
         CALL GRGENV ('PS_VERBOSE_TEXT', STEMP, I)
         VTEXT = (I.GT.0)
         IF (VTEXT) THEN
            SLEN = GRTRIM(STRING)
            STEMP = '% Start "' // STRING(1:SLEN) // '"'
            CALL GREXEC (GRGTYP, 23, RBUF, 0, STEMP, SLEN+10)
         END IF
      END IF
C
C Save current viewport, and open the viewport to include the full
C view surface.
C
      XORG = GRXPRE(GRCIDE)
      YORG = GRYPRE(GRCIDE)
      XMIN = GRXMIN(GRCIDE)
      XMAX = GRXMAX(GRCIDE)
      YMIN = GRYMIN(GRCIDE)
      YMAX = GRYMAX(GRCIDE)
      CALL GRAREA(GRCIDE, 0.0, 0.0, 0.0, 0.0)
C
C Compute scaling and orientation.
C
      ANGLE = ORIENT*(3.14159265359/180.)
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
      CALL GRTXY0(ABSXY, X0, Y0, XORG, YORG)
      FNTBAS = 0.0
      FNTFAC = 1.0
      IFNTLV = 0
      DX = 0.0
      DY = 0.0
C
C Convert the string to a list of symbol numbers; to prevent overflow
C of array LIST, the length of STRING is limited to 256 characters.
C
      CALL GRSYDS(LIST,NLIST,STRING(1:MIN(256,LEN(STRING))),
     1             GRCFNT(GRCIDE))
C
C Plot the string of characters
C
      DO 380 I = 1,NLIST
          IF (LIST(I).LT.0) THEN
              IF (LIST(I).EQ.-1) THEN
C                 ! up
                  IFNTLV = IFNTLV+1
                  FNTBAS = FNTBAS + 16.0*FNTFAC
                  FNTFAC = 0.75**ABS(IFNTLV)
              ELSE IF (LIST(I).EQ.-2) THEN
C                 ! down
                  IFNTLV = IFNTLV-1
                  FNTFAC = 0.75**ABS(IFNTLV)
                  FNTBAS = FNTBAS - 16.0*FNTFAC
              ELSE IF (LIST(I).EQ.-3) THEN
C                 ! backspace
                  XORG = XORG - DX*FNTFAC
                  YORG = YORG - DY*FNTFAC
              END IF
              GOTO 380
          END IF
          CALL GRSYXD(LIST(I),XYGRID,UNUSED)
          VISBLE = .FALSE.
          LX = XYGRID(5)-XYGRID(4)
          DX = COSA*LX*RATIO
          DY = SINA*LX
          K = 4
          LXLAST = -64
          LYLAST = -64
  320     K = K+2
          LX = XYGRID(K)
          LY = XYGRID(K+1)
          IF (LY.EQ.-64) GOTO 330
          IF (LX.EQ.-64) THEN
              VISBLE = .FALSE.
          ELSE
              RLX = (LX - XYGRID(4))*FNTFAC
              RLY = (LY - XYGRID(2))*FNTFAC + FNTBAS
              IF ((LX.NE.LXLAST) .OR. (LY.NE.LYLAST)) THEN
                  XCUR = XORG + (COSA*RLX - SINA*RLY)*RATIO
                  YCUR = YORG + (SINA*RLX + COSA*RLY)
                  IF (VISBLE) THEN
                      CALL GRLIN0(XCUR,YCUR)
                  ELSE
                      GRXPRE(GRCIDE) = XCUR
                      GRYPRE(GRCIDE) = YCUR
                  END IF
              END IF
              VISBLE = .TRUE.
              LXLAST = LX
              LYLAST = LY
          END IF
          GOTO 320
  330     XORG = XORG + DX*FNTFAC
          YORG = YORG + DY*FNTFAC
  380 CONTINUE
C
C Set pen position ready for next character.
C
      GRXPRE(GRCIDE) = XORG
      GRYPRE(GRCIDE) = YORG
C
C Another possible device dependent section
C
      IF (VTEXT) THEN
         STEMP = '% End "' // STRING(1:SLEN) // '"'
         CALL GREXEC(GRGTYP, 23, RBUF, 0, STEMP, SLEN+8)
      END IF
C
C Restore the viewport and line-style, and return.
C
      GRXMIN(GRCIDE) = XMIN
      GRXMAX(GRCIDE) = XMAX
      GRYMIN(GRCIDE) = YMIN
      GRYMAX(GRCIDE) = YMAX
      CALL GRSLS(LSTYLE)
C
      END

C*GRTOUP -- convert character string to upper case
C+
      SUBROUTINE GRTOUP (DST, SRC)
      CHARACTER*(*) DST, SRC
C
C GRPCKG (internal routine): convert character string to upper case.
C
C Arguments:
C  DST    (output) : output string (upper case).
C  SRC    (input)  : input string to be converted.
C--
C 1988-Jan-18 (TJP)
C-----------------------------------------------------------------------
      INTEGER I, N, NCHI, NCHO, NCH
      NCHI = LEN(SRC)
      NCHO = LEN(DST)
      NCH = MIN(NCHI, NCHO)
      DO 10 I=1,NCH
          N = ICHAR(SRC(I:I))
          IF ((N .GE. 97) .AND. (N .LE. 122)) THEN
              DST(I:I) = CHAR(N - 32)
          ELSE
              DST(I:I) = CHAR(N)
          END IF
   10 CONTINUE
      IF (NCHO .GT. NCHI) DST(NCHI+1:NCHO) = ' '
      END

C*GRTRAN -- define scaling transformation
C+
      SUBROUTINE GRTRAN (IDENT,XORG,YORG,XSCALE,YSCALE)
C
C GRPCKG (internal routine): Define scaling transformation.
C
C Arguments:
C
C IDENT (input, integer): plot identifier, as returned by GROPEN.
C XORG, YORG, XSCALE, YSCALE (input, real): parameters of the scaling
C       transformation. This is defined by:
C               XABS = XORG + XWORLD * XSCALE,
C               YABS = YORG + YWORLD * YSCALE,
C       where (XABS, YABS) are the absolute device coordinates
C       corresponding to world coordinates (XWORLD, YWORLD).
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INTEGER  IDENT
      REAL     XORG, YORG, XSCALE, YSCALE
C
      CALL GRSLCT(IDENT)
      CALL GRTRN0(XORG, YORG, XSCALE, YSCALE)
C
      END
C*GRTRIM -- length of string excluding trailing blanks
C+
      INTEGER FUNCTION GRTRIM(S)
      CHARACTER*(*) S
C
C Find the length of a character string excluding trailing blanks.
C A blank string returns a value of 0.
C
C Argument:
C  S      (input)  : character string.
C
C Returns:
C  GRTRIM          : number of characters in S, excluding trailing
C                    blanks, in range 0...LEN(S). A blank string
C                    returns a value of 0.
C
C Subroutines required:
C  None
C
C Fortran 77 extensions:
C  None
C
C History:
C  1987 Nov 12 - TJP.
C-----------------------------------------------------------------------
      INTEGER  I
C
      IF (S.EQ.' ') THEN
          GRTRIM = 0
      ELSE
          DO 10 I=LEN(S),1,-1
              GRTRIM = I
              IF (S(I:I).NE.' ') GOTO 20
   10     CONTINUE
          GRTRIM = 0
   20     CONTINUE
      END IF
      END

C*GRTRN0 -- define scaling transformation
C+
      SUBROUTINE GRTRN0 (XORG,YORG,XSCALE,YSCALE)
C
C GRPCKG (internal routine): Define scaling transformation for current
C device (equivalent to GRTRAN without device selection).
C
C Arguments:
C
C XORG, YORG, XSCALE, YSCALE (input, real): parameters of the scaling
C       transformation. This is defined by:
C               XABS = XORG + XWORLD * XSCALE,
C               YABS = YORG + YWORLD * YSCALE,
C       where (XABS, YABS) are the absolute device coordinates
C       corresponding to world coordinates (XWORLD, YWORLD).
C--
C  1-Feb-83:
C 11-Feb-92: Add driver support (TJP).
C  1-Sep-94: Suppress driver call (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     XORG, YORG, XSCALE, YSCALE
      REAL           RBUF(6)
      INTEGER        NBUF,LCHR
      CHARACTER*16   CHR
C
      GRXORG(GRCIDE) = XORG
      GRXSCL(GRCIDE) = XSCALE
      GRYORG(GRCIDE) = YORG
      GRYSCL(GRCIDE) = YSCALE
C
C Pass info to device driver?
C
      IF (GRGCAP(GRCIDE)(2:2).EQ.'X') THEN
          RBUF(1) = XORG
          RBUF(2) = XSCALE
          RBUF(3) = YORG
          RBUF(4) = YSCALE
          NBUF = 4
          LCHR = 0
          CALL GREXEC(GRGTYP,27,RBUF,NBUF,CHR,LCHR)
      END IF
C
      END

C*GRTXY0 -- convert world coordinates to device coordinates
C+
      SUBROUTINE GRTXY0 (ABSXY,X,Y,XT,YT)
C
C GRPCKG (internal routine): Convert scaled position to absolute
C position.
C
C Arguments:
C
C ABSXY (input, logical): if FALSE, convert world coordinates to
C       absolute device coordinates; if TRUE, return the input
C       coordinates unchanged.
C X, Y (input, real): input coordinates (absolute or world, depending
C       on setting of ABSXY).
C XT, YT (output, real): output absolute device coordinates.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL  ABSXY
      REAL     X, Y, XT, YT
C
      IF (ABSXY) THEN
          XT = X
          YT = Y
      ELSE
          XT = X * GRXSCL(GRCIDE) + GRXORG(GRCIDE)
          YT = Y * GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      END IF
C
      END

C*GRVCT0 -- draw line segments or dots
C+
      SUBROUTINE GRVCT0 (MODE,ABSXY,POINTS,X,Y)
C
C GRPCKG (internal routine): Draw a line or a set of dots. This
C is the same as GRVECT, but without device selection. It can be used to
C draw a single line-segment, a continuous series of line segments, or
C one or more single dots (pixels).
C
C Arguments:
C
C MODE (input, integer): if MODE=1, a series of line segments is drawn,
C       starting at the current position, moving to X(1),Y(1), ... and
C       ending at X(POINTS),Y(POINTS).
C       If MODE=2, the first vector is blanked, so the line starts at
C       X(1),Y(1).
C       If MODE=3, a single dot is placed at each coordinate pair, with
C       no connecting lines.
C ABSXY (input, logical): if TRUE, the coordinates are absolute device
C       coordinates; if FALSE, they are world coordinates and the
C       scaling transformation is applied.
C POINTS (input, integer): the number of coordinate pairs.
C X, Y (input, real arrays, dimensioned POINTS or greater): the
C       X and Y coordinates of the points.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, MODE, POINTS
      LOGICAL  ABSXY
      REAL     X(POINTS), Y(POINTS), XCUR, YCUR
C
      IF (MODE.EQ.1) THEN
          CALL GRTXY0(ABSXY, X(1), Y(1), XCUR, YCUR)
          CALL GRLIN0(XCUR, YCUR)
      ELSE IF (MODE.EQ.2) THEN
          CALL GRTXY0(ABSXY, X(1), Y(1), GRXPRE(GRCIDE), GRYPRE(GRCIDE))
      END IF
      IF (MODE.EQ.1 .OR. MODE.EQ.2) THEN
          DO 10 I=2,POINTS
              CALL GRTXY0(ABSXY, X(I), Y(I), XCUR, YCUR)
              CALL GRLIN0(XCUR, YCUR)
   10     CONTINUE
      ELSE IF (MODE.EQ.3) THEN
          DO 20 I=1,POINTS
              CALL GRTXY0(ABSXY, X(I), Y(I), XCUR, YCUR)
              CALL GRDOT0(XCUR, YCUR)
   20     CONTINUE
      END IF
C
      END

C*GRVECT -- draw line segments or dots
C+
      SUBROUTINE GRVECT (IDENT,MODE,ABSXY,POINTS,X,Y)
C
C GRPCKG: Draw a line or a set of dots. This routine can be used to
C draw a single line-segment, a continuous series of line segments, or
C one or more single dots (pixels).
C
C Arguments:
C
C IDENT (input, integer): the plot identifier, as returned by GROPEN.
C MODE (input, integer): if MODE=1, a series of line segments is drawn,
C       starting at the current position, moving to X(1),Y(1), ... and
C       ending at X(POINTS),Y(POINTS).
C       If MODE=2, the first vector is blanked, so the line starts at
C       X(1),Y(1).
C       If MODE=3, a single dot is placed at each coordinate pair, with
C       no connecting lines.
C ABSXY (input, logical): if TRUE, the coordinates are absolute device
C       coordinates; if FALSE, they are world coordinates and the
C       scaling transformation is applied.
C POINTS (input, integer): the number of coordinate pairs.
C X, Y (input, real arrays, dimensioned POINTS or greater): the
C       X and Y coordinates of the points.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INTEGER  IDENT, MODE, POINTS
      LOGICAL  ABSXY
      REAL     X(POINTS), Y(POINTS)
C
      CALL GRSLCT(IDENT)
      IF (MODE.LE.0 .OR. MODE.GT.3) THEN
          CALL GRWARN('GRVECT - invalid MODE parameter.')
      ELSE IF (POINTS.GT.0) THEN
          CALL GRVCT0(MODE, ABSXY, POINTS, X, Y)
      END IF
C
      END
C*GRWARN -- issue warning message to user
C+
      SUBROUTINE GRWARN (TEXT)
      CHARACTER*(*) TEXT
C
C Report a warning message on standard output, with prefix "%PGPLOT, ".
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C  8-Nov-1994 [TJP]
C-----------------------------------------------------------------------
      INTEGER   GRTRIM
C
      IF (TEXT.NE.' ') THEN
          WRITE (*, '(1X,2A)') '%PGPLOT, ', TEXT(1:GRTRIM(TEXT))
      END IF
      END
C*GRXHLS -- convert RGB color to HLS color
C+
      SUBROUTINE GRXHLS (R,G,B,H,L,S)
C
C GRPCKG: Convert a color specified in the RGB color model to one in
C the HLS model.  This is a support routine: no graphics I/O occurs.
C The inverse transformation is accomplished with routine GRXRGB.
C Reference: SIGGRAPH Status Report of the Graphic Standards Planning
C Committee, Computer Graphics, Vol.13, No.3, Association for
C Computing Machinery, New York, NY, 1979.
C
C Arguments:
C
C R,G,B (real, input): red, green, blue color coordinates, each in the
C       range 0.0 to 1.0. Input outside this range causes HLS = (0,1,0)
C       [white] to be returned.
C H,L,S (real, output): hue (0 to 360), lightness (0 to 1.0), and
C       saturation (0 to 1.0).
C--
C  2-Jul-1984 - new routine [TJP].
C 29-Sep-1994 - force H to be in rnage 0-360 [Remko Scharroo; TJP].
C-----------------------------------------------------------------------
      REAL     R,G,B, H,L,S, MA, MI, RR, GG, BB, D
C
      H = 0.0
      L = 1.0
      S = 0.0
      MA = MAX(R,G,B)
      MI = MIN(R,G,B)
      IF (MA.GT.1.0 .OR. MI.LT.0.0) RETURN
      RR = (MA-R)
      GG = (MA-G)
      BB = (MA-B)
C
C Lightness
C
      L = 0.5*(MA+MI)
C
C Achromatic case (R=G=B)
C
      IF (MA.EQ.MI) THEN
          S = 0.0
          H = 0.0
C
C Chromatic case
C
      ELSE
C         -- Saturation
          D = MA-MI
          IF (L.LE.0.5) THEN
              S = D/(MA+MI)
          ELSE
              S = D/(2.0-MA-MI)
          END IF
C         -- Hue
          IF (R.EQ.MA) THEN
C             -- yellow to magenta
              H = (2.0*D+BB-GG)
          ELSE IF (G.EQ.MA) THEN
              H = (4.0*D+RR-BB)
          ELSE
C             ! (B.EQ.MA)
              H = (6.0*D+GG-RR)
          END IF
          H = MOD(H*60.0/D,360.0)
          IF (H.LT.0.0) H = H+360.0
      END IF
C
      END
C*GRXRGB -- convert HLS color to RGB color
C+
      SUBROUTINE GRXRGB (H,L,S,R,G,B)
C
C GRPCKG: Convert a color specified in the HLS color model to one in
C the RGB model.  This is a support routine: no graphics I/O occurs.
C The inverse transformation is accomplished with routine GRXHLS.
C Reference: SIGGRAPH Status Report of the Graphic Standards Planning
C Committee, Computer Graphics, Vol.13, No.3, Association for
C Computing Machinery, New York, NY, 1979.
C
C Arguments:
C
C H,L,S (real, input): hue (0 to 360), lightness (0 to 1.0), and
C       saturation (0 to 1.0).
C R,G,B (real, output): red, green, blue color coordinates, each in the
C       range 0.0 to 1.0.
C--
C  2-Jul-1984 - new routine [TJP].
C 29-Sep-1994 - take H module 360 [TJP].
C 26-Nov-1996 - force results to be in range (avoid rounding error
C               problems on some machines) [TJP].
C-----------------------------------------------------------------------
      REAL     H,L,S, R,G,B, MA, MI, HM
C
      HM = MOD(H, 360.0)
      IF (HM.LT.0.0) HM = HM+360.0
      IF (L.LE.0.5) THEN
          MA = L*(1.0+S)
      ELSE
          MA = L + S - L*S
      END IF
      MI = 2.0*L-MA
C
C R component
C
      IF (HM.LT.60.0) THEN
          R = MI + (MA-MI)*HM/60.0
      ELSE IF (HM.LT.180.0) THEN
          R = MA
      ELSE IF (HM.LT.240.0) THEN
          R = MI + (MA-MI)*(240.0-HM)/60.0
      ELSE
          R = MI
      END IF
C
C G component
C
      IF (HM.LT.120.0) THEN
          G = MI
      ELSE IF (HM.LT.180.0) THEN
          G = MI + (MA-MI)*(HM-120.0)/60.0
      ELSE IF (HM.LT.300.0) THEN
          G = MA
      ELSE
          G = MI + (MA-MI)*(360.0-HM)/60.0
      END IF
C
C B component
C
      IF (HM.LT.60.0 .OR. HM.GE.300.0) THEN
          B = MA
      ELSE IF (HM.LT.120.0) THEN
          B = MI + (MA-MI)*(120.0-HM)/60.0
      ELSE IF (HM.LT.240.0) THEN
          B = MI
      ELSE
          B = MI + (MA-MI)*(HM-240.0)/60.0
      END IF
C
      R = MIN(1.0, MAX(0.0,R))
      G = MIN(1.0, MAX(0.0,G))
      B = MIN(1.0, MAX(0.0,B))
C
      END
C*PGADVANCE -- non-standard alias for PGPAGE
C+
      SUBROUTINE PGADVANCE
C
C See description of PGPAGE.
C--
      CALL PGPAGE
      END
C*PGARRO -- draw an arrow
C%void cpgarro(float x1, float y1, float x2, float y2);
C+
      SUBROUTINE PGARRO (X1, Y1, X2, Y2)
      REAL X1, Y1, X2, Y2
C
C Draw an arrow from the point with world-coordinates (X1,Y1) to 
C (X2,Y2). The size of the arrowhead at (X2,Y2) is determined by 
C the current character size set by routine PGSCH. The default size 
C is 1/40th of the smaller of the width or height of the view surface.
C The appearance of the arrowhead (shape and solid or open) is
C controlled by routine PGSAH.
C
C Arguments:
C  X1, Y1 (input)  : world coordinates of the tail of the arrow.
C  X2, Y2 (input)  : world coordinates of the head of the arrow.
C--
C  7-Feb-92 Keith Horne @ STScI / TJP.
C 13-Oct-92 - use arrowhead attributes; scale (TJP).
C-----------------------------------------------------------------------
      INTEGER AHFS, FS
      REAL DX, DY, XV1, XV2, YV1, YV2, XL, XR, YB, YT, DINDX, DINDY
      REAL XINCH, YINCH, RINCH, CA, SA, SO, CO, YP, XP, YM, XM, DHX, DHY
      REAL PX(4), PY(4)
      REAL AHANGL, AHVENT, SEMANG, CH, DH, XS1, XS2, YS1, YS2
C
      CALL PGBBUF
      CALL PGQAH(AHFS, AHANGL, AHVENT)
      CALL PGQFS(FS)
      CALL PGSFS(AHFS)
      DX = X2 - X1
      DY = Y2 - Y1
      CALL PGQCH(CH)
      CALL PGQVSZ(1, XS1, XS2, YS1, YS2)
C     -- length of arrowhead: 1 40th of the smaller of the height or
C        width of the view surface, scaled by character height.
      DH = CH*MIN(ABS(XS2-XS1),ABS(YS2-YS1))/40.0
      CALL PGMOVE(X2, Y2)
C     -- Is there to be an arrowhead ?
      IF (DH.GT.0.) THEN
          IF (DX.NE.0. .OR. DY.NE.0.) THEN
C             -- Get x and y scales
              CALL PGQVP(1, XV1, XV2, YV1, YV2)
              CALL PGQWIN(XL, XR, YB, YT)
              IF (XR.NE.XL .AND. YT.NE.YB) THEN
                  DINDX = (XV2 - XV1) / (XR - XL)
                  DINDY = (YV2 - YV1) / (YT - YB)
                  DHX = DH / DINDX
                  DHY = DH / DINDY
C                 -- Unit vector in direction of the arrow
                  XINCH = DX * DINDX
                  YINCH = DY * DINDY
                  RINCH = SQRT(XINCH*XINCH + YINCH*YINCH)
                  CA = XINCH / RINCH
                  SA = YINCH / RINCH
C                 -- Semiangle in radians
                  SEMANG = AHANGL/2.0/57.296
                  SO = SIN(SEMANG)
                  CO = -COS(SEMANG)
C                 -- Vector back along one edge of the arrow
                  XP = DHX * (CA*CO - SA*SO)
                  YP = DHY * (SA*CO + CA*SO)
C                 -- Vector back along other edge of the arrow
                  XM = DHX * (CA*CO + SA*SO)
                  YM = DHY * (SA*CO - CA*SO)
C                 -- Draw the arrowhead
                  PX(1) = X2
                  PY(1) = Y2
                  PX(2) = X2 + XP
                  PY(2) = Y2 + YP
                  PX(3) = X2 + 0.5*(XP+XM)*(1.0-AHVENT)
                  PY(3) = Y2 + 0.5*(YP+YM)*(1.0-AHVENT)
                  PX(4) = X2 + XM
                  PY(4) = Y2 + YM
                  CALL PGPOLY(4, PX, PY)
                  CALL PGMOVE(PX(3), PY(3))
              END IF
          END IF
      END IF
      CALL PGDRAW(X1, Y1)
      CALL PGMOVE(X2,Y2)
      CALL PGSFS(FS)
      CALL PGEBUF
      RETURN
      END
C*PGASK -- control new page prompting
C%void cpgask(Logical flag);
C+
      SUBROUTINE PGASK (FLAG)
      LOGICAL FLAG
C
C Change the ``prompt state'' of PGPLOT. If the prompt state is
C ON, PGPAGE will type ``Type RETURN for next page:'' and will wait
C for the user to type a carriage-return before starting a new page.
C The initial prompt state (after the device has been opened) is ON
C for interactive devices. Prompt state is always OFF for
C non-interactive devices.
C
C Arguments:
C  FLAG   (input)  : if .TRUE., and if the device is an interactive
C                    device, the prompt state will be set to ON. If
C                    .FALSE., the prompt state will be set to OFF.
C--
C-----------------------------------------------------------------------
      INCLUDE     'pgplot.inc'
      LOGICAL     PGNOTO
      CHARACTER*1 TYPE
C
      IF (PGNOTO('PGASK')) RETURN
C
      IF (FLAG) THEN
          CALL GRQTYP(TYPE,PGPRMP(PGID))
      ELSE
          PGPRMP(PGID) = .FALSE.
      END IF
      END
C*PGAXIS -- draw an axis
C%void cpgaxis(const char *opt, float x1, float y1, float x2, float y2, \
C%             float v1, float v2, float step, int nsub, float dmajl, \
C%             float dmajr, float fmin, float disp, float orient);
C+
      SUBROUTINE PGAXIS (OPT, X1, Y1, X2, Y2, V1, V2, STEP, NSUB,
     :                   DMAJL, DMAJR, FMIN, DISP, ORIENT)
      CHARACTER*(*) OPT
      REAL X1, Y1, X2, Y2, V1, V2, STEP, DMAJL, DMAJR, FMIN, DISP
      REAL ORIENT
      INTEGER NSUB
C
C Draw a labelled graph axis from world-coordinate position (X1,Y1) to
C (X2,Y2).
C
C Normally, this routine draws a standard LINEAR axis with equal
C subdivisions.   The quantity described by the axis runs from V1 to V2;
C this may be, but need not be, the same as X or Y. 
C
C If the 'L' option is specified, the routine draws a LOGARITHMIC axis.
C In this case, the quantity described by the axis runs from 10**V1 to
C 10**V2. A logarithmic axis always has major, labeled, tick marks 
C spaced by one or more decades. If the major tick marks are spaced
C by one decade (as specified by the STEP argument), then minor
C tick marks are placed at 2, 3, .., 9 times each power of 10;
C otherwise minor tick marks are spaced by one decade. If the axis
C spans less than two decades, numeric labels are placed at 1, 2, and
C 5 times each power of ten.
C
C If the axis spans less than one decade, or if it spans many decades,
C it is preferable to use a linear axis labeled with the logarithm of
C the quantity of interest.
C
C Arguments:
C  OPT    (input)  : a string containing single-letter codes for
C                    various options. The options currently
C                    recognized are:
C                    L : draw a logarithmic axis
C                    N : write numeric labels
C                    1 : force decimal labelling, instead of automatic
C                        choice (see PGNUMB).
C                    2 : force exponential labelling, instead of
C                        automatic.
C  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
C  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
C  V1     (input)  : axis value at first endpoint.
C  V2     (input)  : axis value at second endpoint.
C  STEP   (input)  : major tick marks are drawn at axis value 0.0 plus
C                    or minus integer multiples of STEP. If STEP=0.0,
C                    a value is chosen automatically.
C  NSUB   (input)  : minor tick marks are drawn to divide the major
C                    divisions into NSUB equal subdivisions (ignored if
C                    STEP=0.0). If NSUB <= 1, no minor tick marks are
C                    drawn. NSUB is ignored for a logarithmic axis.
C  DMAJL  (input)  : length of major tick marks drawn to left of axis
C                    (as seen looking from first endpoint to second), in
C                    units of the character height.
C  DMAJR  (input)  : length of major tick marks drawn to right of axis,
C                    in units of the character height.
C  FMIN   (input)  : length of minor tick marks, as fraction of major.
C  DISP   (input)  : displacement of baseline of tick labels to
C                    right of axis, in units of the character height.
C  ORIENT (input)  : orientation of label text, in degrees; angle between
C                    baseline of text and direction of axis (0-360°).
C--
C 25-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL V, VMIN, VMAX, DVMAJ, DVMIN
      REAL PGRND
      INTEGER I, K, K1, K2, NSUBT, NV, NP, LLAB, CLIP, FORM
      LOGICAL OPTN, PGNOTO
      CHARACTER CH, LABEL*32
C
C Check arguments.
C
      IF (PGNOTO('PGAXIS')) RETURN
      IF (X1.EQ.X2 .AND. Y1.EQ.Y2) RETURN
      IF (V1.EQ.V2) RETURN
C
C Decode options.
C
      FORM = 0
      OPTN = .FALSE.
      DO 10 I=1,LEN(OPT)
         CH = OPT(I:I)
         CALL GRTOUP(CH, CH)
         IF (CH.EQ.'N') THEN
C           -- numeric labels requested
            OPTN = .TRUE.
         ELSE IF (CH.EQ.'L') THEN
C           -- logarithmic axis requested
            CALL PGAXLG(OPT, X1, Y1, X2, Y2, V1, V2, STEP,
     :                  DMAJL, DMAJR, FMIN, DISP, ORIENT)
            RETURN
         ELSE IF (CH.EQ.'1') THEN
C           -- decimal labels requested
            FORM = 1
         ELSE IF (CH.EQ.'2') THEN
C           -- exponential labels requested
            FORM = 2
         END IF
 10   CONTINUE
C
C Choose major interval if defaulted. Requested interval = STEP,
C with NSUB subdivisions. We will use interval = DVMAJ with NSUBT
C subdivisions of size DVMIN. Note that DVMAJ is always positive.
C
      IF (STEP.EQ.0.0) THEN
          DVMAJ = PGRND(0.20*ABS(V1-V2),NSUBT)
      ELSE
          DVMAJ = ABS(STEP)
          NSUBT = MAX(NSUB,1)
      END IF
      DVMIN = DVMAJ/NSUBT
C
C For labelling, we need to express DVMIN as an integer times a
C power of 10, NV*(10**NP).
C
      NP = INT(LOG10(ABS(DVMIN)))-4
      NV = NINT(DVMIN/10.0**NP)
      DVMIN = REAL(NV)*(10.0**NP)
C
      CALL PGBBUF
      CALL PGQCLP(CLIP)
      CALL PGSCLP(0)
C
C Draw the axis.
C
      CALL PGMOVE(X1, Y1)
      CALL PGDRAW(X2, Y2)
C
C Draw the tick marks. Minor ticks are drawn at V = K*DVMIN, 
C major (labelled) ticks where K is a multiple of NSUBT.
C
      VMIN = MIN(V1, V2)
      VMAX = MAX(V1, V2)
      K1 = INT(VMIN/DVMIN)
      IF (DVMIN*K1.LT.VMIN) K1 = K1+1
      K2 = INT(VMAX/DVMIN)
      IF (DVMIN*K2.GT.VMAX) K2 = K2-1
      DO 20 K=K1,K2
         V = (K*DVMIN-V1)/(V2-V1)
         IF (MOD(K,NSUBT).EQ.0) THEN
C             -- major tick mark
            IF (OPTN) THEN
               CALL PGNUMB(K*NV, NP, FORM, LABEL, LLAB)
            ELSE
               LABEL = ' '
               LLAB = 1
            END IF
            CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL, DMAJR,
     :                  DISP, ORIENT, LABEL(:LLAB))
         ELSE
C             -- minor tick mark
            CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL*FMIN, DMAJR*FMIN,
     :                  0.0, ORIENT, ' ')
         END IF
 20   CONTINUE
C
      CALL PGSCLP(CLIP)
      CALL PGEBUF
      END
C PGAXLG -- draw a logarithmic axis [internal routine]
C
      SUBROUTINE PGAXLG (OPT, X1, Y1, X2, Y2, V1, V2, STEP,
     :                   DMAJL, DMAJR, FMIN, DISP, ORIENT)
      CHARACTER*(*) OPT
      REAL X1, Y1, X2, Y2, V1, V2, STEP
      REAL DMAJL, DMAJR, FMIN, DISP, ORIENT
C
C Draw a labelled graph axis from world-coordinate position (X1,Y1)
C  to (X2,Y2). The quantity described by the axis runs from 10**V1 to
C 10**V2. A logarithmic axis always has major, labeled, tick marks 
C spaced by one or more decades. If the major tick marks are spaced
C by one decade (as specified by the STEP argument), then minor
C tick marks are placed at 2, 3, .., 9 times each power of 10;
C otherwise minor tick marks are spaced by one decade. If the axis
C spans less than two decades, numeric labels are placed at 1, 2, and
C 5 times each power of ten.
C
C It is not advisable to use this routine if the axis spans less than
C one decade, or if it spans many decades. In these cases it is
C preferable to use a linear axis labeled with the logarithm of the
C quantity of interest.
C
C Arguments:
C  OPT    (input)  : a string containing single-letter codes for
C                    various options. The options currently
C                    recognized are:
C                    N : write numeric labels
C                    1 : force decimal labelling, instead of automatic
C                        choice (see PGNUMB).
C                    2 : force exponential labelling, instead of
C                        automatic.
C  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
C  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
C  V1     (input)  : logarithm of axis value at first endpoint.
C  V2     (input)  : logarithm of axis value at second endpoint.
C  STEP   (input)  : the number of decades between major (labeled) tick
C                    marks.
C  DMAJL  (input)  : length of major tick marks drawn to left of axis
C                    (as seen looking from first endpoint to second), in
C                    units of the character height.
C  DMAJR  (input)  : length of major tick marks drawn to right of axis,
C                    in units of the character height.
C  FMIN   (input)  : length of minor tick marks, as fraction of major.
C  DISP   (input)  : displacement of baseline of tick labels to
C                    right of axis, in units of the character height.
C  ORIENT (input)  : orientation of text label relative to axis (see
C                    PGTICK).
C--
C 25-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL V, VMIN, VMAX, DVMAJ, DVMIN, PGRND
      INTEGER I, K, K1, K2, LLAB, NSUBT, CLIP, FORM
      LOGICAL XLAB, OPTN
      CHARACTER*32 LABEL
      REAL TAB(9)
C
C Table of logarithms 1..9
C
      DATA TAB / 0.00000, 0.30103, 0.47712, 0.60206, 0.69897,
     :           0.77815, 0.84510, 0.90309, 0.95424 /
C
C Check arguments.
C
      IF (X1.EQ.X2 .AND. Y1.EQ.Y2) RETURN
      IF (V1.EQ.V2) RETURN
C
C Decode options.
C
      OPTN = INDEX(OPT,'N').NE.0 .OR. INDEX(OPT,'n').NE.0
      FORM =0
      IF (INDEX(OPT,'1').NE.0) FORM = 1
      IF (INDEX(OPT,'2').NE.0) FORM = 2
C
C Choose major interval (DVMAJ in the logarithm, with minimum value
C 1.0 = one decade). The minor interval is always 1.0.
C
      IF (STEP.GT.0.5) THEN
         DVMAJ = NINT(STEP)
      ELSE
         DVMAJ = PGRND(0.20*ABS(V1-V2),NSUBT)
         IF (DVMAJ.LT.1.0) DVMAJ = 1.0
      END IF
      DVMIN = 1.0
      NSUBT = DVMAJ/DVMIN
C
      CALL PGBBUF
      CALL PGQCLP(CLIP)
      CALL PGSCLP(0)
C
C Draw the axis.
C
      CALL PGMOVE(X1, Y1)
      CALL PGDRAW(X2, Y2)
C
C Draw the tick marks. Major ticks are drawn at V = K*DVMAJ.
C
      VMIN = MIN(V1, V2)
      VMAX = MAX(V1, V2)
      K1 = INT(VMIN/DVMIN)
      IF (DVMIN*K1.LT.VMIN) K1 = K1+1
      K2 = INT(VMAX/DVMIN)
      IF (DVMIN*K2.GT.VMAX) K2 = K2-1
      XLAB = (K2-K1) .LE. 2
      DO 20 K=K1,K2
         V = (K*DVMIN-V1)/(V2-V1)
         IF (MOD(K,NSUBT).EQ.0) THEN
C             -- major tick mark
            IF (OPTN) THEN
               CALL PGNUMB(1, NINT(K*DVMIN), FORM, LABEL, LLAB)
            ELSE
               LABEL = ' '
               LLAB = 1
            END IF
            CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL, DMAJR,
     :                  DISP, ORIENT, LABEL(:LLAB))
         ELSE
C             -- minor tick mark
            CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL*FMIN, DMAJR*FMIN,
     :                  0.0, ORIENT, ' ')
         END IF
 20   CONTINUE
C
C Draw intermediate tick marks if required. 
C Label them if axis spans less than 2 decades.
C
      IF (NSUBT.EQ.1) THEN
         DO 30 K=K1-1,K2+1
            DO 25 I=2,9
               V = (K*DVMIN + TAB(I) -V1)/(V2-V1)
               IF (V.GE.0.0 .AND. V.LE.1.0) THEN
                  IF (OPTN.AND.(XLAB .AND.(I.EQ.2 .OR. I.EQ.5))) THEN
C                    -- labeled minor tick mark
                     CALL PGNUMB(I, NINT(K*DVMIN), FORM, LABEL, LLAB)
                  ELSE
C                    -- unlabeled minor tick mark
                     LABEL = ' '
                     LLAB = 1
                  END IF
                  CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL*FMIN, DMAJR*FMIN,
     :                        DISP, ORIENT, LABEL(:LLAB))
               END IF
 25         CONTINUE
 30      CONTINUE
      END IF
C
      CALL PGSCLP(CLIP)
      CALL PGEBUF
      END
C*PGBAND -- read cursor position, with anchor
C%int cpgband(int mode, int posn, float xref, float yref, float *x,\
C%            float *y, char *ch_scalar);
C+
      INTEGER FUNCTION PGBAND (MODE, POSN, XREF, YREF, X, Y, CH)
      INTEGER MODE, POSN
      REAL XREF, YREF, X, Y
      CHARACTER*(*) CH
C
C Read the cursor position and a character typed by the user.
C The position is returned in world coordinates.  PGBAND positions
C the cursor at the position specified (if POSN=1), allows the user to
C move the cursor using the mouse or arrow keys or whatever is available
C on the device. When he has positioned the cursor, the user types a
C single character on the keyboard; PGBAND then returns this
C character and the new cursor position (in world coordinates).
C
C Some interactive devices offer a selection of cursor types,
C implemented as thin lines that move with the cursor, but without
C erasing underlying graphics. Of these types, some extend between
C a stationary anchor-point at XREF,YREF, and the position of the
C cursor, while others simply follow the cursor without changing shape
C or size. The cursor type is specified with one of the following MODE
C values. Cursor types that are not supported by a given device, are
C treated as MODE=0.
C
C -- If MODE=0, the anchor point is ignored and the routine behaves
C like PGCURS.
C -- If MODE=1, a straight line is drawn joining the anchor point 
C and the cursor position.
C -- If MODE=2, a hollow rectangle is extended as the cursor is moved,
C with one vertex at the anchor point and the opposite vertex at the
C current cursor position; the edges of the rectangle are horizontal
C and vertical.
C -- If MODE=3, two horizontal lines are extended across the width of
C the display, one drawn through the anchor point and the other
C through the moving cursor position. This could be used to select
C a Y-axis range when one end of the range is known.
C -- If MODE=4, two vertical lines are extended over the height of
C the display, one drawn through the anchor point and the other
C through the moving cursor position. This could be used to select an
C X-axis range when one end of the range is known.
C -- If MODE=5, a horizontal line is extended through the cursor
C position over the width of the display. This could be used to select
C an X-axis value such as the start of an X-axis range. The anchor point
C is ignored.
C -- If MODE=6, a vertical line is extended through the cursor
C position over the height of the display. This could be used to select
C a Y-axis value such as the start of a Y-axis range. The anchor point
C is ignored.
C -- If MODE=7, a cross-hair, centered on the cursor, is extended over
C the width and height of the display. The anchor point is ignored.
C
C Returns:
C  PGBAND          : 1 if the call was successful; 0 if the device
C                    has no cursor or some other error occurs.
C Arguments:
C  MODE   (input)  : display mode (0, 1, ..7: see above).
C  POSN   (input)  : if POSN=1, PGBAND attempts to place the cursor
C                    at point (X,Y); if POSN=0, it leaves the cursor
C                    at its current position. (On some devices this
C                    request may be ignored.)
C  XREF   (input)  : the world x-coordinate of the anchor point.
C  YREF   (input)  : the world y-coordinate of the anchor point.
C  X      (in/out) : the world x-coordinate of the cursor.
C  Y      (in/out) : the world y-coordinate of the cursor.
C  CH     (output) : the character typed by the user; if the device has
C                    no cursor or if some other error occurs, the value
C                    CHAR(0) [ASCII NUL character] is returned.
C
C Note: The cursor coordinates (X,Y) may be changed by PGBAND even if
C the device has no cursor or if the user does not move the cursor.
C Under these circumstances, the position returned in (X,Y) is that of
C the pixel nearest to the requested position.
C--
C 7-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      INTEGER      GRCURS, I, J, IREF, JREF
      LOGICAL      PGNOTO
C
      IF (PGNOTO('PGBAND')) THEN
          CH = CHAR(0)
          PGBAND = 0
          RETURN
      END IF
      IF (MODE.LT.0 .OR. MODE.GT.7) CALL GRWARN(
     :     'Invalid MODE argument in PGBAND')
      IF (POSN.LT.0 .OR. POSN.GT.1) CALL GRWARN(
     :     'Invalid POSN argument in PGBAND')
C
      I = NINT(PGXORG(PGID) + X*PGXSCL(PGID))
      J = NINT(PGYORG(PGID) + Y*PGYSCL(PGID))
      IREF = NINT(PGXORG(PGID) + XREF*PGXSCL(PGID))
      JREF = NINT(PGYORG(PGID) + YREF*PGYSCL(PGID))
      PGBAND = GRCURS(PGID,I,J,IREF,JREF,MODE,POSN,CH)
      X = (I - PGXORG(PGID))/PGXSCL(PGID)
      Y = (J - PGYORG(PGID))/PGYSCL(PGID)
      CALL GRTERM
      END
C*PGBBUF -- begin batch of output (buffer)
C%void cpgbbuf(void);
C+
      SUBROUTINE PGBBUF
C
C Begin saving graphical output commands in an internal buffer; the
C commands are held until a matching PGEBUF call (or until the buffer
C is emptied by PGUPDT). This can greatly improve the efficiency of
C PGPLOT.  PGBBUF increments an internal counter, while PGEBUF
C decrements this counter and flushes the buffer to the output
C device when the counter drops to zero.  PGBBUF and PGEBUF calls
C should always be paired.
C
C Arguments: none
C--
C 21-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (.NOT.PGNOTO('PGBBUF')) THEN
          PGBLEV(PGID) = PGBLEV(PGID) + 1
      END IF
      END
C*PGBEG -- open a graphics device
C%int cpgbeg(int unit, const char *file, int nxsub, int nysub);
C+
      INTEGER FUNCTION PGBEG (UNIT, FILE, NXSUB, NYSUB)
      INTEGER       UNIT
      CHARACTER*(*) FILE
      INTEGER       NXSUB, NYSUB
C
C Note: new programs should use PGOPEN rather than PGBEG. PGOPEN
C is retained for compatibility with existing programs. Unlike PGOPEN,
C PGBEG closes any graphics devices that are already open, so it 
C cannot be used to open devices to be used in parallel.
C
C PGBEG opens a graphical device or file and prepares it for
C subsequent plotting. A device must be opened with PGBEG or PGOPEN
C before any other calls to PGPLOT subroutines for the device.
C
C If any device  is already open for PGPLOT output, it is closed before
C the new device is opened.
C
C Returns:
C  PGBEG         : a status return value. A value of 1 indicates
C                    successful completion, any other value indicates
C                    an error. In the event of error a message is
C                    written on the standard error unit.  
C                    To test the return value, call
C                    PGBEG as a function, eg IER=PGBEG(...); note
C                    that PGBEG must be declared INTEGER in the
C                    calling program. Some Fortran compilers allow
C                    you to use CALL PGBEG(...) and discard the
C                    return value, but this is not standard Fortran.
C Arguments:
C  UNIT  (input)   : this argument is ignored by PGBEG (use zero).
C  FILE  (input)   : the "device specification" for the plot device.
C                    (For explanation, see description of PGOPEN.)
C  NXSUB  (input)  : the number of subdivisions of the view surface in
C                    X (>0 or <0).
C  NYSUB  (input)  : the number of subdivisions of the view surface in
C                    Y (>0).
C                    PGPLOT puts NXSUB x NYSUB graphs on each plot
C                    page or screen; when the view surface is sub-
C                    divided in this way, PGPAGE moves to the next
C                    panel, not the  next physical page. If
C                    NXSUB > 0, PGPLOT uses the panels in row
C                    order; if <0, PGPLOT uses them in column order.
C--
C 21-Dec-1995 [TJP] - changed for multiple devices; call PGOPEN.
C 27-Feb-1997 [TJP] - updated description.
C-----------------------------------------------------------------------
      INTEGER       IER
      INTEGER       PGOPEN
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Close the plot-file if it is already open.
C
      CALL PGEND
C
C Call PGOPEN to open the device.
C
      IER = PGOPEN(FILE)
      IF (IER.GT.0) THEN
         CALL PGSUBP(NXSUB, NYSUB)
         PGBEG = 1
      ELSE
         PGBEG = IER
      END IF
C
      RETURN
      END
C*PGBEGIN -- non-standard alias for PGBEG
C+
      INTEGER FUNCTION PGBEGIN (UNIT, FILE, NXSUB, NYSUB)
      INTEGER       UNIT
      CHARACTER*(*) FILE
      INTEGER       NXSUB, NYSUB
C
C See description of PGBEG.   
C--
      INTEGER       PGBEG
      PGBEGIN = PGBEG (UNIT, FILE, NXSUB, NYSUB)
      END
C*PGBIN -- histogram of binned data
C%void cpgbin(int nbin, const float *x, const float *data, \
C% Logical center);
C+
      SUBROUTINE PGBIN (NBIN, X, DATA, CENTER)
      INTEGER NBIN
      REAL X(*), DATA(*)
      LOGICAL CENTER
C
C Plot a histogram of NBIN values with X(1..NBIN) values along
C the ordinate, and DATA(1...NBIN) along the abscissa. Bin width is
C spacing between X values.
C
C Arguments:
C  NBIN   (input)  : number of values.
C  X      (input)  : abscissae of bins.
C  DATA   (input)  : data values of bins.
C  CENTER (input)  : if .TRUE., the X values denote the center of the
C                    bin; if .FALSE., the X values denote the lower
C                    edge (in X) of the bin.
C--
C 19-Aug-92: change argument check (TJP).
C-----------------------------------------------------------------------
      LOGICAL  PGNOTO
      INTEGER  IBIN
      REAL     TX(4), TY(4)
C
C Check arguments.
C
      IF (NBIN.LT.2) RETURN
      IF (PGNOTO('PGBIN')) RETURN
      CALL PGBBUF
C
C Draw Histogram. Centered an uncentered bins are treated separately.
C
      IF (CENTER) THEN
C         !set up initial point.
          TX(2) = (3.*X(1) - X(2))/2.
          TY(2) = DATA(1)
          TX(3) = (X(1) + X(2))/2.
          TY(3) = TY(2)
          CALL GRVCT0(2, .FALSE., 2, TX(2), TY(2))
C         !draw initial horizontal line
C         !now loop over bins
          DO 10 IBIN=2,NBIN-1
              TX(1) = TX(3)
              TX(2) = TX(1)
              TX(3) = ( X(IBIN) + X(IBIN+1) ) / 2.
              TY(1) = TY(3)
              TY(2) = DATA(IBIN)
              TY(3) = TY(2)
              CALL GRVCT0(2, .FALSE., 3, TX, TY)
   10     CONTINUE
C         !now draw last segment.
          TX(1) = TX(3)
          TX(2) = TX(1)
          TX(3) = (3.*X(NBIN) - X(NBIN-1) )/2.
          TY(1) = TY(3)
          TY(2) = DATA(NBIN)
          TY(3) = TY(2)
          CALL GRVCT0(2, .FALSE., 3, TX, TY)
C
C               Uncentered bins
C
      ELSE
C         !set up first line.
          TX(2) = X(1)
          TY(2) = DATA(1)
          TX(3) = X(2)
          TY(3) = TY(2)
          CALL GRVCT0(2, .FALSE., 2, TX(2), TY(2))
          DO 20 IBIN=2,NBIN
              TX(1) = TX(3)
              TX(2) = TX(1)
              IF (IBIN.EQ.NBIN) THEN
                  TX(3) = 2.*X(NBIN) - X(NBIN-1)
              ELSE
                  TX(3) = X(IBIN+1)
              END IF
              TY(1) = TY(3)
C             !get height for last segment.
              TY(2) = DATA(IBIN)
              TY(3) = TY(2)
              CALL GRVCT0(2, .FALSE., 3, TX, TY)
   20     CONTINUE
      END IF
C
      CALL PGEBUF
      END
C PGBOX1 -- support routine for PGBOX
C
      SUBROUTINE PGBOX1 (XA, XB, XD, I1, I2)
      REAL XA, XB, XD
      INTEGER I1, I2
C
C This routine is used to determine where to draw the tick marks on
C an axis. The input arguments XA and XB are the world-coordinate
C end points of the axis; XD is the tick interval. PGBOX1 returns
C two integers, I1 and I2, such that the required tick marks are
C to be placed at world-coordinates (I*XD), for I=I1,...,I2.
C Normally I2 is greater than or equal to I1, but if there are no
C values of I such that I*XD lies in the inclusive range (XA, XB),
C then I2 will be 1 less than I1.
C
C Arguments:
C  XA, XB (input)  : world-coordinate end points of the axis. XA must
C                    not be equal to XB; otherwise, there are no
C                    restrictions.
C  XD     (input)  : world-coordinate tick interval. XD may be positive
C                    or negative, but may not be zero.
C  I1, I2 (output) : tick marks should be drawn at world
C                    coordinates I*XD for I in the inclusive range
C                    I1...I2 (see above).
C
C 14-Jan-1986 - new routine [TJP].
C 13-Dec-1990 - remove rror check [TJP].
C-----------------------------------------------------------------------
      REAL XLO, XHI
C
      XLO = MIN(XA/XD, XB/XD)
      XHI = MAX(XA/XD, XB/XD)
      I1 = NINT(XLO)
      IF (I1.LT.XLO) I1 = I1+1
      I2 = NINT(XHI)
      IF (I2.GT.XHI) I2 = I2-1
      END
C*PGBOX -- draw labeled frame around viewport
C%void cpgbox(const char *xopt, float xtick, int nxsub, \
C% const char *yopt, float ytick, int nysub);
C+
      SUBROUTINE PGBOX (XOPT, XTICK, NXSUB, YOPT, YTICK, NYSUB)
      CHARACTER*(*) XOPT, YOPT
      REAL XTICK, YTICK
      INTEGER NXSUB, NYSUB
C
C Annotate the viewport with frame, axes, numeric labels, etc.
C PGBOX is called by on the user's behalf by PGENV, but may also be
C called explicitly.
C
C Arguments:
C  XOPT   (input)  : string of options for X (horizontal) axis of
C                    plot. Options are single letters, and may be in
C                    any order (see below).
C  XTICK  (input)  : world coordinate interval between major tick marks
C                    on X axis. If XTICK=0.0, the interval is chosen by
C                    PGBOX, so that there will be at least 3 major tick
C                    marks along the axis.
C  NXSUB  (input)  : the number of subintervals to divide the major
C                    coordinate interval into. If XTICK=0.0 or NXSUB=0,
C                    the number is chosen by PGBOX.
C  YOPT   (input)  : string of options for Y (vertical) axis of plot.
C                    Coding is the same as for XOPT.
C  YTICK  (input)  : like XTICK for the Y axis.
C  NYSUB  (input)  : like NXSUB for the Y axis.
C
C Options (for parameters XOPT and YOPT):
C  A : draw Axis (X axis is horizontal line Y=0, Y axis is vertical
C      line X=0).
C  B : draw bottom (X) or left (Y) edge of frame.
C  C : draw top (X) or right (Y) edge of frame.
C  G : draw Grid of vertical (X) or horizontal (Y) lines.
C  I : Invert the tick marks; ie draw them outside the viewport
C      instead of inside.
C  L : label axis Logarithmically (see below).
C  N : write Numeric labels in the conventional location below the
C      viewport (X) or to the left of the viewport (Y).
C  P : extend ("Project") major tick marks outside the box (ignored if
C      option I is specified).
C  M : write numeric labels in the unconventional location above the
C      viewport (X) or to the right of the viewport (Y).
C  T : draw major Tick marks at the major coordinate interval.
C  S : draw minor tick marks (Subticks).
C  V : orient numeric labels Vertically. This is only applicable to Y.
C      The default is to write Y-labels parallel to the axis.
C  1 : force decimal labelling, instead of automatic choice (see PGNUMB).
C  2 : force exponential labelling, instead of automatic.
C
C To get a complete frame, specify BC in both XOPT and YOPT.
C Tick marks, if requested, are drawn on the axes or frame
C or both, depending which are requested. If none of ABC is specified,
C tick marks will not be drawn. When PGENV calls PGBOX, it sets both
C XOPT and YOPT according to the value of its parameter AXIS:
C -1: 'BC', 0: 'BCNST', 1: 'ABCNST', 2: 'ABCGNST'.
C
C For a logarithmic axis, the major tick interval is always 1.0. The
C numeric label is 10**(x) where x is the world coordinate at the
C tick mark. If subticks are requested, 8 subticks are drawn between
C each major tick at equal logarithmic intervals.
C
C To label an axis with time (days, hours, minutes, seconds) or
C angle (degrees, arcmin, arcsec), use routine PGTBOX.
C--
C 19-Oct-1983
C 23-Sep-1984 - fix bug in labelling reversed logarithmic axes.
C  6-May-1985 - improve behavior for pen plotters [TJP].
C 23-Nov-1985 - add 'P' option [TJP].
C 14-Jan-1986 - use new routine PGBOX1 to fix problem of missing
C               labels at end of axis [TJP].
C  8-Apr-1987 - improve automatic choice of tick interval; improve
C               erroneous rounding of tick interval to 1 digit [TJP].
C 23-Apr-1987 - fix bug: limit max number of ticks to ~10 [TJP].
C  7-Nov-1987 - yet another change to algorithm for choosing tick
C               interval; maximum tick interval is now 0.2*range of
C               axis, which may round up to 0.5 [TJP].
C 15-Dec-1988 - correct declaration of MAJOR [TJP].
C  6-Sep-1989 - use Fortran generic intrinsic functions [TJP].
C 18-Oct-1990 - correctly initialize UTAB(1) [AFT].
C 19-Oct-1990 - do all plotting in world coordinates [TJP].
C  6-Nov-1991 - label logarithmic subticks when necessary [TJP].
C  4-Jul-1994 - add '1' and '2' options [TJP].
C 20-Apr-1995 - adjust position of labels slightly, and move out
C               when ticks are inverted [TJP].
C 26-Feb-1997 - use new routine pgclp [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      CHARACTER*20  CLBL
      CHARACTER*64  OPT
      LOGICAL  XOPTA, XOPTB, XOPTC, XOPTG, XOPTN, XOPTM, XOPTT, XOPTS
      LOGICAL  YOPTA, YOPTB, YOPTC, YOPTG, YOPTN, YOPTM, YOPTT, YOPTS
      LOGICAL  XOPTI, YOPTI, YOPTV, XOPTL, YOPTL, XOPTP, YOPTP, lRANGE
      LOGICAL  IRANGE, MAJOR, XOPTLS, YOPTLS, PGNOTO
      REAL     TAB(9), UTAB(9)
      INTEGER  I, I1, I2, J, NC, NP, NV, KI, CLIP
      INTEGER  NSUBX, NSUBY, JMAX, XNFORM, YNFORM
      REAL     TIKL, TIKL1, TIKL2, XC, YC
      REAL     XINT, XINT2, XVAL, YINT, YINT2, YVAL
      REAL     PGRND
      REAL     A, B, C
      REAL     XNDSP, XMDSP, YNDSP, YMDSP, YNVDSP, YMVDSP
      REAL     XBLC, XTRC, YBLC, YTRC
      INTRINSIC ABS, INDEX, INT, LOG10, MAX, MIN, MOD, NINT, SIGN, REAL
C
C Table of logarithms 1..9
C
      DATA TAB / 0.00000, 0.30103, 0.47712, 0.60206, 0.69897,
     1           0.77815, 0.84510, 0.90309, 0.95424 /
C
      lRANGE(A,B,C) = (A.LT.B.AND.B.LT.C) .OR. (C.LT.B.AND.B.LT.A)
      IRANGE(A,B,C) = (A.LE.B.AND.B.LE.C) .OR. (C.LE.B.AND.B.LE.A)
C
      IF (PGNOTO('PGBOX')) RETURN
      CALL PGBBUF
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
C
C Decode options.
C
      CALL GRTOUP(OPT,XOPT)
      XOPTA = INDEX(OPT,'A').NE.0 .AND. lRANGE(YBLC,0.0,YTRC)
      XOPTB = INDEX(OPT,'B').NE.0
      XOPTC = INDEX(OPT,'C').NE.0
      XOPTG = INDEX(OPT,'G').NE.0
      XOPTI = INDEX(OPT,'I').NE.0
      XOPTL = INDEX(OPT,'L').NE.0
      XOPTM = INDEX(OPT,'M').NE.0
      XOPTN = INDEX(OPT,'N').NE.0
      XOPTS = INDEX(OPT,'S').NE.0
      XOPTT = INDEX(OPT,'T').NE.0
      XOPTP = INDEX(OPT,'P').NE.0 .AND. (.NOT.XOPTI)
      XNFORM = 0
      IF (INDEX(OPT,'1').NE.0) XNFORM = 1
      IF (INDEX(OPT,'2').NE.0) XNFORM = 2
      CALL GRTOUP(OPT,YOPT)
      YOPTA = INDEX(OPT,'A').NE.0 .AND. lRANGE(XBLC,0.0,XTRC)
      YOPTB = INDEX(OPT,'B').NE.0
      YOPTC = INDEX(OPT,'C').NE.0
      YOPTG = INDEX(OPT,'G').NE.0
      YOPTI = INDEX(OPT,'I').NE.0
      YOPTL = INDEX(OPT,'L').NE.0
      YOPTN = INDEX(OPT,'N').NE.0
      YOPTM = INDEX(OPT,'M').NE.0
      YOPTS = INDEX(OPT,'S').NE.0
      YOPTT = INDEX(OPT,'T').NE.0
      YOPTV = INDEX(OPT,'V').NE.0
      YOPTP = INDEX(OPT,'P').NE.0 .AND. (.NOT.YOPTI)
      YNFORM = 0
      IF (INDEX(OPT,'1').NE.0) YNFORM = 1
      IF (INDEX(OPT,'2').NE.0) YNFORM = 2
C
C Displacement of labels from edge of box
C (for X bottom/top, Y left/right, and Y left/right with V option).
C
      XNDSP = 1.2
      XMDSP = 0.7
      YNDSP = 0.7
      YMDSP = 1.2
      YNVDSP = 0.7
      YMVDSP = 0.7
      IF (XOPTI) THEN
         XNDSP = XNDSP + 0.3
         XMDSP = XMDSP + 0.3
      END IF
      IF (YOPTI) THEN
         YNDSP = YNDSP + 0.3
         YMDSP = YMDSP + 0.3
         YNVDSP = YNVDSP + 0.3
         YMVDSP = YMVDSP + 0.3
      END IF
C
C Disable clipping.
C
      CALL PGQCLP(CLIP)
      CALL PGSCLP(0)
C
C Draw box.
C
      IF (XOPTB) THEN
          CALL GRMOVA(XBLC, YBLC)
          CALL GRLINA(XTRC, YBLC)
      END IF
      IF (YOPTC) THEN
          CALL GRMOVA(XTRC, YBLC)
          CALL GRLINA(XTRC, YTRC)
      END IF
      IF (XOPTC) THEN
          CALL GRMOVA(XTRC, YTRC)
          CALL GRLINA(XBLC, YTRC)
      END IF
      IF (YOPTB) THEN
          CALL GRMOVA(XBLC, YTRC)
          CALL GRLINA(XBLC, YBLC)
      END IF
C
C Draw axes if required.
C
      IF (XOPTA.AND..NOT.XOPTG) THEN
          CALL GRMOVA(XBLC, 0.0)
          CALL GRLINA(XTRC, 0.0)
      END IF
      IF (YOPTA.AND..NOT.YOPTG) THEN
          CALL GRMOVA(0.0, YBLC)
          CALL GRLINA(0.0, YTRC)
      END IF
C
C Length of X tick marks.
C
      TIKL1 = PGXSP(PGID)*0.6*(YTRC-YBLC)/PGYLEN(PGID)
      IF (XOPTI) TIKL1 = -TIKL1
      TIKL2 = TIKL1*0.5
C
C Choose X tick intervals. Major interval = XINT,
C minor interval = XINT2 = XINT/NSUBX.
C
      UTAB(1) = 0.0
      IF (XOPTL) THEN
          XINT = SIGN(1.0,XTRC-XBLC)
          NSUBX = 1
          DO 10 J=2,9
              UTAB(J) = TAB(J)
              IF (XINT.LT.0.0) UTAB(J) = 1.0-TAB(J)
   10     CONTINUE
      ELSE IF (XTICK.EQ.0.0) THEN
          XINT = MAX(0.05, MIN(7.0*PGXSP(PGID)/PGXLEN(PGID), 0.20))
     1           *(XTRC-XBLC)
          XINT = PGRND(XINT,NSUBX)
      ELSE
          XINT = SIGN(XTICK,XTRC-XBLC)
          NSUBX = MAX(NXSUB,1)
      END IF
      IF (.NOT.XOPTS) NSUBX = 1
      NP = INT(LOG10(ABS(XINT)))-4
      NV = NINT(XINT/10.**NP)
      XINT2 = XINT/NSUBX
      XOPTLS = XOPTL .AND. XOPTS .AND. (ABS(XTRC-XBLC).LT.2.0)
C
C Draw X grid.
C
      IF (XOPTG) THEN
          CALL PGBOX1(XBLC, XTRC, XINT, I1, I2)
          DO 20 I=I1,I2
              CALL GRMOVA(REAL(I)*XINT, YBLC)
              CALL GRLINA(REAL(I)*XINT, YTRC)
   20     CONTINUE
      END IF
C
C Draw X ticks.
C
      IF (XOPTT.OR.XOPTS) THEN
          CALL PGBOX1(XBLC, XTRC, XINT2, I1, I2)
          JMAX = 1
          IF (XOPTL.AND.XOPTS) JMAX=9
C
C         Bottom ticks.
C
          IF (XOPTB) THEN
            DO 40 I=I1-1,I2
            DO 30 J=1,JMAX
                MAJOR = (MOD(I,NSUBX).EQ.0).AND.XOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                XVAL = (I+UTAB(J))*XINT2
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    IF (XOPTP.AND.MAJOR) THEN
                        CALL GRMOVA(XVAL, YBLC-TIKL2)
                    ELSE
                        CALL GRMOVA(XVAL, YBLC)
                    END IF
                    CALL GRLINA(XVAL, YBLC+TIKL)
                END IF
   30        CONTINUE
   40       CONTINUE
          END IF
C
C         Axis ticks.
C
          IF (XOPTA) THEN
            DO 60 I=I1-1,I2
            DO 50 J=1,JMAX
                MAJOR = (MOD(I,NSUBX).EQ.0).AND.XOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                XVAL = (I+UTAB(J))*XINT2
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    CALL GRMOVA(XVAL, -TIKL)
                    CALL GRLINA(XVAL, TIKL)
                END IF
   50       CONTINUE
   60       CONTINUE
          END IF
C
C         Top ticks.
C
          IF (XOPTC) THEN
            DO 80 I=I1-1,I2
            DO 70 J=1,JMAX
                MAJOR = (MOD(I,NSUBX).EQ.0).AND.XOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                XVAL = (I+UTAB(J))*XINT2
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    CALL GRMOVA(XVAL, YTRC-TIKL)
                    CALL GRLINA(XVAL, YTRC)
                END IF
   70       CONTINUE
   80       CONTINUE
          END IF
      END IF
C
C Write X labels.
C
      IF (XOPTN .OR. XOPTM) THEN
          CALL PGBOX1(XBLC, XTRC, XINT, I1, I2)
          DO 90 I=I1,I2
              XC = (I*XINT-XBLC)/(XTRC-XBLC)
              IF (XOPTL) THEN
                  CALL PGNUMB(1,NINT(I*XINT),XNFORM,CLBL,NC)
              ELSE
                  CALL PGNUMB(I*NV,NP,XNFORM,CLBL,NC)
              END IF
              IF (XOPTN) CALL PGMTXT('B', XNDSP, XC, 0.5, CLBL(1:NC))
              IF (XOPTM) CALL PGMTXT('T', XMDSP, XC, 0.5, CLBL(1:NC))
   90     CONTINUE
      END IF
C
C Extra X labels for log axes.
C
      IF (XOPTLS) THEN
          CALL PGBOX1(XBLC, XTRC, XINT2, I1, I2)
          DO 401 I=I1-1,I2
             DO 301 J=2,5,3
                XVAL = (I+UTAB(J))*XINT2
                XC = (XVAL-XBLC)/(XTRC-XBLC)
                KI = I
                IF (XTRC.LT.XBLC) KI = KI+1
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    CALL PGNUMB(J,NINT(KI*XINT2),XNFORM,CLBL,NC)
                    IF (XOPTN) 
     1                CALL PGMTXT('B', XNDSP, XC, 0.5, CLBL(1:NC))
                    IF (XOPTM) 
     1                CALL PGMTXT('T', XMDSP, XC, 0.5, CLBL(1:NC))
                END IF
  301       CONTINUE
  401     CONTINUE
      END IF
C
C Length of Y tick marks.
C
      TIKL1 = PGXSP(PGID)*0.6*(XTRC-XBLC)/PGXLEN(PGID)
      IF (YOPTI) TIKL1 = -TIKL1
      TIKL2 = TIKL1*0.5
C
C Choose Y tick intervals. Major interval = YINT,
C minor interval = YINT2 = YINT/NSUBY.
C
      UTAB(1) = 0.0
      IF (YOPTL) THEN
          YINT = SIGN(1.0,YTRC-YBLC)
          NSUBY = 1
          DO 100 J=2,9
              UTAB(J) = TAB(J)
              IF (YINT.LT.0.0) UTAB(J) = 1.0-TAB(J)
  100     CONTINUE
      ELSE IF (YTICK.EQ.0.0) THEN
          YINT = MAX(0.05, MIN(7.0*PGXSP(PGID)/PGYLEN(PGID), 0.20))
     1           *(YTRC-YBLC)
          YINT = PGRND(YINT,NSUBY)
      ELSE
          YINT  = SIGN(YTICK,YTRC-YBLC)
          NSUBY = MAX(NYSUB,1)
      END IF
      IF (.NOT.YOPTS) NSUBY = 1
      NP = INT(LOG10(ABS(YINT)))-4
      NV = NINT(YINT/10.**NP)
      YINT2 = YINT/NSUBY
      YOPTLS = YOPTL .AND. YOPTS .AND. (ABS(YTRC-YBLC).LT.2.0)
C
C Draw Y grid.
C
      IF (YOPTG) THEN
          CALL PGBOX1(YBLC, YTRC, YINT, I1, I2)
          DO 110 I=I1,I2
              CALL GRMOVA(XBLC, REAL(I)*YINT)
              CALL GRLINA(XTRC, REAL(I)*YINT)
  110     CONTINUE
      END IF
C
C Draw Y ticks.
C
      IF (YOPTT.OR.YOPTS) THEN
          CALL PGBOX1(YBLC, YTRC, YINT2, I1, I2)
          JMAX = 1
          IF (YOPTL.AND.YOPTS) JMAX = 9
C
C               Left ticks.
C
            IF (YOPTB) THEN
            DO 130 I=I1-1,I2
            DO 120 J=1,JMAX
                MAJOR = (MOD(I,NSUBY).EQ.0).AND.YOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                YVAL = (I+UTAB(J))*YINT2
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    IF (YOPTP.AND.MAJOR) THEN
                        CALL GRMOVA(XBLC-TIKL2, YVAL)
                    ELSE
                        CALL GRMOVA(XBLC, YVAL)
                    END IF
                    CALL GRLINA(XBLC+TIKL, YVAL)
                END IF
  120       CONTINUE
  130       CONTINUE
            END IF
C
C               Axis ticks.
C
            IF (YOPTA) THEN
            DO 150 I=I1-1,I2
            DO 140 J=1,JMAX
                MAJOR = (MOD(I,NSUBY).EQ.0).AND.YOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                YVAL = (I+UTAB(J))*YINT2
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    CALL GRMOVA(-TIKL, YVAL)
                    CALL GRLINA(TIKL, YVAL)
                END IF
  140       CONTINUE
  150       CONTINUE
            END IF
C
C               Right ticks.
C
            IF (YOPTC) THEN
            DO 170 I=I1-1,I2
            DO 160 J=1,JMAX
                MAJOR = (MOD(I,NSUBY).EQ.0).AND.YOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                YVAL = (I+UTAB(J))*YINT2
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    CALL GRMOVA(XTRC-TIKL, YVAL)
                    CALL GRLINA(XTRC, YVAL)
                END IF
  160       CONTINUE
  170       CONTINUE
            END IF
        END IF
C
C Write Y labels.
C
      IF (YOPTN.OR.YOPTM) THEN
          CALL PGBOX1(YBLC, YTRC, YINT, I1, I2)
          DO 180 I=I1,I2
              YC = (I*YINT-YBLC)/(YTRC-YBLC)
              IF (YOPTL) THEN
                  CALL PGNUMB(1,NINT(I*YINT),YNFORM,CLBL,NC)
              ELSE
                  CALL PGNUMB(I*NV,NP,YNFORM,CLBL,NC)
              END IF
              IF (YOPTV) THEN
                  IF (YOPTN) CALL PGMTXT('LV',YNVDSP,YC,1.0,CLBL(1:NC))
                  IF (YOPTM) CALL PGMTXT('RV',YMVDSP,YC,0.0,CLBL(1:NC))
              ELSE
                  IF (YOPTN) CALL PGMTXT('L',YNDSP,YC,0.5,CLBL(1:NC))
                  IF (YOPTM) CALL PGMTXT('R',YMDSP,YC,0.5,CLBL(1:NC))
              END IF
  180     CONTINUE
      END IF
C
C Extra Y labels for log axes.
C
      IF (YOPTLS) THEN
          CALL PGBOX1(YBLC, YTRC, YINT2, I1, I2)
          DO 402 I=I1-1,I2
            DO 302 J=2,5,3
                YVAL = (I+UTAB(J))*YINT2
                YC = (YVAL-YBLC)/(YTRC-YBLC)
                KI = I
                IF (YBLC.GT.YTRC) KI = KI+1
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    CALL PGNUMB(J,NINT(KI*YINT2),YNFORM,CLBL,NC)
                    IF (YOPTV) THEN
                    IF (YOPTN) 
     1                CALL PGMTXT('LV', YNVDSP, YC, 1.0, CLBL(1:NC))
                    IF (YOPTM) 
     1                CALL PGMTXT('RV', YMVDSP, YC, 0.0, CLBL(1:NC))
                    ELSE
                    IF (YOPTN) 
     1                CALL PGMTXT('L', YNDSP, YC, 0.5, CLBL(1:NC))
                    IF (YOPTM) 
     1                CALL PGMTXT('R', YMDSP, YC, 0.5, CLBL(1:NC))
                    END IF
                END IF
  302       CONTINUE
  402     CONTINUE
      END IF
C
C Enable clipping.
C
      CALL PGSCLP(CLIP)
C
      CALL PGEBUF
      END
C*PGCIRC -- draw a circle, using fill-area attributes
C%void cpgcirc(float xcent, float ycent, float radius);
C+
      SUBROUTINE PGCIRC (XCENT, YCENT, RADIUS)
      REAL XCENT, YCENT, RADIUS
C
C Draw a circle. The action of this routine depends
C on the setting of the Fill-Area Style attribute. If Fill-Area Style
C is SOLID (the default), the interior of the circle is solid-filled
C using the current Color Index. If Fill-Area Style is HOLLOW, the
C outline of the circle is drawn using the current line attributes
C (color index, line-style, and line-width).
C
C Arguments:
C  XCENT  (input)  : world x-coordinate of the center of the circle.
C  YCENT  (input)  : world y-coordinate of the center of the circle.
C  RADIUS (input)  : radius of circle (world coordinates).
C--
C 26-Nov-1992 - [TJP].
C 20-Sep-1994 - adjust number of points according to size [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER MAXPTS
      PARAMETER (MAXPTS=72)
C
      INTEGER NPTS,I,RADPIX
      REAL ANGLE
      REAL X(MAXPTS),Y(MAXPTS)
C
      RADPIX = NINT(RADIUS*MAX(PGXSCL(PGID), PGYSCL(PGID)))
      NPTS = MAX(8, MIN(MAXPTS, RADPIX))
      DO 10 I=1,NPTS
         ANGLE = I*360.0/REAL(NPTS)/57.3
         X(I) = XCENT + RADIUS*COS(ANGLE)
         Y(I) = YCENT + RADIUS*SIN(ANGLE)
   10 CONTINUE
      CALL PGPOLY (NPTS,X,Y)
C     write (*,*) 'PGCIRC', NPTS
C-----------------------------------------------------------------------
      END
C
      SUBROUTINE PGCL (K, X, Y, Z)
      INTEGER K
      REAL X, Y, Z
C
C PGPLOT (internal routine): Label one contour segment (for use by
C PGCONX).
C
C Arguments:
C
C K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
C       a line from the current position to (X,Y); otherwise
C       do nothing.
C X (input, real): X world-coordinate of end point.
C Y (input, real): Y world-coordinate of end point.
C Z (input, real): the value of the contour level, not used by PGCL.
C--
C  5-May-1994 - new routine [TJP]
C  7-Mar-1995 - correct error in angle; do not draw labels outside
C               window [TJP].
C 28-Aug-1995 - check arguments of atan2 [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL     XX, YY, XC, YC, XV1, XV2, YV1, YV2, XL, XR, YB, YT
      REAL     XN, YN
      REAL     ANGLE, XO, YO, XP, YP, DINDX, DINDY, XBOX(4), YBOX(4)
      INTEGER  I, TB
      SAVE     I
      DATA     I /0/
C
C     -- transform to world coordinates
      XX = TRANS(1) + TRANS(2)*X + TRANS(3)*Y
      YY = TRANS(4) + TRANS(5)*X + TRANS(6)*Y
C
      IF (K.EQ.0) THEN
C        -- start of contour: reset segment counter
         I = 0
      ELSE
C        -- increment segment counter and check whether this
C           segment should be labelled
         I = MOD(I+1,PGCINT)
         IF (I.EQ.PGCMIN) THEN
C           -- find center of this segment (XC, YC)
            CALL PGQPOS(XP, YP)
            XC = (XX+XP)*0.5
            YC = (YY+YP)*0.5
C            -- find slope of this segment (ANGLE)
            CALL PGQVP(1, XV1, XV2, YV1, YV2)
            CALL PGQWIN(XL, XR, YB, YT)
            ANGLE = 0.0
            IF (XR.NE.XL .AND. YT.NE.YB) THEN
               DINDX = (XV2 - XV1) / (XR - XL)
               DINDY = (YV2 - YV1) / (YT - YB)
               IF (YY-YP.NE.0.0 .OR. XX-XP.NE.0.0)
     :           ANGLE = 57.3*ATAN2((YY-YP)*DINDY, (XX-XP)*DINDX)
            END IF
C           -- check whether point is in window
            XN = (XC-XL)/(XR-XL)
            YN = (YC-YB)/(YT-YB)
            IF (XN.GE.0.0 .AND. XN.LE.1.0 .AND.
     :          YN.GE.0.0 .AND. YN.LE.1.0) THEN
C              -- save current text background and set to erase
               CALL PGQTBG(TB)
               CALL PGSTBG(0)
C              -- find bounding box of label
               CALL PGQTXT(XC, YC, ANGLE, 0.5, PGCLAB, XBOX, YBOX)
               XO = 0.5*(XBOX(1)+XBOX(3))
               YO = 0.5*(YBOX(1)+YBOX(3))
C              -- plot label with bounding box centered at (XC, YC)
               CALL PGPTXT(2.0*XC-XO, 2.0*YC-YO, ANGLE, 0.5, PGCLAB)
C              -- restore text background
               CALL PGSTBG(TB)
            END IF
         END IF
      END IF
      CALL PGMOVE(XX,YY)
      END
C*PGCLOS -- close the selected graphics device
C%void cpgclos(void);
C+
      SUBROUTINE PGCLOS
C
C Close the currently selected graphics device. After the device has
C been closed, either another open device must be selected with PGSLCT
C or another device must be opened with PGOPEN before any further
C plotting can be done. If the call to PGCLOS is omitted, some or all 
C of the plot may be lost.
C
C [This routine was added to PGPLOT in Version 5.1.0. Older programs
C use PGEND instead.]
C
C Arguments: none
C--
C 22-Dec-1995 - new routine, derived from the old PGEND.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      CHARACTER*16 DEFSTR
      LOGICAL PGNOTO
C
      IF (.NOT.PGNOTO('PGCLOS')) THEN
         CALL GRTERM
         IF (PGPRMP(PGID)) THEN
            CALL GRQCAP(DEFSTR)
            IF (DEFSTR(8:8).EQ.'V') CALL GRPROM
         END IF
         CALL GRCLOS
         PGDEVS(PGID) = 0
         PGID = 0
      END IF
C     WRITE (*,*) 'PGCLOS', PGID, ':', PGDEVS
      END
      SUBROUTINE PGCN01(Z, MX, MY, IA, IB, JA, JB, Z0, PLOT,
     1                  FLAGS, IS, JS, SDIR)
C
C Support routine for PGCNSC. This routine draws a continuous contour,
C starting at the specified point, until it either crosses the edge of
C the array or closes on itself.
C-----------------------------------------------------------------------
      INTEGER UP, DOWN, LEFT, RIGHT
      PARAMETER (UP=1, DOWN=2, LEFT=3, RIGHT=4)
      INTEGER  MAXEMX, MAXEMY
      PARAMETER (MAXEMX=100, MAXEMY=100)
      LOGICAL FLAGS(MAXEMX,MAXEMY,2)
      INTEGER MX, MY, IA, IB, JA, JB, IS, JS, I, J, II, JJ, DIR, SDIR
      REAL Z(MX,*)
      REAL Z0, X, Y, STARTX, STARTY
      EXTERNAL PLOT
C
      I = IS
      J = JS
      DIR = SDIR
      II = 1+I-IA
      JJ = 1+J-JA
      IF (DIR.EQ.UP .OR. DIR.EQ.DOWN) THEN
          X = REAL(I) + (Z0-Z(I,J))/(Z(I+1,J)-Z(I,J))
          Y = REAL(J)
      ELSE
          X = REAL(I)
          Y = REAL(J) + (Z0-Z(I,J))/(Z(I,J+1)-Z(I,J))
      END IF
CD    WRITE (*,*) 'SEGMENT'
C
C Move to start of contour and record starting point.
C
      CALL PLOT(0, X, Y, Z0)
      STARTX = X
      STARTY = Y
C
C We have reached grid-point (I,J) going in direction DIR (UP, DOWN,
C LEFT, or RIGHT). Look at the other three sides of the cell we are
C entering to decide where to go next. It is important to look to the
C two sides before looking straight ahead, in order to avoid self-
C intersecting contours. If all 3 sides have unused crossing-points,
C the cell is "degenerate" and we have to decide which of two possible 
C pairs of contour segments to draw; at present we make an arbitrary 
C choice. If we have reached the edge of the array, we have
C finished drawing an unclosed contour. If none of the other three
C sides of the cell have an unused crossing-point, we must have
C completed a closed contour, which requires a final segment back to
C the starting point.
C
  100 CONTINUE
CD    WRITE (*,*) I,J,DIR
      II = 1 + I - IA
      JJ = 1 + J - JA
      GOTO (110, 120, 130, 140), DIR
C
C DIR = UP
C
  110 CONTINUE
      FLAGS(II,JJ,1) = .FALSE.
      IF (J.EQ.JB) THEN
          RETURN
      ELSE IF (FLAGS(II,JJ,2)) THEN
          DIR = LEFT
          GOTO 200
      ELSE IF (FLAGS(II+1,JJ,2)) THEN
          DIR = RIGHT
          I = I+1
          GOTO 200
      ELSE IF (FLAGS(II,JJ+1,1)) THEN
C!        DIR = UP
          J = J+1
          GOTO 250
      ELSE
          GOTO 300
      END IF
C
C DIR = DOWN
C
  120 CONTINUE
      FLAGS(II,JJ,1) = .FALSE.
      IF (J.EQ.JA) THEN
          RETURN
      ELSE IF (FLAGS(II+1,JJ-1,2)) THEN
          DIR = RIGHT
          I = I+1
          J = J-1
          GOTO 200
      ELSE IF (FLAGS(II,JJ-1,2)) THEN
          DIR = LEFT
          J = J-1
          GOTO 200
      ELSE IF (FLAGS(II,JJ-1,1)) THEN
C!        DIR = DOWN
          J = J-1
          GOTO 250
      ELSE
          GOTO 300
      END IF
C
C DIR = LEFT
C
  130 CONTINUE
      FLAGS(II,JJ,2) = .FALSE.
      IF (I.EQ.IA) THEN
          RETURN
      ELSE IF (FLAGS(II-1,JJ,1)) THEN
          DIR = DOWN
          I = I-1
          GOTO 250
      ELSE IF (FLAGS(II-1,JJ+1,1)) THEN
          DIR = UP
          I = I-1
          J = J+1
          GOTO 250
      ELSE IF (FLAGS(II-1,JJ,2)) THEN
C!        DIR = LEFT
          I = I-1
          GOTO 200
      ELSE
          GOTO 300
      END IF
C
C DIR = RIGHT
C
  140 CONTINUE
      FLAGS(II,JJ,2) = .FALSE.
      IF (I.EQ.IB) THEN
          RETURN
      ELSE IF (FLAGS(II,JJ+1,1)) THEN
          DIR = UP
          J = J+1
          GOTO 250
      ELSE IF (FLAGS(II,JJ,1)) THEN
          DIR = DOWN
          GOTO 250
      ELSE IF (FLAGS(II+1,JJ,2)) THEN
C!        DIR = RIGHT
          I = I+1
          GOTO 200
      ELSE
          GOTO 300
      END IF
C
C Draw a segment of the contour.
C
  200 X = REAL(I)
      Y = REAL(J) + (Z0-Z(I,J))/(Z(I,J+1)-Z(I,J))
      CALL PLOT(1,X,Y,Z0)
      GOTO 100
  250 X = REAL(I) + (Z0-Z(I,J))/(Z(I+1,J)-Z(I,J))
      Y = REAL(J)
      CALL PLOT(1,X,Y,Z0)
      GOTO 100
C
C Close the contour and go look for another one.
C
  300 CALL PLOT(1,STARTX,STARTY,Z0)
      RETURN
C
      END
      SUBROUTINE PGCNSC (Z, MX, MY, IA, IB, JA, JB, Z0, PLOT)
      INTEGER MX, MY, IA, IB, JA, JB
      REAL Z(MX,*)
      REAL Z0
      EXTERNAL PLOT
C
C PGPLOT (internal routine): Draw a single contour.  This routine is
C called by PGCONT, but may be called directly by the user.
C
C Arguments:
C
C Z (real array dimension MX,MY, input): the array of function values.
C MX,MY (integer, input): actual declared dimension of Z(*,*).
C IA,IB (integer, input): inclusive range of the first index of Z to be
C       contoured.
C JA,JB (integer, input): inclusive range of the second index of Z to
C       be contoured.
C Z0 (real, input): the contour level sought.
C PLOT (the name of a subroutine declared EXTERNAL in the calling
C       routine): this routine is called by PGCNSC to do all graphical
C       output. The calling sequence is CALL PLOT(K,X,Y,Z) where Z is
C       the contour level, (X,Y) are the coordinates of a point (in the
C       inclusive range I1<X<I2, J1<Y<J2, and if K is 0, the routine is
C       to move then pen to (X,Y); if K is 1, it is to draw a line from
C       the current position to (X,Y).
C
C NOTE:  the intervals (IA,IB) and (JA,JB) must not exceed the
C dimensions of an internal array. These are currently set at 100.
C--
C 17-Sep-1989 - Completely rewritten [TJP]. The algorithm is my own,
C               but it is probably not original. It could probably be
C               coded more briefly, if not as clearly.
C  1-May-1994 - Modified to draw contours anticlockwise about maxima,
C               to prevent contours at different levels from
C               crossing in degenerate cells [TJP].
C-----------------------------------------------------------------------
      INTEGER UP, DOWN, LEFT, RIGHT
      PARAMETER (UP=1, DOWN=2, LEFT=3, RIGHT=4)
      INTEGER  MAXEMX, MAXEMY
      PARAMETER (MAXEMX=100, MAXEMY=100)
C
      LOGICAL FLAGS(MAXEMX,MAXEMY,2), lRANGE
      INTEGER I, J, II, JJ, DIR
      REAL Z1, Z2, Z3, P, P1, P2
C
C The statement function RANGE decides whether a contour at level P
C crosses the line between two gridpoints with values P1 and P2. It is
C important that a contour cannot cross a line with equal endpoints.
C
      lRANGE (P,P1,P2) = (P.GT.MIN(P1,P2)) .AND. (P.LE.MAX(P1,P2))
     1                  .AND. (P1.NE.P2)
C
C Check for errors.
C
      IF ( (IB-IA+1) .GT. MAXEMX .OR.  (JB-JA+1) .GT. MAXEMY ) THEN
          CALL GRWARN('PGCNSC - array index range exceeds'//
     1                ' built-in limit of 100')
          RETURN
      END IF
C
C Initialize the flags. The first flag for a gridpoint is set if
C the contour crosses the line segment to the right of the gridpoint
C (joining [I,J] to [I+1,J]); the second flag is set if if it crosses
C the line segment above the gridpoint (joining [I,J] to [I,J+1]).
C The top and right edges require special treatment. (For purposes
C of description only, we assume I increases horizontally to the right
C and J increases vertically upwards.)
C
      DO 20 I=IA,IB
          II = I-IA+1
          DO 10 J=JA,JB
              JJ = J-JA+1
              Z1 = Z(I,J)
              FLAGS(II,JJ,1) = .FALSE.
              FLAGS(II,JJ,2) = .FALSE.
              IF (I.LT.IB) THEN
                Z2 = Z(I+1,J)
                IF (lRANGE(Z0,Z1,Z2)) FLAGS(II,JJ,1) = .TRUE.
              END IF
              IF (J.LT.JB) THEN
                Z3 = Z(I,J+1)
                IF (lRANGE(Z0,Z1,Z3)) FLAGS(II,JJ,2) = .TRUE.
              END IF
   10     CONTINUE
   20 CONTINUE
C
C Search the edges of the array for the start of an unclosed contour.
C Note that (if the algorithm is implemented correctly) all unclosed
C contours must begin and end at the edge of the array. When one is
C found, call PGCN01 to draw the contour, telling it the correct
C starting direction so that it follows the contour into the array
C instead of out of it. A contour is only started if the higher
C ground lies to the left: this is to enforce the direction convention
C that contours are drawn anticlockwise around maxima. If the high
C ground lies to the right, we will find the other end of the contour
C and start there.
C
C Bottom edge.
C
      J = JA
      JJ = J-JA+1
      DO 26 I=IA,IB-1
          II = I-IA+1
          IF (FLAGS(II,JJ,1) .AND. (Z(I,J).GT.Z(I+1,J)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, UP)
   26 CONTINUE
C
C Right edge.
C
      I = IB
      II = I-IA+1
      DO 27 J=JA,JB-1
          JJ = J-JA+1
          IF (FLAGS(II,JJ,2) .AND. (Z(I,J).GT.Z(I,J+1)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, LEFT)
   27 CONTINUE
C
C Top edge.
C
      J = JB
      JJ = J-JA+1
      DO 28 I=IB-1,IA,-1
          II = I-IA+1
          IF (FLAGS(II,JJ,1) .AND. (Z(I+1,J).GT.Z(I,J)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, DOWN)
   28 CONTINUE
C
C Left edge.
C
      I = IA
      II = I-IA+1
      DO 29 J=JB-1,JA,-1
          JJ = J-JA+1
          IF (FLAGS(II,JJ,2)  .AND. (Z(I,J+1).GT.Z(I,J)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, RIGHT)
   29 CONTINUE
C
C Now search the interior of the array for a crossing point, which will
C lie on a closed contour (because all unclosed contours have been
C eliminated). It is sufficient to search just the horizontal crossings
C (or the vertical ones); any closed contour must cross a horizontal
C and a vertical gridline. PGCN01 assumes that when it cannot proceed
C any further, it has reached the end of a closed contour. Thus all
C unclosed contours must be eliminated first.
C
      DO 40 I=IA+1,IB-1
          II = I-IA+1
          DO 30 J=JA+1,JB-1
              JJ = J-JA+1
              IF (FLAGS(II,JJ,1)) THEN
                  DIR = UP
                  IF (Z(I+1,J).GT. Z(I,J)) DIR = DOWN
                  CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     1                        Z0, PLOT, FLAGS, I, J, DIR)

              END IF
   30     CONTINUE
   40 CONTINUE
C
C We didn't find any more crossing points: we're finished.
C
      RETURN
      END
C*PGCONB -- contour map of a 2D data array, with blanking
C%void cpgconb(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, const float *c, int nc, const float *tr, \
C% float blank);
C+
      SUBROUTINE PGCONB (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, TR, 
     1                   BLANK)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, NC
      REAL    A(IDIM,JDIM), C(*), TR(6), BLANK
C
C Draw a contour map of an array. This routine is the same as PGCONS,
C except that array elements that have the "magic value" defined by
C argument BLANK are ignored, making gaps in the contour map. The
C routine may be useful for data measured on most but not all of the
C points of a grid.
C
C Arguments:
C  A      (input)  : data array.
C  IDIM   (input)  : first dimension of A.
C  JDIM   (input)  : second dimension of A.
C  I1,I2  (input)  : range of first index to be contoured (inclusive).
C  J1,J2  (input)  : range of second index to be contoured (inclusive).
C  C      (input)  : array of contour levels (in the same units as the
C                    data in array A); dimension at least NC.
C  NC     (input)  : number of contour levels (less than or equal to
C                    dimension of C). The absolute value of this
C                    argument is used (for compatibility with PGCONT,
C                    where the sign of NC is significant).
C  TR     (input)  : array defining a transformation between the I,J
C                    grid of the array and the world coordinates. The
C                    world coordinates of the array point A(I,J) are
C                    given by:
C                      X = TR(1) + TR(2)*I + TR(3)*J
C                      Y = TR(4) + TR(5)*I + TR(6)*J
C                    Usually TR(3) and TR(5) are zero - unless the
C                    coordinate transformation involves a rotation
C                    or shear.
C  BLANK   (input) : elements of array A that are exactly equal to
C                    this value are ignored (blanked).
C--
C 21-Sep-1989 - Derived from PGCONS [TJP].
C-----------------------------------------------------------------------
      INTEGER  I, IC, ICORN, IDELT(6), J, K, NPT
      INTEGER  IOFF(8), JOFF(8), IENC, ITMP, JTMP, ILO, ITOT
      LOGICAL  PGNOTO
      REAL     CTR, DELTA, DVAL(5), XX, YY, X(4), Y(4)
      INTRINSIC ABS
      DATA     IDELT/0,-1,-1,0,0,-1/
      DATA     IOFF/-2,-2,-1,-1, 0, 0, 1, 1/
      DATA     JOFF/ 0,-1,-2, 1,-2, 1,-1, 0/
C
C Check arguments.
C
      IF (PGNOTO('PGCONB')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) RETURN
      IF (NC.EQ.0) RETURN
      CALL PGBBUF
C
      DO 130 J=J1+1,J2
      DO 130 I=I1+1,I2
          DVAL(1) = A(I-1,J)
          DVAL(2) = A(I-1,J-1)
          DVAL(3) = A(I,J-1)
          DVAL(4) = A(I,J)
          DVAL(5) = DVAL(1)
          IF (DVAL(1).EQ.BLANK .OR. DVAL(2).EQ.BLANK .OR.
     1        DVAL(3).EQ.BLANK .OR. DVAL(4).EQ.BLANK) GOTO 130
      DO 110 IC=1,ABS(NC)
          CTR = C(IC)
          NPT = 0
          DO 120 ICORN=1,4
          IF( (DVAL(ICORN).LT.CTR .AND. DVAL(ICORN+1).LT.CTR)
     1    .OR.(DVAL(ICORN).GE.CTR .AND. DVAL(ICORN+1).GE.CTR) ) GOTO 120
            NPT=NPT+1
            DELTA = (CTR-DVAL(ICORN))/(DVAL(ICORN+1)-DVAL(ICORN))
            GOTO (60,70,60,70), ICORN
C
   60       XX = I+IDELT(ICORN+1)
            YY = REAL(J+IDELT(ICORN)) + 
     1           DELTA*REAL(IDELT(ICORN+1)-IDELT(ICORN))
            GOTO 80
C
   70       XX = REAL(I+IDELT(ICORN+1)) +
     1           DELTA*REAL(IDELT(ICORN+2)-IDELT(ICORN+1))
            YY  = J+IDELT(ICORN)
C
   80       X(NPT) = TR(1) + TR(2)*XX + TR(3)*YY
            Y(NPT) = TR(4) + TR(5)*XX + TR(6)*YY
C
  120     CONTINUE
          IF (NPT.EQ.2) THEN
C             -- Contour crosses two sides of cell. Draw line-segment.
              CALL PGMOVE(X(1),Y(1))
              CALL PGDRAW(X(2),Y(2))
          ELSE IF (NPT.EQ.4) THEN
C             -- The 'ambiguous' case.  The routine must draw two line
C             segments here and there are two ways to do so.  The
C             following 4 lines would implement the original PGPLOT
C             method:
C            CALL PGCP(0,X(1),Y(1),CTR)
C            CALL PGCP(1,X(2),Y(2),CTR)
C            CALL PGCP(0,X(3),Y(3),CTR)
C            CALL PGCP(1,X(4),Y(4),CTR)
C            -- Choose between \\ and // based on the 8 points just
C            outside the current box.  If half or more of these points
C            lie below the contour level, then draw the lines such that
C            the high corners lie between the lines, otherwise, draw
C            the lines such that the low corners are enclosed.  Care is
C            taken to avoid going off the edge.
            ITOT=0
            ILO=0
            DO 140 K=1,8
               ITMP=I+IOFF(K)
               JTMP=J+JOFF(K)
               IF(ITMP.LT.I1 .OR. ITMP.GT.I2) GOTO 140
               IF(JTMP.LT.J1 .OR. JTMP.GT.J2) GOTO 140
               IF(A(ITMP,JTMP).EQ.BLANK) GOTO 140
               ITOT=ITOT+1
               IF(A(ITMP,JTMP).LT.CTR) ILO=ILO+1
  140       CONTINUE
            IENC=+1
            IF(ILO.LT.ITOT/2) IENC=-1
            IF(IENC.LT.0 .AND. DVAL(1).LT.CTR .OR.
     :         IENC.GT.0 .AND. DVAL(1).GE.CTR) THEN
               CALL PGMOVE(X(1),Y(1))
               CALL PGDRAW(X(2),Y(2))
               CALL PGMOVE(X(3),Y(3))
               CALL PGDRAW(X(4),Y(4))
            ELSE
               CALL PGMOVE(X(1),Y(1))
               CALL PGDRAW(X(4),Y(4))
               CALL PGMOVE(X(3),Y(3))
               CALL PGDRAW(X(2),Y(2))
            END IF
          END IF
  110     CONTINUE
  130 CONTINUE
C
      CALL PGEBUF
      END
C*PGCONF -- fill between two contours
C%void cpgconf(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float c1, float c2, const float *tr);
C+
      SUBROUTINE PGCONF (A, IDIM, JDIM, I1, I2, J1, J2, C1, C2, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM), C1, C2, TR(6)
C
C Shade the region between two contour levels of a function defined on
C the nodes of a rectangular grid. The routine uses the current fill
C attributes, hatching style (if appropriate), and color index.
C
C If you want to both shade between contours and draw the contour
C lines, call this routine first (once for each pair of levels) and 
C then CALL PGCONT (or PGCONS) to draw the contour lines on top of the
C shading.
C
C Note 1: This routine is not very efficient: it generates a polygon
C fill command for each cell of the mesh that intersects the desired
C area, rather than consolidating adjacent cells into a single polygon.
C
C Note 2: If both contours intersect all four edges of a particular
C mesh cell, the program behaves badly and may consider some parts
C of the cell to lie in more than one contour range.
C
C Note 3: If a contour crosses all four edges of a cell, this
C routine may not generate the same contours as PGCONT or PGCONS
C (these two routines may not agree either). Such cases are always
C ambiguous and the routines use different approaches to resolving
C the ambiguity.
C
C Arguments:
C  A      (input)  : data array.
C  IDIM   (input)  : first dimension of A.
C  JDIM   (input)  : second dimension of A.
C  I1,I2  (input)  : range of first index to be contoured (inclusive).
C  J1,J2  (input)  : range of second index to be contoured (inclusive).
C  C1, C2 (input)  : contour levels; note that C1 must be less than C2.
C  TR     (input)  : array defining a transformation between the I,J
C                    grid of the array and the world coordinates. The
C                    world coordinates of the array point A(I,J) are
C                    given by:
C                      X = TR(1) + TR(2)*I + TR(3)*J
C                      Y = TR(4) + TR(5)*I + TR(6)*J
C                    Usually TR(3) and TR(5) are zero - unless the
C                    coordinate transformation involves a rotation
C                    or shear.
C--
C 03-Oct-1996 - new routine [TJP].
C-----------------------------------------------------------------------
      INTEGER  I, J, IC, NPT, LEV
      LOGICAL  PGNOTO
      REAL     DVAL(5), X(8), Y(8), DELTA, XX, YY, C, R
      INTEGER  IDELT(6)
      DATA     IDELT/0,-1,-1,0,0,-1/
C
C Check arguments.
C
      IF (PGNOTO('PGCONF')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     :    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) RETURN
      IF (C1.GE.C2) RETURN
      CALL PGBBUF
C
      DO 140 J=J1+1,J2
         DO 130 I=I1+1,I2
            DVAL(1) = A(I-1,J)
            DVAL(2) = A(I-1,J-1)
            DVAL(3) = A(I,J-1)
            DVAL(4) = A(I,J)
            DVAL(5) = DVAL(1)
C
            NPT = 0
            DO 120 IC=1,4
               IF (DVAL(IC).GE.C1 .AND. DVAL(IC).LT.C2) THEN
                  NPT = NPT+1
                  XX = I+IDELT(IC+1)
                  YY = J+IDELT(IC)
                  X(NPT) = TR(1) + TR(2)*XX + TR(3)*YY
                  Y(NPT) = TR(4) + TR(5)*XX + TR(6)*YY
               END IF
               R = DVAL(IC+1)-DVAL(IC)
               IF (R.EQ.0.0) GOTO 120
               DO 110 LEV=1,2
                  IF (R.GT.0.0) THEN
                     C = C1
                     IF (LEV.EQ.2) C = C2
                  ELSE
                     C = C2
                     IF (LEV.EQ.2) C = C1
                  END IF
                  DELTA = (C-DVAL(IC))/R
                  IF (DELTA.GT.0.0 .AND. DELTA.LT.1.0) THEN
                     IF (IC.EQ.1 .OR. IC.EQ.3) THEN
                        XX = I+IDELT(IC+1)
                        YY = REAL(J+IDELT(IC)) + 
     :                       DELTA*REAL(IDELT(IC+1)-IDELT(IC))
                     ELSE
                        XX = REAL(I+IDELT(IC+1)) +
     :                       DELTA*REAL(IDELT(IC+2)-IDELT(IC+1))
                        YY = J+IDELT(IC)
                     END IF
                     NPT = NPT+1
                     X(NPT) = TR(1) + TR(2)*XX + TR(3)*YY
                     Y(NPT) = TR(4) + TR(5)*XX + TR(6)*YY
                  END IF
 110           CONTINUE
 120        CONTINUE
            IF (NPT.GE.3) CALL PGPOLY(NPT, X, Y)
 130     CONTINUE
 140  CONTINUE
      CALL PGEBUF
      END
C*PGCONL -- label contour map of a 2D data array 
C%void cpgconl(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float c, const float *tr, const char *label, \
C% int intval, int minint);
C+
      SUBROUTINE PGCONL (A, IDIM, JDIM, I1, I2, J1, J2, C, TR,
     1                   LABEL, INTVAL, MININT)
      INTEGER IDIM, JDIM, I1, J1, I2, J2, INTVAL, MININT
      REAL A(IDIM,JDIM), C, TR(6)
      CHARACTER*(*) LABEL
C
C Label a contour map drawn with routine PGCONT. Routine PGCONT should
C be called first to draw the contour lines, then this routine should be
C called to add the labels. Labels are written at intervals along the
C contour lines, centered on the contour lines with lettering aligned
C in the up-hill direction. Labels are opaque, so a part of the under-
C lying contour line is obscured by the label. Labels use the current
C attributes (character height, line width, color index, character
C font).
C
C The first 9 arguments are the same as those supplied to PGCONT, and
C should normally be identical to those used with PGCONT. Note that
C only one contour level can be specified; tolabel more contours, call
C PGCONL for each level.
C
C The Label is supplied as a character string in argument LABEL.
C
C The spacing of labels along the contour is specified by parameters
C INTVAL and MININT. The routine follows the contour through the
C array, counting the number of cells that the contour crosses. The
C first label will be written in the MININT'th cell, and additional
C labels will be written every INTVAL cells thereafter. A contour
C that crosses less than MININT cells will not be labelled. Some
C experimentation may be needed to get satisfactory results; a good
C place to start is INTVAL=20, MININT=10.
C
C Arguments:
C  A      (input) : data array.
C  IDIM   (input) : first dimension of A.
C  JDIM   (input) : second dimension of A.
C  I1, I2 (input) : range of first index to be contoured (inclusive).
C  J1, J2 (input) : range of second index to be contoured (inclusive).
C  C      (input) : the level of the contour to be labelled (one of the
C                   values given to PGCONT).
C  TR     (input) : array defining a transformation between the I,J
C                   grid of the array and the world coordinates.
C                   The world coordinates of the array point A(I,J)
C                   are given by:
C                     X = TR(1) + TR(2)*I + TR(3)*J
C                     Y = TR(4) + TR(5)*I + TR(6)*J
C                   Usually TR(3) and TR(5) are zero - unless the
C                   coordinate transformation involves a rotation or
C                   shear.
C  LABEL  (input) : character strings to be used to label the specified
C                   contour. Leading and trailing blank spaces are
C                   ignored.
C  INTVAL (input) : spacing along the contour between labels, in
C                   grid cells.
C  MININT (input) : contours that cross less than MININT cells
C                   will not be labelled.
C--
C  5-May-1994 - New routine; this routine is virtually identical to
C               PGCONT, but calls PGCONX with a different external
C               routine [TJP].
C  4-Feb-1997 - PGCONX requires an array argument, not scalar [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     CL(1)
      EXTERNAL PGCL
C
      IF (PGNOTO('PGCONL')) RETURN
C
C Save TRANS matrix and other parameters.
C
      DO 10 I=1,6
          TRANS(I) = TR(I)
   10 CONTINUE
      PGCINT = INTVAL
      PGCMIN = MININT
      PGCLAB = LABEL
C
C Use PGCONX with external function PGCL.
C
      CL(1) = C
      CALL PGCONX (A, IDIM, JDIM, I1, I2, J1, J2, CL, 1, PGCL)
C
      END
C*PGCONS -- contour map of a 2D data array (fast algorithm)
C%void cpgcons(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, const float *c, int nc, const float *tr);
C+
      SUBROUTINE PGCONS (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, NC
      REAL    A(IDIM,JDIM), C(*), TR(6)
C
C Draw a contour map of an array. The map is truncated if
C necessary at the boundaries of the viewport.  Each contour line is
C drawn with the current line attributes (color index, style, and
C width).  This routine, unlike PGCONT, does not draw each contour as a
C continuous line, but draws the straight line segments composing each
C contour in a random order.  It is thus not suitable for use on pen
C plotters, and it usually gives unsatisfactory results with dashed or
C dotted lines.  It is, however, faster than PGCONT, especially if
C several contour levels are drawn with one call of PGCONS.
C
C Arguments:
C  A      (input)  : data array.
C  IDIM   (input)  : first dimension of A.
C  JDIM   (input)  : second dimension of A.
C  I1,I2  (input)  : range of first index to be contoured (inclusive).
C  J1,J2  (input)  : range of second index to be contoured (inclusive).
C  C      (input)  : array of contour levels (in the same units as the
C                    data in array A); dimension at least NC.
C  NC     (input)  : number of contour levels (less than or equal to
C                    dimension of C). The absolute value of this
C                    argument is used (for compatibility with PGCONT,
C                    where the sign of NC is significant).
C  TR     (input)  : array defining a transformation between the I,J
C                    grid of the array and the world coordinates. The
C                    world coordinates of the array point A(I,J) are
C                    given by:
C                      X = TR(1) + TR(2)*I + TR(3)*J
C                      Y = TR(4) + TR(5)*I + TR(6)*J
C                    Usually TR(3) and TR(5) are zero - unless the
C                    coordinate transformation involves a rotation
C                    or shear.
C--
C 27-Aug-1984 - [TJP].
C 21-Sep-1989 - Better treatment of the 'ambiguous' case [A. Tennant];
C               compute world coordinates internally and eliminate
C               dependence on common block [TJP].
C-----------------------------------------------------------------------
      INTEGER  I, IC, ICORN, IDELT(6), J, K, NPT
      INTEGER  IOFF(8), JOFF(8), IENC, ITMP, JTMP, ILO, ITOT
      LOGICAL  PGNOTO
      REAL     CTR, DELTA, DVAL(5), XX, YY, X(4), Y(4)
      INTRINSIC ABS
      DATA     IDELT/0,-1,-1,0,0,-1/
      DATA     IOFF/-2,-2,-1,-1, 0, 0, 1, 1/
      DATA     JOFF/ 0,-1,-2, 1,-2, 1,-1, 0/
C
C Check arguments.
C
      IF (PGNOTO('PGCONS')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) RETURN
      IF (NC.EQ.0) RETURN
      CALL PGBBUF
C
      DO 130 J=J1+1,J2
      DO 130 I=I1+1,I2
          DVAL(1) = A(I-1,J)
          DVAL(2) = A(I-1,J-1)
          DVAL(3) = A(I,J-1)
          DVAL(4) = A(I,J)
          DVAL(5) = DVAL(1)
      DO 110 IC=1,ABS(NC)
          CTR = C(IC)
          NPT = 0
          DO 120 ICORN=1,4
          IF( (DVAL(ICORN).LT.CTR .AND. DVAL(ICORN+1).LT.CTR)
     1    .OR.(DVAL(ICORN).GE.CTR .AND. DVAL(ICORN+1).GE.CTR) ) GOTO 120
            NPT=NPT+1
            DELTA = (CTR-DVAL(ICORN))/(DVAL(ICORN+1)-DVAL(ICORN))
            GOTO (60,70,60,70), ICORN
C
   60       XX = I+IDELT(ICORN+1)
            YY = REAL(J+IDELT(ICORN)) + 
     1           DELTA*REAL(IDELT(ICORN+1)-IDELT(ICORN))
            GOTO 80
C
   70       XX = REAL(I+IDELT(ICORN+1)) +
     1           DELTA*REAL(IDELT(ICORN+2)-IDELT(ICORN+1))
            YY  = J+IDELT(ICORN)
C
   80       X(NPT) = TR(1) + TR(2)*XX + TR(3)*YY
            Y(NPT) = TR(4) + TR(5)*XX + TR(6)*YY
C
  120     CONTINUE
          IF (NPT.EQ.2) THEN
C             -- Contour crosses two sides of cell. Draw line-segment.
              CALL PGMOVE(X(1),Y(1))
              CALL PGDRAW(X(2),Y(2))
          ELSE IF (NPT.EQ.4) THEN
C             -- The 'ambiguous' case.  The routine must draw two line
C             segments here and there are two ways to do so.  The
C             following 4 lines would implement the original PGPLOT
C             method:
C            CALL PGCP(0,X(1),Y(1),CTR)
C            CALL PGCP(1,X(2),Y(2),CTR)
C            CALL PGCP(0,X(3),Y(3),CTR)
C            CALL PGCP(1,X(4),Y(4),CTR)
C            -- Choose between \\ and // based on the 8 points just
C            outside the current box.  If half or more of these points
C            lie below the contour level, then draw the lines such that
C            the high corners lie between the lines, otherwise, draw
C            the lines such that the low corners are enclosed.  Care is
C            taken to avoid going off the edge.
            ITOT=0
            ILO=0
            DO 140 K=1,8
               ITMP=I+IOFF(K)
               JTMP=J+JOFF(K)
               IF(ITMP.LT.I1 .OR. ITMP.GT.I2) GOTO 140
               IF(JTMP.LT.J1 .OR. JTMP.GT.J2) GOTO 140
               ITOT=ITOT+1
               IF(A(ITMP,JTMP).LT.CTR) ILO=ILO+1
  140       CONTINUE
            IENC=+1
            IF(ILO.LT.ITOT/2) IENC=-1
            IF(IENC.LT.0 .AND. DVAL(1).LT.CTR .OR.
     :         IENC.GT.0 .AND. DVAL(1).GE.CTR) THEN
               CALL PGMOVE(X(1),Y(1))
               CALL PGDRAW(X(2),Y(2))
               CALL PGMOVE(X(3),Y(3))
               CALL PGDRAW(X(4),Y(4))
            ELSE
               CALL PGMOVE(X(1),Y(1))
               CALL PGDRAW(X(4),Y(4))
               CALL PGMOVE(X(3),Y(3))
               CALL PGDRAW(X(2),Y(2))
            END IF
          END IF
  110     CONTINUE
  130 CONTINUE
C
      CALL PGEBUF
      END
C*PGCONT -- contour map of a 2D data array (contour-following)
C%void cpgcont(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, const float *c, int nc, const float *tr);
C+
      SUBROUTINE PGCONT (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, TR)
      INTEGER IDIM, JDIM, I1, J1, I2, J2, NC
      REAL A(IDIM,JDIM), C(*), TR(6)
C
C Draw a contour map of an array.  The map is truncated if
C necessary at the boundaries of the viewport.  Each contour line
C is drawn with the current line attributes (color index, style, and
C width); except that if argument NC is positive (see below), the line
C style is set by PGCONT to 1 (solid) for positive contours or 2
C (dashed) for negative contours.
C
C Arguments:
C  A      (input) : data array.
C  IDIM   (input) : first dimension of A.
C  JDIM   (input) : second dimension of A.
C  I1, I2 (input) : range of first index to be contoured (inclusive).
C  J1, J2 (input) : range of second index to be contoured (inclusive).
C  C      (input) : array of NC contour levels; dimension at least NC.
C  NC     (input) : +/- number of contour levels (less than or equal
C                   to dimension of C). If NC is positive, it is the
C                   number of contour levels, and the line-style is
C                   chosen automatically as described above. If NC is
C                   negative, it is minus the number of contour
C                   levels, and the current setting of line-style is
C                   used for all the contours.
C  TR     (input) : array defining a transformation between the I,J
C                   grid of the array and the world coordinates.
C                   The world coordinates of the array point A(I,J)
C                   are given by:
C                     X = TR(1) + TR(2)*I + TR(3)*J
C                     Y = TR(4) + TR(5)*I + TR(6)*J
C                   Usually TR(3) and TR(5) are zero - unless the
C                   coordinate transformation involves a rotation or
C                   shear.
C--
C (7-Feb-1983)
C (24-Aug-1984) Revised to add the option of not automatically
C       setting the line-style. Sorry about the ugly way this is
C       done (negative NC); this is the least incompatible way of doing
C       it (TJP).
C (21-Sep-1989) Changed to call PGCONX instead of duplicating the code
C       [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  I
      LOGICAL  PGNOTO
      EXTERNAL PGCP
C
      IF (PGNOTO('PGCONT')) RETURN
C
C Save TRANS matrix.
C
      DO 10 I=1,6
          TRANS(I) = TR(I)
   10 CONTINUE
C
C Use PGCONX with external function PGCP, which applies the TRANS
C scaling.
C
      CALL PGCONX (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, PGCP)
C
      END
C*PGCONX -- contour map of a 2D data array (non rectangular)
C+
      SUBROUTINE PGCONX (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, PLOT)
      INTEGER  IDIM, JDIM, I1, J1, I2, J2, NC
      REAL     A(IDIM,JDIM), C(*)
      EXTERNAL PLOT
C
C Draw a contour map of an array using a user-supplied plotting
C routine.  This routine should be used instead of PGCONT when the
C data are defined on a non-rectangular grid.  PGCONT permits only
C a linear transformation between the (I,J) grid of the array
C and the world coordinate system (x,y), but PGCONX permits any
C transformation to be used, the transformation being defined by a
C user-supplied subroutine. The nature of the contouring algorithm,
C however, dictates that the transformation should maintain the
C rectangular topology of the grid, although grid-points may be
C allowed to coalesce.  As an example of a deformed rectangular
C grid, consider data given on the polar grid theta=0.1n(pi/2),
C for n=0,1,...,10, and r=0.25m, for m=0,1,..,4. This grid
C contains 55 points, of which 11 are coincident at the origin.
C The input array for PGCONX should be dimensioned (11,5), and
C data values should be provided for all 55 elements.  PGCONX can
C also be used for special applications in which the height of the
C contour affects its appearance, e.g., stereoscopic views.
C
C The map is truncated if necessary at the boundaries of the viewport.
C Each contour line is drawn with the current line attributes (color
C index, style, and width); except that if argument NC is positive
C (see below), the line style is set by PGCONX to 1 (solid) for
C positive contours or 2 (dashed) for negative contours. Attributes
C for the contour lines can also be set in the user-supplied
C subroutine, if desired.
C
C Arguments:
C  A      (input) : data array.
C  IDIM   (input) : first dimension of A.
C  JDIM   (input) : second dimension of A.
C  I1, I2 (input) : range of first index to be contoured (inclusive).
C  J1, J2 (input) : range of second index to be contoured (inclusive).
C  C      (input) : array of NC contour levels; dimension at least NC.
C  NC     (input) : +/- number of contour levels (less than or equal
C                   to dimension of C). If NC is positive, it is the
C                   number of contour levels, and the line-style is
C                   chosen automatically as described above. If NC is
C                   negative, it is minus the number of contour
C                   levels, and the current setting of line-style is
C                   used for all the contours.
C  PLOT   (input) : the address (name) of a subroutine supplied by
C                   the user, which will be called by PGCONX to do
C                   the actual plotting. This must be declared
C                   EXTERNAL in the program unit calling PGCONX.
C
C The subroutine PLOT will be called with four arguments:
C      CALL PLOT(VISBLE,X,Y,Z)
C where X,Y (input) are real variables corresponding to
C I,J indices of the array A. If  VISBLE (input, integer) is 1,
C PLOT should draw a visible line from the current pen
C position to the world coordinate point corresponding to (X,Y);
C if it is 0, it should move the pen to (X,Y). Z is the value
C of the current contour level, and may be used by PLOT if desired.
C Example:
C       SUBROUTINE PLOT (VISBLE,X,Y,Z)
C       REAL X, Y, Z, XWORLD, YWORLD
C       INTEGER VISBLE
C       XWORLD = X*COS(Y) ! this is the user-defined
C       YWORLD = X*SIN(Y) ! transformation
C       IF (VISBLE.EQ.0) THEN
C           CALL PGMOVE (XWORLD, YWORLD)
C       ELSE
C           CALL PGDRAW (XWORLD, YWORLD)
C       END IF
C       END
C--
C 14-Nov-1985 - new routine [TJP].
C 12-Sep-1989 - correct documentation error [TJP].
C 22-Apr-1990 - corrected bug in panelling algorithm [TJP].
C 13-Dec-1990 - make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INTEGER  MAXEMX,MAXEMY
      PARAMETER (MAXEMX=100)
      PARAMETER (MAXEMY=100)
      INTEGER  I
      INTEGER  NNX,NNY, KX,KY, KI,KJ, IA,IB, JA,JB, LS, PX, PY
      LOGICAL  STYLE, PGNOTO
C
C Check arguments.
C
      IF (PGNOTO('PGCONX')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) THEN
          CALL GRWARN('PGCONX: invalid range I1:I2, J1:J2')
          RETURN
      END IF
      IF (NC.EQ.0) RETURN
      STYLE = NC.GT.0
      CALL PGQLS(LS)
      CALL PGBBUF
C
C Divide arrays into panels not exceeding MAXEMX by MAXEMY for
C contouring by PGCNSC.
C
CD    write (*,*) 'PGCONX window:',i1,i2,j1,j2
      NNX = I2-I1+1
      NNY = J2-J1+1
      KX = MAX(1,(NNX+MAXEMX-2)/(MAXEMX-1))
      KY = MAX(1,(NNY+MAXEMY-2)/(MAXEMY-1))
      PX = (NNX+KX-1)/KX
      PY = (NNY+KY-1)/KY
      DO 60 KI=1,KX
          IA = I1 + (KI-1)*PX
          IB = MIN(I2, IA + PX)
          DO 50 KJ=1,KY
              JA = J1 + (KJ-1)*PY
              JB = MIN(J2, JA + PY)
C
C             Draw the contours in one panel.
C
CD            write (*,*) 'PGCONX panel:',ia,ib,ja,jb
              IF (STYLE) CALL PGSLS(1)
              DO 40 I=1,ABS(NC)
                  IF (STYLE.AND.(C(I).LT.0.0)) CALL PGSLS(2)
                  CALL PGCNSC(A,IDIM,JDIM,IA,IB,JA,JB,C(I),PLOT)
                  IF (STYLE) CALL PGSLS(1)
   40         CONTINUE
   50     CONTINUE
   60 CONTINUE
C
      CALL PGSLS(LS)
      CALL PGEBUF
      END
C
      SUBROUTINE PGCP (K, X, Y, Z)
C
C PGPLOT (internal routine): Draw one contour segment (for use by
C PGCNSC).
C
C Arguments:
C
C K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
C       a line from the current position to (X,Y); otherwise
C       do nothing.
C X (input, real): X world-coordinate of end point.
C Y (input, real): Y world-coordinate of end point.
C Z (input, real): the value of the contour level, not used by PGCP at
C       the moment.
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  K
      REAL     X,XX,Y,YY,Z
C
      XX = TRANS(1) + TRANS(2)*X + TRANS(3)*Y
      YY = TRANS(4) + TRANS(5)*X + TRANS(6)*Y
      IF (K.EQ.1) THEN
          CALL GRLINA(XX,YY)
      ELSE IF (K.EQ.0) THEN
          CALL GRMOVA(XX,YY)
      END IF
      END
C*PGCTAB -- install the color table to be used by PGIMAG
C%void cpgctab(const float *l, const float *r, const float *g, \
C% const float *b, int nc, float contra, float bright);
C+
      SUBROUTINE PGCTAB(L, R, G, B, NC, CONTRA, BRIGHT)
      INTEGER NC
      REAL    L(NC), R(NC), G(NC), B(NC), CONTRA, BRIGHT
C
C Use the given color table to change the color representations of
C all color indexes marked for use by PGIMAG. To change which
C color indexes are thus marked, call PGSCIR before calling PGCTAB
C or PGIMAG. On devices that can change the color representations
C of previously plotted graphics, PGCTAB will also change the colors
C of existing graphics that were plotted with the marked color
C indexes. This feature can then be combined with PGBAND to
C interactively manipulate the displayed colors of data previously
C plotted with PGIMAG.
C
C Limitations:
C  1. Some devices do not propagate color representation changes
C     to previously drawn graphics.
C  2. Some devices ignore requests to change color representations.
C  3. The appearance of specific color representations on grey-scale
C     devices is device-dependent.
C
C Notes:
C  To reverse the sense of a color table, change the chosen contrast
C  and brightness to -CONTRA and 1-BRIGHT.
C
C  In the following, the term 'color table' refers to the input
C  L,R,G,B arrays, whereas 'color ramp' refers to the resulting
C  ramp of colors that would be seen with PGWEDG.
C
C Arguments:
C  L      (input)  : An array of NC normalized ramp-intensity levels
C                    corresponding to the RGB primary color intensities
C                    in R(),G(),B(). Colors on the ramp are linearly
C                    interpolated from neighbouring levels.
C                    Levels must be sorted in increasing order.
C                     0.0 places a color at the beginning of the ramp.
C                     1.0 places a color at the end of the ramp.
C                    Colors outside these limits are legal, but will
C                    not be visible if CONTRA=1.0 and BRIGHT=0.5.
C  R      (input)  : An array of NC normalized red intensities.
C  G      (input)  : An array of NC normalized green intensities.
C  B      (input)  : An array of NC normalized blue intensities.
C  NC     (input)  : The number of color table entries.
C  CONTRA (input)  : The contrast of the color ramp (normally 1.0).
C                    Negative values reverse the direction of the ramp.
C  BRIGHT (input)  : The brightness of the color ramp. This is normally
C                    0.5, but can sensibly hold any value between 0.0
C                    and 1.0. Values at or beyond the latter two
C                    extremes, saturate the color ramp with the colors
C                    of the respective end of the color table.
C--
C  17-Sep-1994 - New routine [MCS].
C  14-Apr-1997 - Modified to implement a more conventional
C                interpretation of contrast and brightness [MCS].
C-----------------------------------------------------------------------
      INTEGER MININD, MAXIND, CI
      INTEGER NTOTAL, NSPAN
      INTEGER BELOW, ABOVE
      LOGICAL FORWRD
      REAL CA, CB, CIFRAC, SPAN
      REAL LEVEL
      REAL LDIFF, LFRAC
      REAL RED, GREEN, BLUE
C
C Set the minimum absolute contrast - this prevents a divide by zero.
C
      REAL MINCTR
      PARAMETER (MINCTR = 1.0/256)
C
C No colormap entries?
C
      IF(NC .EQ. 0) RETURN
C
C Determine the range of color indexes to be used.
C
      CALL PGQCIR(MININD, MAXIND)
C
C Count the total number of color indexes to be processed.
C
      NTOTAL = MAXIND - MININD + 1
C
C No definable colors?
C
      IF(NTOTAL .LT. 1 .OR. MININD .LT. 0) RETURN
C
C Prevent a divide by zero later by ensuring that CONTRA >= ABS(MINCTR).
C
      IF(ABS(CONTRA) .LT. MINCTR) THEN
        CONTRA = SIGN(MINCTR, CONTRA)
      END IF
C
C Convert contrast to the normalized stretch of the
C color table across the available color index range.
C
      SPAN = 1.0 / ABS(CONTRA)
C
C Translate from brightness and contrast to the normalized color index
C coordinates, CA and CB, at which to place the start and end of the
C color table.
C
      IF(CONTRA .GE. 0.0) THEN
        CA = 1.0 - BRIGHT * (1.0 + SPAN)
        CB = CA + SPAN
      ELSE
        CA = BRIGHT * (1.0 + SPAN)
        CB = CA - SPAN
      END IF
C
C Determine the number of color indexes spanned by the color table.
C
      NSPAN = INT(SPAN * NTOTAL)
C
C Determine the direction in which the color table should be traversed.
C
      FORWRD = CA .LE. CB
C
C Initialize the indexes at which to start searching the color table.
C
C Set the start index for traversing the table from NC to 1.
C
      BELOW = NC
C
C Set the start index for traversing the table from 1 to NC.
C
      ABOVE = 1
C
C Buffer PGPLOT commands until the color map has been completely
C installed.
C
      CALL PGBBUF
C
C Linearly interpolate the color table RGB values onto each color index.
C
      DO 1 CI=MININD, MAXIND
C
C Turn the color index into a fraction of the range MININD..MAXIND.
C
        CIFRAC = REAL(CI-MININD) / REAL(MAXIND-MININD)
C
C Determine the color table position that corresponds to color index,
C CI.
C
        IF(NSPAN .GT. 0) THEN
          LEVEL = (CIFRAC-CA) / (CB-CA)
        ELSE
          IF(CIFRAC .LE. CA) THEN
            LEVEL = 0.0
          ELSE
            LEVEL = 1.0
          END IF
        END IF
C
C Search for the indexes of the two color table entries that straddle
C LEVEL. The search algorithm assumes that values in L() are
C arranged in increasing order. This allows us to search the color table
C from the point at which the last search left off, rather than having
C to search the whole color table each time.
C
        IF(FORWRD) THEN
 2        IF(ABOVE.LE.NC .AND. L(ABOVE).LT.LEVEL) THEN
            ABOVE = ABOVE + 1
            GOTO 2
          END IF
          BELOW = ABOVE - 1
        ELSE
 3        IF(BELOW.GE.1 .AND. L(BELOW).GT.LEVEL) THEN
            BELOW = BELOW - 1
            GOTO 3
          END IF
          ABOVE = BELOW + 1
        END IF
C
C If the indexes lie outside the table, substitute the index of the
C nearest edge of the table.
C
        IF(BELOW .LT. 1) THEN
          LEVEL = 0.0
          BELOW = 1
          ABOVE = 1
        ELSE IF(ABOVE .GT. NC) THEN
          LEVEL = 1.0
          BELOW = NC
          ABOVE = NC
        END IF
C
C Linearly interpolate the primary color intensities from color table
C entries, BELOW and ABOVE.
C
        LDIFF = L(ABOVE) - L(BELOW)
        IF(LDIFF .GT. MINCTR) THEN
          LFRAC = (LEVEL - L(BELOW)) / LDIFF
        ELSE
          LFRAC = 0.0
        END IF
        RED   = R(BELOW) + (R(ABOVE) - R(BELOW)) * LFRAC
        GREEN = G(BELOW) + (G(ABOVE) - G(BELOW)) * LFRAC
        BLUE  = B(BELOW) + (B(ABOVE) - B(BELOW)) * LFRAC
C
C Intensities are only defined between 0 and 1.
C
        IF(RED   .LT. 0.0)   RED = 0.0
        IF(RED   .GT. 1.0)   RED = 1.0
        IF(GREEN .LT. 0.0) GREEN = 0.0
        IF(GREEN .GT. 1.0) GREEN = 1.0
        IF(BLUE  .LT. 0.0)  BLUE = 0.0
        IF(BLUE  .GT. 1.0)  BLUE = 1.0
C
C Install the new color representation.
C
        CALL PGSCR(CI, RED, GREEN, BLUE)
 1    CONTINUE
C
C Reveal the changed color map.
C
      CALL PGEBUF
      RETURN
      END
C*PGCURS -- read cursor position
C%int cpgcurs(float *x, float *y, char *ch_scalar);
C+
      INTEGER FUNCTION PGCURS (X, Y, CH)
      REAL X, Y
      CHARACTER*(*) CH
C
C Read the cursor position and a character typed by the user.
C The position is returned in world coordinates.  PGCURS positions
C the cursor at the position specified, allows the user to move the
C cursor using the joystick or arrow keys or whatever is available on
C the device. When he has positioned the cursor, the user types a
C single character on the keyboard; PGCURS then returns this
C character and the new cursor position (in world coordinates).
C
C Returns:
C  PGCURS         : 1 if the call was successful; 0 if the device
C                    has no cursor or some other error occurs.
C Arguments:
C  X      (in/out) : the world x-coordinate of the cursor.
C  Y      (in/out) : the world y-coordinate of the cursor.
C  CH     (output) : the character typed by the user; if the device has
C                    no cursor or if some other error occurs, the value
C                    CHAR(0) [ASCII NUL character] is returned.
C
C Note: The cursor coordinates (X,Y) may be changed by PGCURS even if
C the device has no cursor or if the user does not move the cursor.
C Under these circumstances, the position returned in (X,Y) is that of
C the pixel nearest to the requested position.
C--
C  7-Sep-1994 - changed to use PGBAND [TJP].
C-----------------------------------------------------------------------
      INTEGER PGBAND
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGCURS')) THEN
         CH = CHAR(0)
         PGCURS = 0
      ELSE
         PGCURS = PGBAND(0, 1, 0.0, 0.0, X, Y, CH)
      END IF
      END
C*PGCURSE -- non-standard alias for PGCURS
C+
      INTEGER FUNCTION PGCURSE (X, Y, CH)
      REAL X, Y
      CHARACTER*1 CH
C
C See description of PGCURS.
C--
      INTEGER PGCURS
      PGCURSE = PGCURS (X, Y, CH)
      END
C*PGDRAW -- draw a line from the current pen position to a point
C%void cpgdraw(float x, float y);
C+
      SUBROUTINE PGDRAW (X, Y)
      REAL X, Y
C
C Draw a line from the current pen position to the point
C with world-coordinates (X,Y). The line is clipped at the edge of the
C current window. The new pen position is (X,Y) in world coordinates.
C
C Arguments:
C  X      (input)  : world x-coordinate of the end point of the line.
C  Y      (input)  : world y-coordinate of the end point of the line.
C--
C 27-Nov-1986
C-----------------------------------------------------------------------
      CALL PGBBUF
      CALL GRLINA(X,Y)
      CALL PGEBUF
      END
C*PGEBUF -- end batch of output (buffer)
C%void cpgebuf(void);
C+
      SUBROUTINE PGEBUF
C
C A call to PGEBUF marks the end of a batch of graphical output begun
C with the last call of PGBBUF.  PGBBUF and PGEBUF calls should always
C be paired. Each call to PGBBUF increments a counter, while each call
C to PGEBUF decrements the counter. When the counter reaches 0, the
C batch of output is written on the output device.
C
C Arguments: none
C--
C 21-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (.NOT.PGNOTO('PGEBUF')) THEN
          PGBLEV(PGID) = MAX(0, PGBLEV(PGID) - 1)
          IF (PGBLEV(PGID).EQ.0) CALL GRTERM
      END IF
      END
C*PGEND -- close all open graphics devices
C%void cpgend(void);
C+
      SUBROUTINE PGEND
C
C Close and release any open graphics devices. All devices must be
C closed by calling either PGCLOS (for each device) or PGEND before
C the program terminates. If a device is not closed properly, some
C or all of the graphical output may be lost.
C
C Arguments: none
C--
C 22-Dec-1995 [TJP] - revised to call PGCLOS for each open device.
C 25-Feb-1997 [TJP] - revised description.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER I
C
      DO 10 I=1,PGMAXD
         IF (PGDEVS(I).EQ.1) THEN
            CALL PGSLCT(I)
            CALL PGCLOS
         END IF
 10   CONTINUE
      END
C*PGENV -- set window and viewport and draw labeled frame
C%void cpgenv(float xmin, float xmax, float ymin, float ymax, \
C% int just, int axis);
C+
      SUBROUTINE PGENV (XMIN, XMAX, YMIN, YMAX, JUST, AXIS)
      REAL XMIN, XMAX, YMIN, YMAX
      INTEGER JUST, AXIS
C
C Set PGPLOT "Plotter Environment".  PGENV establishes the scaling
C for subsequent calls to PGPT, PGLINE, etc.  The plotter is
C advanced to a new page or panel, clearing the screen if necessary.
C If the "prompt state" is ON (see PGASK), confirmation
C is requested from the user before clearing the screen.
C If requested, a box, axes, labels, etc. are drawn according to
C the setting of argument AXIS.
C
C Arguments:
C  XMIN   (input)  : the world x-coordinate at the bottom left corner
C                    of the viewport.
C  XMAX   (input)  : the world x-coordinate at the top right corner
C                    of the viewport (note XMAX may be less than XMIN).
C  YMIN   (input)  : the world y-coordinate at the bottom left corner
C                    of the viewport.
C  YMAX   (input)  : the world y-coordinate at the top right corner
C                    of the viewport (note YMAX may be less than YMIN).
C  JUST   (input)  : if JUST=1, the scales of the x and y axes (in
C                    world coordinates per inch) will be equal,
C                    otherwise they will be scaled independently.
C  AXIS   (input)  : controls the plotting of axes, tick marks, etc:
C      AXIS = -2 : draw no box, axes or labels;
C      AXIS = -1 : draw box only;
C      AXIS =  0 : draw box and label it with coordinates;
C      AXIS =  1 : same as AXIS=0, but also draw the
C                  coordinate axes (X=0, Y=0);
C      AXIS =  2 : same as AXIS=1, but also draw grid lines
C                  at major increments of the coordinates;
C      AXIS = 10 : draw box and label X-axis logarithmically;
C      AXIS = 20 : draw box and label Y-axis logarithmically;
C      AXIS = 30 : draw box and label both axes logarithmically.
C
C For other axis options, use routine PGBOX. PGENV can be persuaded to
C call PGBOX with additional axis options by defining an environment
C parameter PGPLOT_ENVOPT containing the required option codes. 
C Examples:
C   PGPLOT_ENVOPT=P      ! draw Projecting tick marks
C   PGPLOT_ENVOPT=I      ! Invert the tick marks
C   PGPLOT_ENVOPT=IV     ! Invert tick marks and label y Vertically
C--
C  1-May-1983
C 25-Sep-1985 [TJP] - change to use PGWNAD.
C 23-Nov-1985 [TJP] - add PGPLOT_ENVOPT option.
C 31-Dec-1985 [TJP] - remove automatic PGBEG call.
C 29-Aug-1989 [TJP] - remove common block; no longer needed.
C-----------------------------------------------------------------------
      INTEGER      L
      LOGICAL      PGNOTO
      CHARACTER*10 XOPTS, YOPTS, ENVOPT, TEMP
C
      IF (PGNOTO('PGENV')) RETURN
C
C Start a new picture: move to a new panel or page as necessary.
C
      CALL PGPAGE
C
C Redefine the standard viewport.
C
      CALL PGVSTD
C
C If invalid arguments are specified, issue warning and leave window
C unchanged.
C
      IF (XMIN.EQ.XMAX) THEN
          CALL GRWARN('invalid x limits in PGENV: XMIN = XMAX.')
          RETURN
      ELSE IF (YMIN.EQ.YMAX) THEN
          CALL GRWARN('invalid y limits in PGENV: YMIN = YMAX.')
          RETURN
      END IF
C
C Call PGSWIN to define the window.
C If equal-scales requested, adjust viewport.
C
      IF (JUST.EQ.1) THEN
          CALL PGWNAD(XMIN,XMAX,YMIN,YMAX)
      ELSE
          CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
      END IF
C
C Call PGBOX to draw and label frame around viewport.
C
      YOPTS = '*'
      IF (AXIS.EQ.-2) THEN
          XOPTS = ' '
      ELSE IF (AXIS.EQ.-1) THEN
          XOPTS = 'BC'
      ELSE IF (AXIS.EQ.0) THEN
          XOPTS = 'BCNST'
      ELSE IF (AXIS.EQ.1) THEN
          XOPTS = 'ABCNST'
      ELSE IF (AXIS.EQ.2) THEN
          XOPTS = 'ABCGNST'
      ELSE IF (AXIS.EQ.10) THEN
          XOPTS = 'BCNSTL'
          YOPTS = 'BCNST'
      ELSE IF (AXIS.EQ.20) THEN
          XOPTS = 'BCNST'
          YOPTS = 'BCNSTL'
      ELSE IF (AXIS.EQ.30) THEN
          XOPTS = 'BCNSTL'
          YOPTS = 'BCNSTL'
      ELSE
          CALL GRWARN('PGENV: illegal AXIS argument.')
          XOPTS = 'BCNST'
      END IF
      IF (YOPTS.EQ.'*') YOPTS = XOPTS
C
C Additional PGBOX options from PGPLOT_ENVOPT.
C
      CALL GRGENV('ENVOPT', ENVOPT, L)
      IF (L.GT.0 .AND. AXIS.GE.0) THEN
          TEMP = XOPTS
          XOPTS = ENVOPT(1:L)//TEMP
          TEMP = YOPTS
          YOPTS = ENVOPT(1:L)//TEMP
      END IF
      CALL PGBOX(XOPTS, 0.0, 0, YOPTS, 0.0, 0)
C
      END
C*PGERAS -- erase all graphics from current page
C%void cpgeras(void);
C+
      SUBROUTINE PGERAS
C
C Erase all graphics from the current page (or current panel, if
C the view surface has been divided into panels with PGSUBP).
C
C Arguments: none
C--
C 24-Jun-1994
C-----------------------------------------------------------------------
      INTEGER CI, FS
      REAL XV1, XV2, YV1, YV2, XW1, XW2, YW1, YW2
      CALL PGBBUF
      CALL PGQCI(CI)
      CALL PGQFS(FS)
      CALL PGSCI(0)
      CALL PGSFS(1)
      CALL PGQWIN(XW1, XW2, YW1, YW2)
      CALL PGQVP(0, XV1, XV2, YV1, YV2)
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGRECT(XW1, XW2, YW1, YW2)
      CALL PGSVP(XV1, XV2, YV1, YV2)
      CALL PGSCI(CI)
      CALL PGSFS(FS)
      CALL PGEBUF
      END
C*PGERR1 -- horizontal or vertical error bar
C%void cpgerr1(int dir, float x, float y, float e, float t);
C+
      SUBROUTINE PGERR1 (DIR, X, Y, E, T)
      INTEGER DIR
      REAL X, Y, E
      REAL T
C
C Plot a single error bar in the direction specified by DIR.
C This routine draws an error bar only; to mark the data point at
C the start of the error bar, an additional call to PGPT is required.
C To plot many error bars, use PGERRB.
C
C Arguments:
C  DIR    (input)  : direction to plot the error bar relative to
C                    the data point. 
C                    One-sided error bar:
C                      DIR is 1 for +X (X to X+E);
C                             2 for +Y (Y to Y+E);
C                             3 for -X (X to X-E);
C                             4 for -Y (Y to Y-E).
C                    Two-sided error bar:
C                      DIR is 5 for +/-X (X-E to X+E); 
C                             6 for +/-Y (Y-E to Y+E).
C  X      (input)  : world x-coordinate of the data.
C  Y      (input)  : world y-coordinate of the data.
C  E      (input)  : value of error bar distance to be added to the
C                    data position in world coordinates.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C--
C 31-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL  PGNOTO
      REAL     XTIK, YTIK, XX, YY
C
      IF (PGNOTO('PGERR1')) RETURN
      IF (DIR.LT.1 .OR. DIR.GT.6) RETURN
      CALL PGBBUF
C
C Determine terminal length.
C
      CALL PGTIKL(T, XTIK, YTIK)
C
C Draw terminal at starting point if required.
C
      IF (DIR.EQ.5) THEN
         XX = X-E
         YY = Y
      ELSE IF (DIR.EQ.6) THEN
         XX = X
         YY = Y-E
      ELSE
         XX = X
         YY = Y
      END IF
      IF (T.NE.0.0) THEN
         IF (DIR.EQ.5) THEN
            CALL GRMOVA(XX,YY-YTIK)
            CALL GRLINA(XX,YY+YTIK)
         ELSE IF (DIR.EQ.6) THEN
            CALL GRMOVA(XX-XTIK,YY)
            CALL GRLINA(XX+XTIK,YY)
         END IF
      END IF
C
C Draw the error bar itself.
C
      CALL GRMOVA(XX,YY)
      IF (DIR.EQ.1 .OR. DIR.EQ.5) THEN
         XX = X+E
         YY = Y
      ELSE IF (DIR.EQ.2 .OR. DIR.EQ.6) THEN
         XX = X
         YY = Y+E
      ELSE IF (DIR.EQ.3) THEN
         XX = X-E
         YY = Y
      ELSE IF (DIR.EQ.4) THEN
         XX = X
         YY = Y-E
      END IF
      CALL GRLINA(XX,YY)
C
C Draw terminal at end point.
C
      IF (T.NE.0.0) THEN
         IF (MOD(DIR,2).EQ.1) THEN
            CALL GRMOVA(XX,YY-YTIK)
            CALL GRLINA(XX,YY+YTIK)
         ELSE
            CALL GRMOVA(XX-XTIK,YY)
            CALL GRLINA(XX+XTIK,YY)
         END IF
      END IF
C
      CALL PGEBUF
      END
C*PGERRB -- horizontal or vertical error bar
C%void cpgerrb(int dir, int n, const float *x, const float *y, \
C% const float *e, float t);
C+
      SUBROUTINE PGERRB (DIR, N, X, Y, E, T)
      INTEGER DIR, N
      REAL X(*), Y(*), E(*)
      REAL T
C
C Plot error bars in the direction specified by DIR.
C This routine draws an error bar only; to mark the data point at
C the start of the error bar, an additional call to PGPT is required.
C
C Arguments:
C  DIR    (input)  : direction to plot the error bar relative to
C                    the data point. 
C                    One-sided error bar:
C                      DIR is 1 for +X (X to X+E);
C                             2 for +Y (Y to Y+E);
C                             3 for -X (X to X-E);
C                             4 for -Y (Y to Y-E).
C                    Two-sided error bar:
C                      DIR is 5 for +/-X (X-E to X+E); 
C                             6 for +/-Y (Y-E to Y+E).
C  N      (input)  : number of error bars to plot.
C  X      (input)  : world x-coordinates of the data.
C  Y      (input)  : world y-coordinates of the data.
C  E      (input)  : value of error bar distance to be added to the
C                    data position in world coordinates.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X, Y, and E must be greater
C than or equal to N. If N is 1, X, Y, and E may be scalar
C variables, or expressions.
C--
C  1-Mar-1991 - new routine [JM].
C 20-Apr-1992 - correct bug [ALF, TJP].
C 28-Mar-1995 - add options DIR = 5 or 6 [TJP].
C 31-Mar-1997 - use pgtikl [TJP].
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     XTIK, YTIK, XX, YY
C
      IF (PGNOTO('PGERRB')) RETURN
      IF (N.LT.1) RETURN
      IF (DIR.LT.1 .OR. DIR.GT.6) RETURN
      CALL PGBBUF
C
C Determine terminal length.
C
      CALL PGTIKL(T, XTIK, YTIK)
C
C Loop through points.
C
      DO 10 I=1,N
C
C Draw terminal at starting point if required.
C
         IF (DIR.EQ.5) THEN
            XX = X(I)-E(I)
            YY = Y(I)
         ELSE IF (DIR.EQ.6) THEN
            XX = X(I)
            YY = Y(I)-E(I)
         ELSE
            XX = X(I)
            YY = Y(I)
         END IF
         IF (T.NE.0.0) THEN
            IF (DIR.EQ.5) THEN
               CALL GRMOVA(XX,YY-YTIK)
               CALL GRLINA(XX,YY+YTIK)
            ELSE IF (DIR.EQ.6) THEN
               CALL GRMOVA(XX-XTIK,YY)
               CALL GRLINA(XX+XTIK,YY)
            END IF
         END IF
C
C Draw the error bar itself.
C
         CALL GRMOVA(XX,YY)
         IF (DIR.EQ.1 .OR. DIR.EQ.5) THEN
            XX = X(I)+E(I)
            YY = Y(I)
         ELSE IF (DIR.EQ.2 .OR. DIR.EQ.6) THEN
            XX = X(I)
            YY = Y(I)+E(I)
         ELSE IF (DIR.EQ.3) THEN
            XX = X(I)-E(I)
            YY = Y(I)
         ELSE IF (DIR.EQ.4) THEN
            XX = X(I)
            YY = Y(I)-E(I)
         END IF
         CALL GRLINA(XX,YY)
C
C Draw terminal at end point.
C
         IF (T.NE.0.0) THEN
            IF (MOD(DIR,2).EQ.1) THEN
               CALL GRMOVA(XX,YY-YTIK)
               CALL GRLINA(XX,YY+YTIK)
            ELSE
               CALL GRMOVA(XX-XTIK,YY)
               CALL GRLINA(XX+XTIK,YY)
            END IF
         END IF
C
 10   CONTINUE
      CALL PGEBUF
      END
C*PGERRX -- horizontal error bar
C%void cpgerrx(int n, const float *x1, const float *x2, \
C% const float *y, float t);
C+
      SUBROUTINE PGERRX (N, X1, X2, Y, T)
      INTEGER N
      REAL X1(*), X2(*), Y(*)
      REAL T
C
C Plot horizontal error bars.
C This routine draws an error bar only; to mark the data point in
C the middle of the error bar, an additional call to PGPT or
C PGERRY is required.
C
C Arguments:
C  N      (input)  : number of error bars to plot.
C  X1     (input)  : world x-coordinates of lower end of the
C                    error bars.
C  X2     (input)  : world x-coordinates of upper end of the
C                    error bars.
C  Y      (input)  : world y-coordinates of the data.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X1, X2, and Y must be greater
C than or equal to N. If N is 1, X1, X2, and Y may be scalar
C variables, or expressions, eg:
C       CALL PGERRX(1,X-SIGMA,X+SIGMA,Y)
C--
C (6-Oct-1983)
C 31-Mar-1997 - use pgtikl [TJP[.
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     XTIK, YTIK
C
      IF (PGNOTO('PGERRX')) RETURN
      IF (N.LT.1) RETURN
      CALL PGBBUF
C
      CALL PGTIKL(T, XTIK, YTIK)
      DO 10 I=1,N
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X1(I),Y(I)-YTIK)
              CALL GRLINA(X1(I),Y(I)+YTIK)
          END IF
          CALL GRMOVA(X1(I),Y(I))
          CALL GRLINA(X2(I),Y(I))
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X2(I),Y(I)-YTIK)
              CALL GRLINA(X2(I),Y(I)+YTIK)
          END IF
   10 CONTINUE
      CALL PGEBUF
      END
C*PGERRY -- vertical error bar
C%void cpgerry(int n, const float *x, const float *y1, \
C% const float *y2, float t);
C+
      SUBROUTINE PGERRY (N, X, Y1, Y2, T)
      INTEGER N
      REAL X(*), Y1(*), Y2(*)
      REAL T
C
C Plot vertical error bars.
C This routine draws an error bar only; to mark the data point in
C the middle of the error bar, an additional call to PGPT or
C PGERRX is required.
C
C Arguments:
C  N      (input)  : number of error bars to plot.
C  X      (input)  : world x-coordinates of the data.
C  Y1     (input)  : world y-coordinates of top end of the
C                    error bars.
C  Y2     (input)  : world y-coordinates of bottom end of the
C                    error bars.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X, Y1, and Y2 must be greater
C than or equal to N. If N is 1, X, Y1, and Y2 may be scalar
C variables or expressions, eg:
C       CALL PGERRY(1,X,Y+SIGMA,Y-SIGMA)
C--
C (6-Oct-1983)
C 31-Mar-1997 - use pgtikl [TJP].
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     XTIK, YTIK
C
      IF (PGNOTO('PGERRY')) RETURN
      IF (N.LT.1) RETURN
      CALL PGBBUF
C
      CALL PGTIKL(T, XTIK, YTIK)
      DO 10 I=1,N
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X(I)-XTIK,Y1(I))
              CALL GRLINA(X(I)+XTIK,Y1(I))
          END IF
          CALL GRMOVA(X(I),Y1(I))
          CALL GRLINA(X(I),Y2(I))
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X(I)-XTIK,Y2(I))
              CALL GRLINA(X(I)+XTIK,Y2(I))
          END IF
   10 CONTINUE
      CALL PGEBUF
      END
C*PGETXT -- erase text from graphics display
C%void cpgetxt(void);
C+
      SUBROUTINE PGETXT
C
C Some graphics terminals display text (the normal interactive dialog)
C on the same screen as graphics. This routine erases the text from the
C view surface without affecting the graphics. It does nothing on
C devices which do not display text on the graphics screen, and on
C devices which do not have this capability.
C
C Arguments:
C  None
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CALL GRETXT
      END
C*PGFUNT -- function defined by X = F(T), Y = G(T)
C+
      SUBROUTINE PGFUNT (FX, FY, N, TMIN, TMAX, PGFLAG)
      REAL FX, FY
      EXTERNAL FX, FY
      INTEGER N
      REAL TMIN, TMAX
      INTEGER PGFLAG
C
C Draw a curve defined by parametric equations X = FX(T), Y = FY(T).
C
C Arguments:
C  FX     (external real function): supplied by the user, evaluates
C                    X-coordinate.
C  FY     (external real function): supplied by the user, evaluates
C                    Y-coordinate.
C  N      (input)  : the number of points required to define the
C                    curve. The functions FX and FY will each be
C                    called N+1 times.
C  TMIN   (input)  : the minimum value for the parameter T.
C  TMAX   (input)  : the maximum value for the parameter T.
C  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGFUNT to
C                    start a new plot with automatic scaling.
C
C Note: The functions FX and FY must be declared EXTERNAL in the
C Fortran program unit that calls PGFUNT.
C--
C  5-Oct-1983
C 11-May-1990 - remove unnecessary include [TJP].
C 13-Dec-1990 - make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXP
      PARAMETER (MAXP=1000)
      INTEGER  I
      REAL     X(0:MAXP), Y(0:MAXP), DT
      REAL     XMIN, XMAX, YMIN, YMAX
C
      IF (N.LT.1 .OR. N.GT.MAXP) THEN
          CALL GRWARN('PGFUNT: invalid arguments')
          RETURN
      END IF
      CALL PGBBUF
C
C Evaluate function.
C
      DT = (TMAX-TMIN)/N
      X(0) = FX(TMIN)
      Y(0) = FY(TMIN)
      XMIN = X(0)
      XMAX = X(0)
      YMIN = Y(0)
      YMAX = Y(0)
      DO 10 I=1,N
          X(I) = FX(TMIN+DT*I)
          Y(I) = FY(TMIN+DT*I)
          XMIN = MIN(XMIN,X(I))
          XMAX = MAX(XMAX,X(I))
          YMIN = MIN(YMIN,Y(I))
          YMAX = MAX(YMAX,Y(I))
   10 CONTINUE
      DT = 0.05*(XMAX-XMIN)
      IF (DT.EQ.0.0) THEN
          XMIN = XMIN - 1.0
          XMAX = XMAX + 1.0
      ELSE
          XMIN = XMIN - DT
          XMAX = XMAX + DT
      END IF
      DT = 0.05*(YMAX-YMIN)
      IF (DT.EQ.0.0) THEN
          YMIN = YMIN - 1.0
          YMAX = YMAX + 1.0
      ELSE
          YMIN = YMIN - DT
          YMAX = YMAX + DT
      END IF
C
C Define environment if necessary.
C
      IF (PGFLAG.EQ.0) CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,0)
C
C Draw curve.
C
      CALL PGMOVE(X(0),Y(0))
      DO 20 I=1,N
          CALL PGDRAW(X(I),Y(I))
   20 CONTINUE
C
      CALL PGEBUF
      END
C*PGFUNX -- function defined by Y = F(X)
C+
      SUBROUTINE PGFUNX (FY, N, XMIN, XMAX, PGFLAG)
      REAL FY
      EXTERNAL FY
      INTEGER N
      REAL XMIN, XMAX
      INTEGER PGFLAG
C
C Draw a curve defined by the equation Y = FY(X), where FY is a
C user-supplied subroutine.
C
C Arguments:
C  FY     (external real function): supplied by the user, evaluates
C                    Y value at a given X-coordinate.
C  N      (input)  : the number of points required to define the
C                    curve. The function FY will be called N+1 times.
C                    If PGFLAG=0 and N is greater than 1000, 1000
C                    will be used instead.  If N is less than 1,
C                    nothing will be drawn.
C  XMIN   (input)  : the minimum value of X.
C  XMAX   (input)  : the maximum value of X.
C  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGFUNX to
C                    start a new plot with X limits (XMIN, XMAX)
C                    and automatic scaling in Y.
C
C Note: The function FY must be declared EXTERNAL in the Fortran
C program unit that calls PGFUNX.  It has one argument, the
C x-coordinate at which the y value is required, e.g.
C   REAL FUNCTION FY(X)
C   REAL X
C   FY = .....
C   END
C--
C  6-Oct-1983 - TJP.
C  6-May-1985 - fix Y(0) bug - TJP.
C 11-May-1990 - remove unnecessary include - TJP.
C-----------------------------------------------------------------------
      INTEGER MAXP
      PARAMETER (MAXP=1000)
      INTEGER  I, NN
      REAL     Y(0:MAXP), DT, DY
      REAL     YMIN, YMAX
C
C Check N > 1, and find parameter increment.
C
      IF (N.LT.1) RETURN
      DT = (XMAX-XMIN)/N
      CALL PGBBUF
C
C Case 1: we do not have to find limits.
C
      IF (PGFLAG.NE.0) THEN
          CALL PGMOVE(XMIN,FY(XMIN))
          DO 10 I=1,N
              CALL PGDRAW(XMIN+I*DT,FY(XMIN+I*DT))
   10     CONTINUE
C
C Case 2: find limits and scale plot; function values must be stored
C in an array.
C
      ELSE
          NN = MIN(N,MAXP)
          Y(0) = FY(XMIN)
          YMIN = Y(0)
          YMAX = Y(0)
          DO 20 I=1,NN
              Y(I) = FY(XMIN+DT*I)
              YMIN = MIN(YMIN,Y(I))
              YMAX = MAX(YMAX,Y(I))
   20     CONTINUE
          DY = 0.05*(YMAX-YMIN)
          IF (DY.EQ.0.0) THEN
              YMIN = YMIN - 1.0
              YMAX = YMAX + 1.0
          ELSE
              YMIN = YMIN - DY
              YMAX = YMAX + DY
          END IF
          CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,0)
          CALL PGMOVE(XMIN,Y(0))
          DO 30 I=1,NN
              CALL PGDRAW(XMIN+DT*I,Y(I))
   30     CONTINUE
      END IF
C
      CALL PGEBUF
      END
C*PGFUNY -- function defined by X = F(Y)
C+
      SUBROUTINE PGFUNY (FX, N, YMIN, YMAX, PGFLAG)
      REAL    FX
      EXTERNAL FX
      INTEGER N
      REAL    YMIN, YMAX
      INTEGER PGFLAG
C
C Draw a curve defined by the equation X = FX(Y), where FY is a
C user-supplied subroutine.
C
C Arguments:
C  FX     (external real function): supplied by the user, evaluates
C                    X value at a given Y-coordinate.
C  N      (input)  : the number of points required to define the
C                    curve. The function FX will be called N+1 times.
C                    If PGFLAG=0 and N is greater than 1000, 1000
C                    will be used instead.  If N is less than 1,
C                    nothing will be drawn.
C  YMIN   (input)  : the minimum value of Y.
C  YMAX   (input)  : the maximum value of Y.
C  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGFUNY to
C                    start a new plot with Y limits (YMIN, YMAX)
C                    and automatic scaling in X.
C
C Note: The function FX must be declared EXTERNAL in the Fortran
C program unit that calls PGFUNY.  It has one argument, the
C y-coordinate at which the x value is required, e.g.
C   REAL FUNCTION FX(Y)
C   REAL Y
C   FX = .....
C   END
C--
C  5-Oct-1983
C 11-May-1990 - remove unnecessary include [TJP].
C 13-DEc-1990 - make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXP
      PARAMETER (MAXP=1000)
      INTEGER  I
      REAL     X(0:MAXP), Y(0:MAXP), DT
      REAL     XMIN, XMAX
C
      IF (N.LT.1 .OR. N.GT.MAXP) THEN
          CALL GRWARN('PGFUNY: invalid arguments')
          RETURN
      END IF
      CALL PGBBUF
C
C Evaluate function.
C
      DT = (YMAX-YMIN)/N
      X(0) = FX(YMIN)
      Y(0) = YMIN
      XMIN = X(0)
      XMAX = X(0)
      DO 10 I=1,N
          X(I) = FX(YMIN+DT*I)
          Y(I) = YMIN + DT*I
          XMIN = MIN(XMIN,X(I))
          XMAX = MAX(XMAX,X(I))
   10 CONTINUE
      DT = 0.05*(XMAX-XMIN)
      IF (DT.EQ.0.0) THEN
          XMIN = XMIN - 1.0
          XMAX = XMAX + 1.0
      ELSE
          XMIN = XMIN - DT
          XMAX = XMAX + DT
      END IF
C
C Define environment if necessary.
C
      IF (PGFLAG.EQ.0) CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,0)
C
C Draw curve.
C
      CALL PGMOVE(X(0),Y(0))
      DO 20 I=1,N
          CALL PGDRAW(X(I),Y(I))
   20 CONTINUE
C
      CALL PGEBUF
      END
C*PGGRAY -- gray-scale map of a 2D data array
C%void cpggray(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float fg, float bg, const float *tr);
C+
      SUBROUTINE PGGRAY (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   FG, BG, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM), FG, BG, TR(6)
C
C Draw gray-scale map of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C of the window and shaded with the shade at each point determined
C by the corresponding array value.  The shade is a number in the
C range 0 to 1 obtained by linear interpolation between the background
C level (BG) and the foreground level (FG), i.e.,
C
C   shade = [A(i,j) - BG] / [FG - BG]
C
C The background level BG can be either less than or greater than the
C foreground level FG.  Points in the array that are outside the range
C BG to FG are assigned shade 0 or 1 as appropriate.
C
C PGGRAY uses two different algorithms, depending how many color
C indices are available in the color index range specified for images.
C (This range is set with routine PGSCIR, and the current or default
C range can be queried by calling routine PGQCIR).
C
C If 16 or more color indices are available, PGGRAY first assigns
C color representations to these color indices to give a linear ramp
C between the background color (color index 0) and the foreground color
C (color index 1), and then calls PGIMAG to draw the image using these
C color indices. In this mode, the shaded region is "opaque": every
C pixel is assigned a color.
C
C If less than 16 color indices are available, PGGRAY uses only
C color index 1, and uses  a "dithering" algorithm to fill in pixels,
C with the shade (computed as above) determining the faction of pixels
C that are filled. In this mode the shaded region is "transparent" and
C allows previously-drawn graphics to show through.
C
C The transformation matrix TR is used to calculate the world
C coordinates of the center of the "cell" that represents each
C array element. The world coordinates of the center of the cell
C corresponding to array element A(I,J) are given by:
C
C          X = TR(1) + TR(2)*I + TR(3)*J
C          Y = TR(4) + TR(5)*I + TR(6)*J
C
C Usually TR(3) and TR(5) are zero -- unless the coordinate
C transformation involves a rotation or shear.  The corners of the
C quadrilateral region that is shaded by PGGRAY are given by
C applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  FG     (input)  : the array value which is to appear with the
C                    foreground color (corresponding to color index 1).
C  BG     (input)  : the array value which is to appear with the
C                    background color (corresponding to color index 0).
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates.
C--
C  2-Sep-1987: remove device-dependent code to routine GRGRAY (TJP).
C  7-Jun-1988: change documentation and argument names (TJP).
C 31-May-1989: allow 1-pixel wide arrays to be plotted (TJP).
C 17-Mar-1994: pass PG scaling info to lower routines (TJP).
C 15-Sep-1994: use PGITF attribute (TJP).
C  8-Feb-1995: use color ramp based on current foreground and background
C              colors (TJP).
C  6-May-1996: allow multiple devives (TJP).
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL PA(6)
      LOGICAL PGNOTO
C
C Check inputs.
C
      IF (PGNOTO('PGGRAY')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
          CALL GRWARN('PGGRAY: invalid range I1:I2, J1:J2')
      ELSE IF (FG.EQ.BG) THEN
          CALL GRWARN('PGGRAY: foreground level = background level')
      ELSE
C
C Call lower-level routine to do the work.
C
          CALL PGBBUF
          CALL PGSAVE
          CALL PGSCI(1)
          PA(1) = TR(1)*PGXSCL(PGID) + PGXORG(PGID)
          PA(2) = TR(2)*PGXSCL(PGID)
          PA(3) = TR(3)*PGXSCL(PGID)
          PA(4) = TR(4)*PGYSCL(PGID) + PGYORG(PGID)
          PA(5) = TR(5)*PGYSCL(PGID)
          PA(6) = TR(6)*PGYSCL(PGID)
          CALL GRGRAY(A, IDIM, JDIM, I1, I2, J1, J2, FG, BG, PA,
     :                PGMNCI(PGID), PGMXCI(PGID), PGITF(PGID))
          CALL PGEBUF
          CALL PGUNSA
      END IF
C-----------------------------------------------------------------------
      END

C*PGHI2D -- cross-sections through a 2D data array
C%void cpghi2d(const float *data, int nxv, int nyv, int ix1, \
C% int ix2, int iy1, int iy2, const float *x, int ioff, float bias, \
C% Logical center, float *ylims);
C+
      SUBROUTINE PGHI2D (DATA, NXV, NYV, IX1, IX2, IY1, IY2, X, IOFF,
     1                   BIAS, CENTER, YLIMS)
      INTEGER NXV, NYV, IX1, IX2, IY1, IY2
      REAL    DATA(NXV,NYV)
      REAL    X(IX2-IX1+1), YLIMS(IX2-IX1+1)
      INTEGER IOFF
      REAL    BIAS
      LOGICAL CENTER
C
C Plot a series of cross-sections through a 2D data array.
C Each cross-section is plotted as a hidden line histogram.  The plot
C can be slanted to give a pseudo-3D effect - if this is done, the
C call to PGENV may have to be changed to allow for the increased X
C range that will be needed.
C
C Arguments:
C  DATA   (input)  : the data array to be plotted.
C  NXV    (input)  : the first dimension of DATA.
C  NYV    (input)  : the second dimension of DATA.
C  IX1    (input)
C  IX2    (input)
C  IY1    (input)
C  IY2    (input)  : PGHI2D plots a subset of the input array DATA.
C                    This subset is delimited in the first (x)
C                    dimension by IX1 and IX2 and the 2nd (y) by IY1
C                    and IY2, inclusively. Note: IY2 < IY1 is
C                    permitted, resulting in a plot with the
C                    cross-sections plotted in reverse Y order.
C                    However, IX2 must be => IX1.
C  X      (input)  : the abscissae of the bins to be plotted. That is,
C                    X(1) should be the X value for DATA(IX1,IY1), and
C                    X should have (IX2-IX1+1) elements.  The program
C                    has to assume that the X value for DATA(x,y) is
C                    the same for all y.
C  IOFF   (input)  : an offset in array elements applied to successive
C                    cross-sections to produce a slanted effect.  A
C                    plot with IOFF > 0 slants to the right, one with
C                    IOFF < 0 slants left.
C  BIAS   (input)  : a bias value applied to each successive cross-
C                    section in order to raise it above the previous
C                    cross-section.  This is in the same units as the
C                    data.
C  CENTER (input)  : if .true., the X values denote the center of the
C                    bins; if .false. the X values denote the lower
C                    edges (in X) of the bins.
C  YLIMS  (input)  : workspace.  Should be an array of at least
C                    (IX2-IX1+1) elements.
C--
C 21-Feb-1984 - Keith Shortridge.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL FIRST,PENDOW,HPLOT,VPLOT
      INTEGER IY,INC,IX,NELMX,IXPT,NOFF
      REAL CBIAS,YNWAS,XNWAS,YN,XN,VTO,VFROM,YLIMWS,YLIM
      REAL PGHIS1
      LOGICAL PGNOTO
C
C Check arguments.
C
      IF (IX1.GT.IX2) RETURN
      IF (PGNOTO('PGHI2D')) RETURN
      CALL PGBBUF
C
C Check Y order.
C
      IF (IY1.GT.IY2) THEN
         INC = -1
      ELSE
         INC = 1
      END IF
C
C Clear limits array.
C
      NELMX = IX2 - IX1 + 1
      DO 10 IX=1,NELMX
         YLIMS(IX) = PGYBLC(PGID)
 10   CONTINUE
C
C Loop through Y values.
C
      NOFF = 0
      CBIAS = 0.
      DO 200 IY=IY1,IY2,INC
         YNWAS = CBIAS
         YLIMWS = YNWAS
         XNWAS = PGHIS1(X,NELMX,CENTER,1+NOFF)
         PENDOW = .FALSE.
         FIRST = .TRUE.
         IXPT = 1
C
C Draw histogram for this Y value.
C
         DO 100 IX=IX1,IX2
            YN = DATA(IX,IY) + CBIAS
            XN = PGHIS1(X,NELMX,CENTER,IXPT+NOFF+1)
            YLIM = YLIMS(IXPT)
C
C Given X and Y old and new values, and limits, see which parts of the
C lines are to be drawn.
C
            IF (YN.GT.YLIM) THEN
               YLIMS(IXPT) = YN
               HPLOT = .TRUE.
               VPLOT = .TRUE.
               VTO = YN
               VFROM = YLIM
               IF (YNWAS.GT.YLIMWS) VFROM = YNWAS
            ELSE
               HPLOT = .FALSE.
               IF (YNWAS.GT.YLIMWS) THEN
                  VPLOT = .TRUE.
                  VFROM = YNWAS
                  VTO = YLIM
               ELSE
                  VPLOT = .FALSE.
               END IF
            END IF
C
C Plot the bin.
C
            IF (VPLOT) THEN
               IF (.NOT.PENDOW) THEN
                  IF (FIRST) THEN
                     CALL GRMOVA(XNWAS,MAX(VTO,CBIAS))
                     FIRST = .FALSE.
                  ELSE
                     CALL GRMOVA(XNWAS,VFROM)
                  END IF
               END IF
               CALL GRLINA(XNWAS,VTO)
               IF (HPLOT) THEN
                  CALL GRLINA(XN,YN)
               END IF
            END IF
            PENDOW = HPLOT
            YLIMWS = YLIM
            YNWAS = YN
            XNWAS = XN
            IXPT = IXPT + 1
 100     CONTINUE
         IF (PENDOW) CALL GRLINA(XN,MAX(YLIM,CBIAS))
C
C If any offset in operation, shift limits array to compensate for it.
C
         IF (IOFF.GT.0) THEN
            DO 110 IX=1,NELMX-IOFF
               YLIMS(IX) = YLIMS(IX+IOFF)
 110        CONTINUE
            DO 120 IX=NELMX-IOFF+1,NELMX
               YLIMS(IX) = PGYBLC(PGID)
 120        CONTINUE
         ELSE IF (IOFF.LT.0) THEN
            DO 130 IX=NELMX,1-IOFF,-1
               YLIMS(IX) = YLIMS(IX+IOFF)
 130        CONTINUE
            DO 140 IX=1,-IOFF
               YLIMS(IX) = PGYBLC(PGID)
 140        CONTINUE
         END IF
         CBIAS = CBIAS + BIAS
         NOFF = NOFF + IOFF
 200  CONTINUE
C
      CALL PGEBUF
      END
      REAL FUNCTION PGHIS1 (X, NELMX, CENTER, IXV)
      LOGICAL CENTER
      INTEGER NELMX, IXV
      REAL X(NELMX)
C
C PGPLOT Internal routine used by PGHI2D.  Calculates the X-value for
C the left hand edge of a given element of the array being plotted.
C
C Arguments -
C
C X (input, real array): abscissae of bins
C NELMX (input, integer): number of bins
C CENTER (Input, logical): if .true., X values denote the center of
C       the bin; if .false., the X values denote the lower edge (in X)
C       of the bin.
C IXV (input, integer): the bin number in question.  Note IXV may be
C       outside the range 1..NELMX, in which case an interpolated
C       value is returned.
C
C 21-Feb-1984 - Keith Shortridge.
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C-----------------------------------------------------------------------
      REAL XN
      INTRINSIC REAL
C
      IF (CENTER) THEN
          IF ((IXV.GT.1).AND.(IXV.LE.NELMX)) THEN
            XN = ( X(IXV-1) + X(IXV) ) * .5
          ELSE IF (IXV.LE.1) THEN
            XN = X(1) - .5 * (X(2) - X(1)) * REAL(3 - 2 * IXV)
          ELSE IF (IXV.GT.NELMX) THEN
            XN = X(NELMX) +.5*(X(NELMX)-X(NELMX-1))*
     1           REAL((IXV-NELMX)*2-1)
          END IF
      ELSE
          IF ((IXV.GE.1).AND.(IXV.LE.NELMX)) THEN
            XN = X(IXV)
          ELSE IF (IXV.LT.1) THEN
            XN = X(1) - ( X(2) - X(1) ) * REAL( 1 - IXV )
          ELSE IF (IXV.GT.NELMX) THEN
            XN = X(NELMX) + ( X(NELMX) - X(NELMX-1)) *
     1           REAL(IXV - NELMX)
          END IF
      END IF
C
      PGHIS1 = XN
      END
C*PGHIST -- histogram of unbinned data
C%void cpghist(int n, const float *data, float datmin, float datmax, \
C% int nbin, int pgflag);
C+
      SUBROUTINE PGHIST(N, DATA, DATMIN, DATMAX, NBIN, PGFLAG)
      INTEGER N
      REAL    DATA(*)
      REAL    DATMIN, DATMAX
      INTEGER NBIN, PGFLAG
C
C Draw a histogram of N values of a variable in array
C DATA(1...N) in the range DATMIN to DATMAX using NBIN bins.  Note
C that array elements which fall exactly on the boundary between
C two bins will be counted in the higher bin rather than the
C lower one; and array elements whose value is less than DATMIN or
C greater than or equal to DATMAX will not be counted at all.
C
C Arguments:
C  N      (input)  : the number of data values.
C  DATA   (input)  : the data values. Note: the dimension of array
C                    DATA must be greater than or equal to N. The
C                    first N elements of the array are used.
C  DATMIN (input)  : the minimum data value for the histogram.
C  DATMAX (input)  : the maximum data value for the histogram.
C  NBIN   (input)  : the number of bins to use: the range DATMIN to
C                    DATMAX is divided into NBIN equal bins and
C                    the number of DATA values in each bin is
C                    determined by PGHIST.  NBIN may not exceed 200.
C  PGFLAG (input)  : if PGFLAG = 1, the histogram is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGHIST to start
C                    a new plot (the x-limits of the window will be
C                    DATMIN and DATMAX; the y-limits will be chosen
C                    automatically.
C                    IF PGFLAG = 2,3 the histogram will be in the same
C                    window and viewport but with a filled area style.
C                    If pgflag=4,5 as for pgflag = 0,1, but simple
C                    line drawn as for PGBIN
C
C--
C Side effects:
C
C The pen position is changed to (DATMAX,0.0) in world coordinates.
C--
C  6-Sep-83:
C 11-Feb-92: fill options added.
C-----------------------------------------------------------------------
      INTEGER  MAXBIN
      PARAMETER (MAXBIN=200)
      INTEGER  I, IBIN, NUM(MAXBIN), NUMMAX, JUNK
      REAL     BINSIZ, PGRND
      REAL     CUR, PREV, XLO, XHI, YLO, YHI
      LOGICAL  PGNOTO
C
      IF (N.LT.1 .OR. DATMAX.LE.DATMIN .OR. NBIN.LT.1 .OR.
     1    NBIN.GT.MAXBIN) THEN
          CALL GRWARN('PGHIST: invalid arguments')
          RETURN
      END IF
      IF (PGNOTO('PGHIST')) RETURN
      CALL PGBBUF
C
C How many values in each bin?
C
      DO 10 IBIN=1,NBIN
          NUM(IBIN) = 0
   10 CONTINUE
      DO 20 I=1,N
          IBIN = (DATA(I)-DATMIN)/(DATMAX-DATMIN)*NBIN+1
          IF (IBIN.GE.1 .AND. IBIN.LE.NBIN) NUM(IBIN) = NUM(IBIN)+1
   20 CONTINUE
      NUMMAX = 0
      DO 30 IBIN=1,NBIN
          NUMMAX = MAX(NUMMAX,NUM(IBIN))
   30 CONTINUE
      BINSIZ = (DATMAX-DATMIN)/NBIN
C
C Boundaries of plot.
C
      XLO = DATMIN
      XHI = DATMAX
      YLO = 0.0
      YHI = PGRND(1.01*NUMMAX,JUNK)
C
C Define environment if necessary.
C
      IF (MOD(PGFLAG,2).EQ.0) THEN
         CALL PGENV(XLO,XHI,YLO,YHI,0,0)
      END IF
C
C Draw Histogram.
C
      IF (PGFLAG/2.EQ.0) THEN
         PREV = 0.0
         XHI=DATMIN
         CALL GRMOVA(DATMIN,0.0)
         DO 40 IBIN=1,NBIN
            CUR = NUM(IBIN)
            XLO=XHI
            XHI = DATMIN + IBIN*BINSIZ
            IF (CUR.EQ.0.0) THEN
               CONTINUE
            ELSE IF (CUR.LE.PREV) THEN
               CALL GRMOVA(XLO,CUR)
               CALL GRLINA(XHI,CUR)
            ELSE
               CALL GRMOVA(XLO,PREV)
               CALL GRLINA(XLO,CUR)
               CALL GRLINA(XHI,CUR)
            END IF
            CALL GRLINA(XHI,0.0)
            PREV = CUR
 40      CONTINUE
      ELSE IF (PGFLAG/2.EQ.1) THEN
         PREV = 0.0
         XHI = DATMIN
         DO 50 IBIN=1,NBIN
            CUR = NUM(IBIN)
            XLO=XHI
            XHI = DATMIN + IBIN*BINSIZ
            IF (CUR.EQ.0.0) THEN
               CONTINUE
            ELSE
               CALL PGRECT(XLO,XHI,0.0,CUR)
            END IF
 50      CONTINUE
      ELSE IF (PGFLAG/2.EQ.2) THEN
         PREV = 0.0
         CALL GRMOVA(DATMIN,0.0)
         XHI=DATMIN
         DO 60 IBIN=1,NBIN
            CUR = NUM(IBIN)
            XLO = XHI
            XHI = DATMIN + IBIN*BINSIZ
            IF (CUR.EQ.0.0 .AND. PREV.EQ.0.0) THEN
               CALL GRMOVA(XHI,0.0)
            ELSE 
               CALL GRLINA(XLO,CUR)
               IF(CUR.NE.0.0) THEN
                  CALL GRLINA(XHI,CUR)
               ELSE
                  CALL GRMOVA(XHI,CUR)
               ENDIF
            END IF
            PREV = CUR
 60      CONTINUE
      END IF
C     
      CALL PGEBUF
      END
C.PGHTCH -- hatch a polygonal area (internal routine)
C.
      SUBROUTINE PGHTCH(N, X, Y, DA)
      INTEGER N
      REAL X(*), Y(*), DA
C
C Hatch a polygonal area using equi-spaced parallel lines. The lines
C are drawn using the current line attributes: line style, line width,
C and color index. Cross-hatching can be achieved by calling this
C routine twice.
C
C Limitations: the hatching will not be done correctly if the
C polygon is so complex that a hatch line intersects more than
C 32 of its sides.
C
C Arguments:
C  N      (input)  : the number of vertices of the polygonal.
C  X,Y    (input)  : the (x,y) world-coordinates of the vertices
C                    (in order).
C  DA      (input) : 0.0 for normal hatching, 90.0 for perpendicular
C                    hatching.
C--
C Reference: I.O. Angel and G. Griffith "High-resolution computer
C graphics using Fortran 77", Halsted Press, 1987.
C
C 18-Feb-1995 [TJP].
C-----------------------------------------------------------------------
C
C MAXP is the maximum number of intersections any hatch line may make 
C with the sides of the polygon.
C
      INTEGER MAXP
      PARAMETER (MAXP=32)
      INTEGER NP(MAXP), I,J, II,JJ, NMIN,NMAX, NX, NI, NNP
      REAL ANGLE, SEPN, PHASE
      REAL RMU(MAXP), DX,DY, C, CMID,CMIN,CMAX, SX,SY, EX,EY, DELTA
      REAL QX,QY, R, RMU1, RMU2, XI,YI, BX,BY
      REAL DH, XS1, XS2, YS1, YS2, XL, XR, YT, YB, DINDX, DINDY
C
C Check arguments.
C
      IF (N.LT.3) RETURN
      CALL PGQHS(ANGLE, SEPN, PHASE)
      ANGLE = ANGLE + DA
      IF (SEPN.EQ.0.0) RETURN
C
C The unit spacing is 1 percent of the smaller of the height or
C width of the view surface. The line-spacing (DH), in inches, is
C obtained by multiplying this by argument SEPN.
C
      CALL PGQVSZ(1, XS1, XS2, YS1, YS2)
      DH = SEPN*MIN(ABS(XS2-XS1),ABS(YS2-YS1))/100.0
C
C DINDX and DINDY are the scales in inches per world-coordinate unit.
C
      CALL PGQVP(1, XS1, XS2, YS1, YS2)
      CALL PGQWIN(XL, XR, YB, YT)
      IF (XR.NE.XL .AND. YT.NE.YB) THEN
         DINDX = (XS2 - XS1) / (XR - XL)
         DINDY = (YS2 - YS1) / (YT - YB)
      ELSE
         RETURN
      END IF
C
C Initialize.
C
      CALL PGBBUF
C
C The vector (SX,SY) is a vector length DH perpendicular to
C the hatching lines, which have vector (DX,DY).
C
      DX = COS(ANGLE/57.29578)
      DY = SIN(ANGLE/57.29578)
      SX = (-DH)*DY
      SY = DH*DX
C
C The hatch lines are labelled by a parameter C, the distance from
C the coordinate origin. Calculate CMID, the C-value of the line
C that passes through the hatching reference point (BX,BY), and
C CMIN and CMAX, the range of C-values spanned by lines that intersect
C the polygon.
C
      BX = PHASE*SX
      BY = PHASE*SY
      CMID = DX*BY - DY*BX
      CMIN = DX*Y(1)*DINDY - DY*X(1)*DINDX
      CMAX = CMIN
      DO 10 I=2,N
         C = DX*Y(I)*DINDY - DY*X(I)*DINDX
         CMIN = MIN(C,CMIN)
         CMAX = MAX(C,CMAX)
 10   CONTINUE
C
C Compute integer labels for the hatch lines; N=0 is the line
C which passes through the reference point; NMIN and NMAX define
C the range of labels for lines that intersect the polygon.
C [Note that INT truncates towards zero; we need FLOOR and CEIL
C functions.]
C
      CMIN = (CMIN-CMID)/DH
      CMAX = (CMAX-CMID)/DH
      NMIN = INT(CMIN)
      IF (REAL(NMIN).LT.CMIN) NMIN = NMIN+1
      NMAX = INT(CMAX)
      IF (REAL(NMAX).GT.CMAX) NMAX = NMAX-1
C
C Each iteration of the following loop draws one hatch line.
C
      DO 60 J=NMIN,NMAX
C
C The parametric representation of this hatch line is
C (X,Y) = (QX,QY) + RMU*(DX,DY).
C
         QX = BX + REAL(J)*SX
         QY = BY + REAL(J)*SY
C
C Find the NX intersections of this line with the edges of the polygon.
C
         NX = 0
         NI = N
         DO 20 I=1,N
            EX = (X(I) - X(NI))*DINDX
            EY = (Y(I) - Y(NI))*DINDY
            DELTA = EX*DY - EY*DX
            IF (ABS(DELTA).LT.1E-5) THEN
C                 -- lines are parallel
            ELSE
C                 -- lines intersect in (XI,YI)
               R = ((QX-X(NI)*DINDX)*DY - (QY-Y(NI)*DINDY)*DX)/DELTA
               IF (R.GT.0.0 .AND. R.LE.1.0) THEN
                  IF (NX.LT.MAXP) NX = NX+1
                  NP(NX) = NX
                  IF (ABS(DX).GT.0.5) THEN
                     XI = X(NI)*DINDX + R*EX
                     RMU(NX) = (XI-QX)/DX
                  ELSE
                     YI = Y(NI)*DINDY + R*EY
                     RMU(NX) = (YI-QY)/DY
                  END IF
               END IF
            END IF
            NI = I
 20      CONTINUE
C     
C The RMU array now contains the intersections. Sort them into order.
C
         DO 40 II=1,NX-1
            DO 30 JJ=II+1,NX
               IF (RMU(NP(II)).LT.RMU(NP(JJ))) THEN
                  NNP = NP(II)
                  NP(II) = NP(JJ)
                  NP(JJ) = NNP
               END IF
 30         CONTINUE
 40      CONTINUE
C
C Join the intersections in pairs.
C
         NI = 1
C         -- do while NI < NX
 50      IF (NI .LT. NX) THEN
            RMU1 = RMU(NP(NI))
            RMU2 = RMU(NP(NI+1))
            CALL PGMOVE((QX+RMU1*DX)/DINDX, (QY+RMU1*DY)/DINDY)
            CALL PGDRAW((QX+RMU2*DX)/DINDX, (QY+RMU2*DY)/DINDY)
            NI = NI+2
            GOTO 50
         END IF
 60   CONTINUE
C
C Tidy up.
C
      CALL PGEBUF
C
      END
C*PGIDEN -- write username, date, and time at bottom of plot
C%void cpgiden(void);
C+
      SUBROUTINE PGIDEN
C
C Write username, date, and time at bottom of plot.
C
C Arguments: none.
C--
C  9-Feb-1988
C 10-Sep-1990 : adjust position of text [TJP]
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER L, M, CF, CI, LW
      CHARACTER*64 TEXT
      REAL D, CH
C
      CALL PGBBUF
C
C Get information for annotation.
C
      CALL GRUSER(TEXT, L)
      TEXT(L+1:) = ' '
      CALL GRDATE(TEXT(L+2:), M)
      L = L+1+M
C
C Save current attributes.
C
      CALL PGQCF(CF)
      CALL PGQCI(CI)
      CALL PGQLW(LW)
      CALL PGQCH(CH)
C
C Change attributes and write text.
C
      CALL PGSCF(1)
      CALL PGSCI(1)
      CALL PGSLW(1)
      CALL PGSCH(0.6)
      CALL GRLEN(TEXT(1:L),D)
      CALL GRTEXT(.FALSE., 0.0, .TRUE., PGXSZ(PGID)-D-2.0,
     1            2.0+PGYSZ(PGID)/130.0, TEXT(1:L))
C
C Restore attributes.
C
      CALL PGSCF(CF)
      CALL PGSCI(CI)
      CALL PGSLW(LW)
      CALL PGSCH(CH)
      CALL PGEBUF
C
      END
C*PGIMAG -- color image from a 2D data array
C%void cpgimag(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float a1, float a2, const float *tr);
C+
      SUBROUTINE PGIMAG (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM), A1, A2, TR(6)
C
C Draw a color image of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C of the window. Each element of the array is represented in the image
C by a small quadrilateral, which is filled with a color specified by
C the corresponding array value.
C
C The subroutine uses color indices in the range C1 to C2, which can
C be specified by calling PGSCIR before PGIMAG. The default values
C for C1 and C2 are device-dependent; these values can be determined by
C calling PGQCIR. Note that color representations should be assigned to
C color indices C1 to C2 by calling PGSCR before calling PGIMAG. On some
C devices (but not all), the color representation can be changed after
C the call to PGIMAG by calling PGSCR again.
C
C Array values in the range A1 to A2 are mapped on to the range of
C color indices C1 to C2, with array values <= A1 being given color
C index C1 and values >= A2 being given color index C2. The mapping
C function for intermediate array values can be specified by
C calling routine PGSITF before PGIMAG; the default is linear.
C
C On devices which have no available color indices (C1 > C2),
C PGIMAG will return without doing anything. On devices with only
C one color index (C1=C2), all array values map to the same color
C which is rather uninteresting. An image is always "opaque",
C i.e., it obscures all graphical elements previously drawn in
C the region.
C
C The transformation matrix TR is used to calculate the world
C coordinates of the center of the "cell" that represents each
C array element. The world coordinates of the center of the cell
C corresponding to array element A(I,J) are given by:
C
C          X = TR(1) + TR(2)*I + TR(3)*J
C          Y = TR(4) + TR(5)*I + TR(6)*J
C
C Usually TR(3) and TR(5) are zero -- unless the coordinate
C transformation involves a rotation or shear.  The corners of the
C quadrilateral region that is shaded by PGIMAG are given by
C applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  A1     (input)  : the array value which is to appear with shade C1.
C  A2     (input)  : the array value which is to appear with shade C2.
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates.
C--
C 15-Sep-1994: new routine [TJP].
C 21-Jun-1995: minor change to header comments [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL PA(6)
      LOGICAL PGNOTO
C
C Check inputs.
C
      IF (PGNOTO('PGIMAG')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
          CALL GRWARN('PGIMAG: invalid range I1:I2, J1:J2')
      ELSE IF (A1.EQ.A2) THEN
          CALL GRWARN('PGIMAG: foreground level = background level')
      ELSE IF (PGMNCI(PGID).GT.PGMXCI(PGID)) THEN
          CALL GRWARN('PGIMAG: not enough colors available')
      ELSE
C
C Call lower-level routine to do the work.
C
          CALL PGBBUF
          PA(1) = TR(1)*PGXSCL(PGID) + PGXORG(PGID)
          PA(2) = TR(2)*PGXSCL(PGID)
          PA(3) = TR(3)*PGXSCL(PGID)
          PA(4) = TR(4)*PGYSCL(PGID) + PGYORG(PGID)
          PA(5) = TR(5)*PGYSCL(PGID)
          PA(6) = TR(6)*PGYSCL(PGID)
          CALL GRIMG0(A, IDIM, JDIM, I1, I2, J1, J2, A1, A2, PA,
     :                PGMNCI(PGID), PGMXCI(PGID), PGITF(PGID))
          CALL PGEBUF
      END IF
C-----------------------------------------------------------------------
      END
C PGINIT -- initialize PGPLOT (internal routine)
C
      SUBROUTINE PGINIT
C
C Initialize PGPLOT. This routine should be called once during program
C execution, before any other PGPLOT routines.
C--
C Last modified: 1996 Apr 30 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER CALLED, I
      SAVE CALLED
      DATA CALLED /0/
C
      IF (CALLED.EQ.0) THEN
         PGID = 0
         DO 10 I=1,PGMAXD
            PGDEVS(I) = 0
 10      CONTINUE
         CALL GRINIT
         CALLED = 1
      END IF
C
      RETURN
      END
C*PGLAB -- write labels for x-axis, y-axis, and top of plot
C%void cpglab(const char *xlbl, const char *ylbl, const char *toplbl);
C+
      SUBROUTINE PGLAB (XLBL, YLBL, TOPLBL)
      CHARACTER*(*) XLBL, YLBL, TOPLBL
C
C Write labels outside the viewport. This routine is a simple
C interface to PGMTXT, which should be used if PGLAB is inadequate.
C
C Arguments:
C  XLBL   (input) : a label for the x-axis (centered below the
C                   viewport).
C  YLBL   (input) : a label for the y-axis (centered to the left
C                   of the viewport, drawn vertically).
C  TOPLBL (input) : a label for the entire plot (centered above the
C                   viewport).
C--
C 11-May-1990 - remove unnecessary include - TJP.
C-----------------------------------------------------------------------
      CALL PGBBUF
      CALL PGMTXT('T', 2.0, 0.5, 0.5, TOPLBL)
      CALL PGMTXT('B', 3.2, 0.5, 0.5, XLBL)
      CALL PGMTXT('L', 2.2, 0.5, 0.5, YLBL)
      CALL PGEBUF
      END
C*PGLABEL -- non-standard alias for PGLAB
C+
      SUBROUTINE PGLABEL (XLBL, YLBL, TOPLBL)
      CHARACTER*(*) XLBL, YLBL, TOPLBL
C
C See description of PGLAB.
C--
      CALL PGLAB (XLBL, YLBL, TOPLBL)
      END
C*PGLCUR -- draw a line using the cursor
C%void cpglcur(int maxpt, int *npt, float *x, float *y);
C+
      SUBROUTINE PGLCUR (MAXPT, NPT, X, Y)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
C
C Interactive routine for user to enter a polyline by use of
C the cursor.  Routine allows user to Add and Delete vertices;
C vertices are joined by straight-line segments.
C
C Arguments:
C  MAXPT  (input)  : maximum number of points that may be accepted.
C  NPT    (in/out) : number of points entered; should be zero on
C                    first call.
C  X      (in/out) : array of x-coordinates (dimension at least MAXPT).
C  Y      (in/out) : array of y-coordinates (dimension at least MAXPT).
C
C Notes:
C
C (1) On return from the program, cursor points are returned in
C the order they were entered. Routine may be (re-)called with points
C already defined in X,Y (# in NPT), and they will be plotted
C first, before editing.
C
C (2) User commands: the user types single-character commands
C after positioning the cursor: the following are accepted:
C   A (Add)    - add point at current cursor location.
C   D (Delete) - delete last-entered point.
C   X (eXit)   - leave subroutine.
C--
C  5-Aug-1984 - new routine [TJP].
C 16-Jul-1988 - correct error in delete operation [TJP].
C 13-Dec-1990 - change warnings to messages [TJP].
C  3-Sep-1992 - fixed erase first point bug under Add option [JM/TJP].
C  7-Sep-1994 - use PGBAND [TJP].
C  2-Aug-1995 - remove dependence on common block [TJP].
C-----------------------------------------------------------------------
      LOGICAL  PGNOTO
      CHARACTER*1 LETTER
      INTEGER  PGBAND, I, SAVCOL, MODE
      REAL     XP, YP, XREF, YREF
      REAL     XBLC, XTRC, YBLC, YTRC
C
C Check that PGPLOT is in the correct state.
C
      IF (PGNOTO('PGLCUR')) RETURN
C
C Save current color.
C
      CALL GRQCI(SAVCOL)
C
C Put current line-segments on screen.
C
      IF (NPT.EQ.1) THEN
          CALL PGPT(1,X(1),Y(1),1)
      END IF
      IF (NPT.GT.0) THEN
          CALL GRMOVA(X(1),Y(1))
          DO 10 I=2,NPT
              CALL GRLINA(X(I),Y(I))
   10     CONTINUE
      END IF
C
C Start with the cursor in the middle of the box,
C unless lines have already been drawn.
C
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
      IF (NPT.GT.0) THEN
          XP = X(NPT)
          YP = Y(NPT)
      ELSE
          XP = 0.5*(XBLC+XTRC)
          YP = 0.5*(YBLC+YTRC)
      END IF
C
C Loop over cursor inputs.
C
      MODE = 0
  100 XREF = XP
      YREF = YP
      IF (PGBAND(MODE,1,XREF,YREF,XP,YP,LETTER).NE.1) RETURN
      CALL GRTOUP(LETTER,LETTER)
      MODE = 1
C
C A (ADD) command:
C
      IF (LETTER .EQ. 'A') THEN
          IF (NPT.GE.MAXPT) THEN
            CALL GRMSG('ADD ignored (too many points).')
              GOTO 100
          END IF
          NPT = NPT+1
          X(NPT) = XP
          Y(NPT) = YP
          IF (NPT.EQ.1) THEN
C           -- first point: draw a dot
            CALL GRMOVA(X(NPT),Y(NPT))
            CALL PGPT(1,X(NPT),Y(NPT),1)
          ELSE
C           -- nth point: draw from (n-1) to (n)
            CALL GRLINA(X(NPT),Y(NPT))
          END IF
          CALL GRTERM
C
C D (DELETE) command:
C
      ELSE IF (LETTER.EQ.'D') THEN
          IF (NPT.LE.0) THEN
            CALL GRMSG('DELETE ignored (there are no points left).')
            GOTO 100
          END IF
          IF (NPT.GT.1) THEN
C           -- delete nth point: erase from (n-1) to (n)
            CALL GRMOVA(X(NPT-1),Y(NPT-1))
            CALL GRSCI(0)
            CALL GRLINA(X(NPT),Y(NPT))
            CALL GRSCI(SAVCOL)
            CALL GRMOVA(X(NPT-1),Y(NPT-1))
            CALL GRTERM
          ELSE IF (NPT.EQ.1) THEN
C           -- delete first point: erase dot
            CALL GRSCI(0)
            CALL PGPT(1,X(NPT),Y(NPT),1)
            CALL GRSCI(SAVCOL)
          END IF
          NPT = NPT-1
          IF (NPT.EQ.0) THEN
            XP = 0.5*(XBLC+XTRC)
            YP = 0.5*(YBLC+YTRC)
          ELSE
            XP = X(NPT)
            YP = Y(NPT)
          END IF
          IF (NPT.EQ.1) THEN
C           -- delete 2nd point: redraw dot at first point
            CALL PGPT(1,X(1),Y(1),1)
          END IF
C
C X (EXIT) command:
C
      ELSE IF (LETTER.EQ.'X') THEN
          CALL GRETXT
          RETURN
C
C Illegal command:
C
      ELSE
          CALL GRMSG('Commands are A (add), D (delete), X (exit).')
      END IF
C
      GOTO 100
      END
C*PGLDEV -- list available device types on standard output
C%void cpgldev(void);
C+
      SUBROUTINE PGLDEV
C
C Writes (to standard output) a list of all device types available in
C the current PGPLOT installation.
C
C Arguments: none.
C--
C 5-Aug-1986 - [AFT].
C 1-Aug-1988 - add version number [TJP].
C 24-Apr-1989 - add copyright notice [TJP].
C 13-Dec-1990 - changed warnings to messages [TJP].
C 26-Feb-1997 - revised description [TJP].
C 18-Mar-1997 - revised [TJP].
C-----------------------------------------------------------------------
      CHARACTER*16 GVER
      INTEGER L
      CHARACTER*10 T
      CHARACTER*64 D
      INTEGER I, N, TLEN, DLEN, INTER

C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Report version and copyright.
C
      CALL PGQINF('VERSION', GVER, L)
      CALL GRMSG('PGPLOT '//GVER(:L)//
     1           ' Copyright 1997 California Institute of Technology')
C
C Find number of device types.
C
      CALL PGQNDT(N)
C
C Loop through device-type list (twice).

      CALL GRMSG('Interactive devices:')
      DO 10 I=1,N
         CALL PGQDT(I, T, TLEN, D, DLEN, INTER)
         IF (TLEN.GT.0 .AND. INTER.EQ.1)
     :        CALL GRMSG('   '//T//' '//D(1:DLEN))
 10   CONTINUE
      CALL GRMSG('Non-interactive file formats:')
      DO 20 I=1,N
         CALL PGQDT(I, T, TLEN, D, DLEN, INTER)
         IF (TLEN.GT.0 .AND. INTER.EQ.0)
     :        CALL GRMSG('   '//T//' '//D(1:DLEN))
 20   CONTINUE
C
      END
C*PGLEN -- find length of a string in a variety of units
C%void cpglen(int units, const char *string, float *xl, float *yl);
C+
      SUBROUTINE PGLEN (UNITS, STRING, XL, YL)
      REAL XL, YL
      INTEGER UNITS
      CHARACTER*(*) STRING
C
C Work out length of a string in x and y directions 
C
C Input
C  UNITS    :  0 => answer in normalized device coordinates
C              1 => answer in inches
C              2 => answer in mm
C              3 => answer in absolute device coordinates (dots)
C              4 => answer in world coordinates
C              5 => answer as a fraction of the current viewport size
C
C  STRING   :  String of interest
C Output
C  XL       :  Length of string in x direction
C  YL       :  Length of string in y direction
C
C--
C 15-Sep-1989 - new routine (Neil Killeen)
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL    D
C
      IF (PGNOTO('PGLEN')) RETURN
C
C   Work out length of a string in absolute device coordinates (dots)
C   and then convert
C
      CALL GRLEN (STRING, D)
C
      IF (UNITS.EQ.0) THEN
        XL = D / PGXSZ(PGID)
        YL = D / PGYSZ(PGID)
      ELSE IF (UNITS.EQ.1) THEN
        XL = D / PGXPIN(PGID)
        YL = D / PGYPIN(PGID)
      ELSE IF (UNITS.EQ.2) THEN
        XL = 25.4 * D / PGXPIN(PGID)
        YL = 25.4 * D / PGYPIN(PGID)
      ELSE IF (UNITS.EQ.3) THEN
        XL = D
        YL = D
      ELSE IF (UNITS.EQ.4) THEN
        XL = D / ABS(PGXSCL(PGID))
        YL = D / ABS(PGYSCL(PGID))
      ELSE IF (UNITS.EQ.5) THEN
        XL = D / PGXLEN(PGID)
        YL = D / PGYLEN(PGID)
      ELSE
        CALL GRWARN('Illegal value for UNITS in routine PGLEN')
      END IF
C
      RETURN
      END
C*PGLINE -- draw a polyline (curve defined by line-segments)
C%void cpgline(int n, const float *xpts, const float *ypts);
C+
      SUBROUTINE PGLINE (N, XPTS, YPTS)
      INTEGER  N
      REAL     XPTS(*), YPTS(*)
C
C Primitive routine to draw a Polyline. A polyline is one or more
C connected straight-line segments.  The polyline is drawn using
C the current setting of attributes color-index, line-style, and
C line-width. The polyline is clipped at the edge of the window.
C
C Arguments:
C  N      (input)  : number of points defining the line; the line
C                    consists of (N-1) straight-line segments.
C                    N should be greater than 1 (if it is 1 or less,
C                    nothing will be drawn).
C  XPTS   (input)  : world x-coordinates of the points.
C  YPTS   (input)  : world y-coordinates of the points.
C
C The dimension of arrays X and Y must be greater than or equal to N.
C The "pen position" is changed to (X(N),Y(N)) in world coordinates
C (if N > 1).
C--
C 27-Nov-1986
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGLINE')) RETURN
      IF (N.LT.2) RETURN
C
      CALL PGBBUF
      CALL GRMOVA(XPTS(1),YPTS(1))
      DO 10 I=2,N
         CALL GRLINA(XPTS(I),YPTS(I))
 10   CONTINUE
      CALL PGEBUF
      END
C*PGMOVE -- move pen (change current pen position)
C%void cpgmove(float x, float y);
C+
      SUBROUTINE PGMOVE (X, Y)
      REAL X, Y
C
C Primitive routine to move the "pen" to the point with world
C coordinates (X,Y). No line is drawn.
C
C Arguments:
C  X      (input)  : world x-coordinate of the new pen position.
C  Y      (input)  : world y-coordinate of the new pen position.
C--
C (29-Dec-1983)
C-----------------------------------------------------------------------
      CALL GRMOVA(X,Y)
      END
C*PGMTEXT -- non-standard alias for PGMTXT
C+
      SUBROUTINE PGMTEXT (SIDE, DISP, COORD, FJUST, TEXT)
      CHARACTER*(*) SIDE, TEXT
      REAL DISP, COORD, FJUST
C
C See description of PGMTXT.
C--
      CALL PGMTXT (SIDE, DISP, COORD, FJUST, TEXT)
      END
C*PGMTXT -- write text at position relative to viewport
C%void cpgmtxt(const char *side, float disp, float coord, \
C% float fjust, const char *text);
C+
      SUBROUTINE PGMTXT (SIDE, DISP, COORD, FJUST, TEXT)
      CHARACTER*(*) SIDE, TEXT
      REAL DISP, COORD, FJUST
C
C Write text at a position specified relative to the viewport (outside
C or inside).  This routine is useful for annotating graphs. It is used
C by routine PGLAB.  The text is written using the current values of
C attributes color-index, line-width, character-height, and
C character-font.
C
C Arguments:
C  SIDE   (input)  : must include one of the characters 'B', 'L', 'T',
C                    or 'R' signifying the Bottom, Left, Top, or Right
C                    margin of the viewport. If it includes 'LV' or
C                    'RV', the string is written perpendicular to the
C                    frame rather than parallel to it.
C  DISP   (input)  : the displacement of the character string from the
C                    specified edge of the viewport, measured outwards
C                    from the viewport in units of the character
C                    height. Use a negative value to write inside the
C                    viewport, a positive value to write outside.
C  COORD  (input)  : the location of the character string along the
C                    specified edge of the viewport, as a fraction of
C                    the length of the edge.
C  FJUST  (input)  : controls justification of the string parallel to
C                    the specified edge of the viewport. If
C                    FJUST = 0.0, the left-hand end of the string will
C                    be placed at COORD; if JUST = 0.5, the center of
C                    the string will be placed at COORD; if JUST = 1.0,
C                    the right-hand end of the string will be placed at
C                    at COORD. Other values between 0 and 1 give inter-
C                    mediate placing, but they are not very useful.
C  TEXT   (input) :  the text string to be plotted. Trailing spaces are
C                    ignored when justifying the string, but leading
C                    spaces are significant.
C
C--
C 18-Apr-1983
C 15-Aug-1987 - fix BBUF/EBUF error.
C 27-Aug-1987 - fix justification error if XPERIN.ne.YPERIN.
C 05-Sep-1989 - change so that DISP has some effect for 'RV' and 
C               'LV' options [nebk]
C 16-Oct-1993 - erase background of opaque text.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL ANGLE, D, X, Y, RATIO, XBOX(4), YBOX(4)
      INTEGER CI, I, L, GRTRIM
      CHARACTER*20 TEST
C
      IF (PGNOTO('PGMTXT')) RETURN
C
      L = GRTRIM(TEXT)
      IF (L.LT.1) RETURN
      D = 0.0
      IF (FJUST.NE.0.0) CALL GRLEN(TEXT(1:L),D)
      D = D*FJUST
      RATIO = PGYPIN(PGID)/PGXPIN(PGID)
      CALL GRTOUP(TEST,SIDE)
      IF (INDEX(TEST,'B').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) + COORD*PGXLEN(PGID) - D
          Y = PGYOFF(PGID) - PGYSP(PGID)*DISP
      ELSE IF (INDEX(TEST,'LV').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) - PGYSP(PGID)*DISP - D
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - 0.3*PGYSP(PGID)
      ELSE IF (INDEX(TEST,'L').NE.0) THEN
          ANGLE = 90.0
          X = PGXOFF(PGID) - PGYSP(PGID)*DISP
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - D*RATIO
      ELSE IF (INDEX(TEST,'T').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) + COORD*PGXLEN(PGID) - D
          Y = PGYOFF(PGID) + PGYLEN(PGID) + PGYSP(PGID)*DISP
      ELSE IF (INDEX(TEST,'RV').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) + PGXLEN(PGID) + PGYSP(PGID)*DISP - D
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - 0.3*PGYSP(PGID)
      ELSE IF (INDEX(TEST,'R').NE.0) THEN
          ANGLE = 90.0
          X = PGXOFF(PGID) + PGXLEN(PGID) + PGYSP(PGID)*DISP
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - D*RATIO
      ELSE
          CALL GRWARN('Invalid "SIDE" argument in PGMTXT.')
          RETURN
      END IF
      CALL PGBBUF
      IF (PGTBCI(PGID).GE.0) THEN
          CALL GRQTXT (ANGLE, X, Y, TEXT(1:L), XBOX, YBOX)
          DO 25 I=1,4
              XBOX(I) = (XBOX(I)-PGXORG(PGID))/PGXSCL(PGID)
              YBOX(I) = (YBOX(I)-PGYORG(PGID))/PGYSCL(PGID)
   25     CONTINUE
          CALL PGQCI(CI)
          CALL PGSCI(PGTBCI(PGID))
          CALL GRFA(4, XBOX, YBOX)
          CALL PGSCI(CI)
      END IF
      CALL GRTEXT(.FALSE.,ANGLE,.TRUE., X, Y, TEXT(1:L))
      CALL PGEBUF
      END
C*PGNCUR -- mark a set of points using the cursor
C%void cpgncur(int maxpt, int *npt, float *x, float *y, int symbol);
C+
      SUBROUTINE PGNCUR (MAXPT, NPT, X, Y, SYMBOL)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
      INTEGER SYMBOL
C
C Interactive routine for user to enter data points by use of
C the cursor.  Routine allows user to Add and Delete points.  The
C points are returned in order of increasing x-coordinate, not in the
C order they were entered.
C
C Arguments:
C  MAXPT  (input)  : maximum number of points that may be accepted.
C  NPT    (in/out) : number of points entered; should be zero on
C                    first call.
C  X      (in/out) : array of x-coordinates.
C  Y      (in/out) : array of y-coordinates.
C  SYMBOL (input)  : code number of symbol to use for marking
C                    entered points (see PGPT).
C
C Note (1): The dimension of arrays X and Y must be greater than or
C equal to MAXPT.
C
C Note (2): On return from the program, cursor points are returned in
C increasing order of X. Routine may be (re-)called with points
C already defined in X,Y (number in NPT), and they will be plotted
C first, before editing.
C
C Note (3): User commands: the user types single-character commands
C after positioning the cursor: the following are accepted:
C A (Add)    - add point at current cursor location.
C D (Delete) - delete nearest point to cursor.
C X (eXit)   - leave subroutine.
C--
C 27-Nov-1983
C  9-Jul-1983 - modified to use GRSCI instead of GRSETLI [TJP].
C 13-Dec-1990 - changed warnings to messages [TJP].
C  2-Aug-1995 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      CHARACTER*1 LETTER
      LOGICAL  PGNOTO
      INTEGER  PGCURS, I, J, SAVCOL
      REAL     DELTA, XP, YP, XPHYS, YPHYS
      REAL     XMIN, XIP, YIP
      REAL     XBLC, XTRC, YBLC, YTRC
C
C Check that PGPLOT is in the correct state.
C
      IF (PGNOTO('PGNCUR')) RETURN
C
C Save current color.
C
      CALL GRQCI(SAVCOL)
C
C Put current points on screen.
C
      IF (NPT.NE.0) CALL PGPT(NPT,X,Y,SYMBOL)
C
C Start with the cursor in the middle of the viewport.
C
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
      XP = 0.5*(XBLC+XTRC)
      YP = 0.5*(YBLC+YTRC)
C
C Loop over cursor inputs.
C
  100 IF (PGCURS(XP,YP,LETTER).NE.1) RETURN
      IF (LETTER.EQ.CHAR(0)) RETURN
      CALL GRTOUP(LETTER,LETTER)
C
C A (ADD) command:
C
      IF (LETTER .EQ. 'A') THEN
          IF (NPT.GE.MAXPT) THEN
              CALL GRMSG('ADD ignored (too many points).')
              GOTO 100
          END IF
C         ! Find what current points new point is between.
          DO 120 J=1,NPT
              IF (XP.LT.X(J)) GOTO 122
  120     CONTINUE
          J = NPT + 1
C         ! New point is beyond last current
  122     CONTINUE
C         ! J is vector location where new point should be included.
          DO 140 I=NPT,J,-1
              X(I+1) = X(I)
              Y(I+1) = Y(I)
  140     CONTINUE
          NPT = NPT + 1
C         ! Add new point to point array.
          X(J) = XP
          Y(J) = YP
          CALL PGPT(1,X(J),Y(J),SYMBOL)
          CALL GRTERM
C
C D (DELETE) command:
C
      ELSE IF (LETTER.EQ.'D') THEN
          IF (NPT.LE.0) THEN
              CALL GRMSG('DELETE ignored (there are no points left).')
              GOTO 100
          END IF
          XMIN = 1.E+08
C         ! Look for point closest in radius.
C         ! Convert cursor points to physical.
          XPHYS = PGXORG(PGID) + XP*PGXSCL(PGID)
          YPHYS = PGYORG(PGID) + YP*PGYSCL(PGID)
          DO 220 I=1,NPT
C             ! Convert array points to physical.
              XIP = PGXORG(PGID) + X(I)*PGXSCL(PGID)
              YIP = PGYORG(PGID) + Y(I)*PGYSCL(PGID)
              DELTA = SQRT( (XIP-XPHYS)**2 + (YIP-YPHYS)**2 )
              IF (DELTA.LT.XMIN) THEN
                 XMIN = DELTA
                 J = I
              END IF
  220     CONTINUE
C         ! Remove point from screen by writing in background color.
          CALL GRSCI(0)
          CALL PGPT(1,X(J),Y(J),SYMBOL)
          CALL GRSCI(SAVCOL)
          CALL GRTERM
C         ! Remove point from cursor array.
          NPT = NPT-1
          DO 240 I=J,NPT
              X(I) = X(I+1)
              Y(I) = Y(I+1)
  240     CONTINUE
C
C X (EXIT) command:
C
      ELSE IF (LETTER.EQ.'X') THEN
          CALL GRETXT
          RETURN
C
C Illegal command:
C
      ELSE
          CALL GRMSG('Commands are A (add), D (delete), X (exit).')
      END IF
C
      GOTO 100
      END
C*PGNCURSE -- non-standard alias for PGNCUR
C+
      SUBROUTINE PGNCURSE (MAXPT, NPT, X, Y, SYMBOL)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
      INTEGER SYMBOL
C
C See description of PGNCUR.
C--
      CALL PGNCUR (MAXPT, NPT, X, Y, SYMBOL)
      END
C
      LOGICAL FUNCTION PGNOTO (RTN)
      CHARACTER*(*) RTN
C
C PGPLOT (internal routine): Test whether a PGPLOT device is open and
C print a message if not. Usage:
C     LOGICAL PGNOTO
C     IF (PGNOTO('routine')) RETURN
C
C Arguments:
C
C RTN (input, character): routine name to be include in message.
C
C Returns:
C     .TRUE. if PGPLOT is not open.
C--
C 11-Nov-1994
C 21-Dec-1995 - revised for multiple devices.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      CHARACTER*80 TEXT
C
      CALL PGINIT
      PGNOTO = .FALSE.
      IF (PGID.LT.1 .OR. PGID.GT.PGMAXD) THEN
         PGNOTO = .TRUE.
         TEXT = RTN//': no graphics device has been selected'
         CALL GRWARN(TEXT)
      ELSE IF (PGDEVS(PGID).NE.1) THEN
         PGNOTO = .TRUE.
         TEXT = RTN//': selected graphics device is not open'
         CALL GRWARN(TEXT)
      END IF
      RETURN
      END

C
C.PGNPL -- Work out how many numerals there are in an integer
C.
      SUBROUTINE PGNPL (NMAX, N, NPL)
C
      INTEGER NMAX, N, NPL
C
C     Work out how many numerals there are in an integer for use with 
C     format statements.   
C     e.g.  N=280 => NPL=3,   N=-3 => NPL=2
C
C     Input:
C       NMAX   :   If > 0, issue a warning that N is going to
C                  exceed the format statement field size if NPL 
C                  exceeds NMAX
C       N      :   Integer of interest
C     Output:
C       NPL    :   Number of numerals
C
C-
C  20-Apr-1991 -- new routine (Neil Killeen)
C-------------------------------------------------------------------------
      IF (N.EQ.0) THEN
        NPL = 1
      ELSE
        NPL = INT(LOG10(REAL(ABS(N)))) + 1
      END IF
      IF (N.LT.0) NPL = NPL + 1
C
      IF (NMAX.GT.0 .AND. NPL.GT.NMAX) 
     *  CALL GRWARN ('PGNPL: output conversion error likely; '
     *               //'number too big for format')
C
      RETURN
      END
C*PGNUMB -- convert a number into a plottable character string
C%void cpgnumb(int mm, int pp, int form, char *string, \
C% int *string_length);
C+
      SUBROUTINE PGNUMB (MM, PP, FORM, STRING, NC)
      INTEGER MM, PP, FORM
      CHARACTER*(*) STRING
      INTEGER NC
C
C This routine converts a number into a decimal character
C representation. To avoid problems of floating-point roundoff, the
C number must be provided as an integer (MM) multiplied by a power of 10
C (10**PP).  The output string retains only significant digits of MM,
C and will be in either integer format (123), decimal format (0.0123),
C or exponential format (1.23x10**5). Standard escape sequences \u, \d 
C raise the exponent and \x is used for the multiplication sign.
C This routine is used by PGBOX to create numeric labels for a plot.
C
C Formatting rules:
C   (a) Decimal notation (FORM=1):
C       - Trailing zeros to the right of the decimal sign are
C         omitted
C       - The decimal sign is omitted if there are no digits
C         to the right of it
C       - When the decimal sign is placed before the first digit
C         of the number, a zero is placed before the decimal sign
C       - The decimal sign is a period (.)
C       - No spaces are placed between digits (ie digits are not
C         grouped in threes as they should be)
C       - A leading minus (-) is added if the number is negative
C   (b) Exponential notation (FORM=2):
C       - The exponent is adjusted to put just one (non-zero)
C         digit before the decimal sign
C       - The mantissa is formatted as in (a), unless its value is
C         1 in which case it and the multiplication sign are omitted
C       - If the power of 10 is not zero and the mantissa is not
C         zero, an exponent of the form \x10\u[-]nnn is appended,
C         where \x is a multiplication sign (cross), \u is an escape
C         sequence to raise the exponent, and as many digits nnn
C         are used as needed
C   (c) Automatic choice (FORM=0):
C         Decimal notation is used if the absolute value of the
C         number is less than 10000 or greater than or equal to
C         0.01. Otherwise exponential notation is used.
C
C Arguments:
C  MM     (input)
C  PP     (input)  : the value to be formatted is MM*10**PP.
C  FORM   (input)  : controls how the number is formatted:
C                    FORM = 0 -- use either decimal or exponential
C                    FORM = 1 -- use decimal notation
C                    FORM = 2 -- use exponential notation
C  STRING (output) : the formatted character string, left justified.
C                    If the length of STRING is insufficient, a single
C                    asterisk is returned, and NC=1.
C  NC     (output) : the number of characters used in STRING:
C                    the string to be printed is STRING(1:NC).
C--
C 23-Nov-1983
C  9-Feb-1988 [TJP] - Use temporary variable to avoid illegal character
C                     assignments; remove non-standard DO loops.
C 15-Dec-1988 [TJP] - More corrections of the same sort.
C 27-Nov-1991 [TJP] - Change code for multiplication sign.
C 23-Jun-1994 [TJP] - Partial implementation of FORM=1 and 2.
C-----------------------------------------------------------------------
      CHARACTER*1 BSLASH
      CHARACTER*2 TIMES, UP, DOWN
      CHARACTER*20 WORK, WEXP, TEMP
      INTEGER M, P, ND, I, J, K, NBP
      LOGICAL MINUS
C
C Define backslash (escape) character and escape sequences.
C
      BSLASH = CHAR(92)
      TIMES  = BSLASH//'x'
      UP     = BSLASH//'u'
      DOWN   = BSLASH//'d'
C
C Zero is always printed as "0".
C
      IF (MM.EQ.0) THEN
          STRING = '0'
          NC = 1
          RETURN
      END IF
C
C If negative, make a note of that fact.
C
      MINUS = MM.LT.0
      M = ABS(MM)
      P = PP
C
C Convert M to a left-justified digit string in WORK. As M is a
C positive integer, it cannot use more than 10 digits (2147483647).
C
      J = 10
   10 IF (M.NE.0) THEN
          K = MOD(M,10)
          M = M/10
          WORK(J:J) = CHAR(ICHAR('0')+K)
          J = J-1
       GOTO 10
      END IF
      TEMP = WORK(J+1:)
      WORK = TEMP
      ND = 10-J
C
C Remove right-hand zeros, and increment P for each one removed.
C ND is the final number of significant digits in WORK, and P the
C power of 10 to be applied. Number of digits before decimal point
C is NBP.
C
   20 IF (WORK(ND:ND).EQ.'0') THEN
          ND = ND-1
          P = P+1
       GOTO 20
      END IF
      NBP = ND+MIN(P,0)
C
C Integral numbers of 4 or less digits are formatted as such.
C
      IF ((P.GE.0) .AND. ((FORM.EQ.0 .AND. P+ND.LE.4) .OR.
     :                    (FORM.EQ.1 .AND. P+ND.LE.10))) THEN
          DO 30 I=1,P
              ND = ND+1
              WORK(ND:ND) = '0'
   30     CONTINUE
          P = 0
C
C If NBP is 4 or less, simply insert a decimal point in the right place.
C
      ELSE IF (FORM.NE.2.AND.NBP.GE.1.AND.NBP.LE.4.AND.NBP.LT.ND) THEN
          TEMP = WORK(NBP+1:ND)
          WORK(NBP+2:ND+1) = TEMP
          WORK(NBP+1:NBP+1) = '.'
          ND = ND+1
          P = 0
C
C Otherwise insert a decimal point after the first digit, and adjust P.
C
      ELSE
          P = P + ND - 1
          IF (FORM.NE.2 .AND. P.EQ.-1) THEN
              TEMP = WORK
              WORK = '0'//TEMP
              ND = ND+1
              P = 0
          ELSE IF (FORM.NE.2 .AND. P.EQ.-2) THEN
              TEMP = WORK
              WORK = '00'//TEMP
              ND = ND+2
              P = 0
          END IF
          IF (ND.GT.1) THEN
              TEMP = WORK(2:ND)
              WORK(3:ND+1) = TEMP
              WORK(2:2) = '.'
              ND = ND + 1
          END IF
      END IF
C
C Add exponent if necessary.
C
      IF (P.NE.0) THEN
          WORK(ND+1:ND+6) = TIMES//'10'//UP
          ND = ND+6
          IF (P.LT.0) THEN
              P = -P
              ND = ND+1
              WORK(ND:ND) = '-'
          END IF
          J = 10
   40     IF (P.NE.0) THEN
              K = MOD(P,10)
              P = P/10
              WEXP(J:J) = CHAR(ICHAR('0')+K)
              J = J-1
           GOTO 40
          END IF
          WORK(ND+1:) = WEXP(J+1:10)
          ND = ND+10-J
          IF (WORK(1:3).EQ.'1'//TIMES) THEN
              TEMP = WORK(4:)
              WORK = TEMP
              ND = ND-3
          END IF
          WORK(ND+1:ND+2) = DOWN
          ND = ND+2
      END IF
C
C Add minus sign if necessary and move result to output.
C
      IF (MINUS) THEN
         TEMP = WORK(1:ND)
         STRING = '-'//TEMP
         NC = ND+1
      ELSE
         STRING = WORK(1:ND)
         NC = ND
      END IF
C
C Check result fits.
C
      IF (NC.GT.LEN(STRING)) THEN
          STRING = '*'
          NC = 1
      END IF
      END
C*PGOLIN -- mark a set of points using the cursor
C%void cpgolin(int maxpt, int *npt, float *x, float *y, int symbol);
C+
      SUBROUTINE PGOLIN (MAXPT, NPT, X, Y, SYMBOL)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
      INTEGER SYMBOL
C
C Interactive routine for user to enter data points by use of
C the cursor.  Routine allows user to Add and Delete points.  The
C points are returned in the order that they were entered (unlike
C PGNCUR).
C
C Arguments:
C  MAXPT  (input)  : maximum number of points that may be accepted.
C  NPT    (in/out) : number of points entered; should be zero on
C                    first call.
C  X      (in/out) : array of x-coordinates.
C  Y      (in/out) : array of y-coordinates.
C  SYMBOL (input)  : code number of symbol to use for marking
C                    entered points (see PGPT).
C
C Note (1): The dimension of arrays X and Y must be greater than or
C equal to MAXPT.
C
C Note (2): On return from the program, cursor points are returned in
C the order they were entered. Routine may be (re-)called with points
C already defined in X,Y (number in NPT), and they will be plotted
C first, before editing.
C
C Note (3): User commands: the user types single-character commands
C after positioning the cursor: the following are accepted:
C A (Add)    - add point at current cursor location.
C D (Delete) - delete the last point entered.
C X (eXit)   - leave subroutine.
C--
C  4-Nov-1985 - new routine (adapted from PGNCUR) - TJP.
C 13-Dec-1990 - change warnings to messages [TJP].
C  7-Sep-1994 - use PGBAND [TJP].
C  2-Aug-1995 - remove dependence on common block [TJP].
C-----------------------------------------------------------------------
      LOGICAL  PGNOTO
      CHARACTER*1 LETTER
      INTEGER  PGBAND, SAVCOL
      REAL     XP, YP, XREF, YREF
      REAL     XBLC, XTRC, YBLC, YTRC
C
C Check that PGPLOT is in the correct state.
C
      IF (PGNOTO('PGOLIN')) RETURN
C
C Save current color.
C
      CALL GRQCI(SAVCOL)
C
C Put current points on screen.  Position cursor on last point,
C or in middle viewport if there are no current points.
C
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
      IF (NPT.NE.0) THEN
          CALL PGPT(NPT,X,Y,SYMBOL)
          XP = X(NPT)
          YP = Y(NPT)
      ELSE
          XP = 0.5*(XBLC+XTRC)
          YP = 0.5*(YBLC+YTRC)
      END IF
C
C Loop over cursor inputs.
C
  100 XREF = XP
      YREF = YP
      IF (PGBAND(0,1,XREF,YREF,XP,YP,LETTER).NE.1) RETURN
      IF (LETTER.EQ.CHAR(0)) RETURN
      CALL GRTOUP(LETTER,LETTER)
C
C A (ADD) command:
C
      IF (LETTER .EQ. 'A') THEN
          IF (NPT.GE.MAXPT) THEN
              CALL GRMSG('ADD ignored (too many points).')
          ELSE
              NPT = NPT + 1
              X(NPT) = XP
              Y(NPT) = YP
              CALL PGPT(1,X(NPT),Y(NPT),SYMBOL)
              CALL GRTERM
          END IF
C
C D (DELETE) command:
C
      ELSE IF (LETTER.EQ.'D') THEN
          IF (NPT.LE.0) THEN
              CALL GRMSG('DELETE ignored (there are no points left).')
          ELSE
              CALL GRSCI(0)
              CALL PGPT(1,X(NPT),Y(NPT),SYMBOL)
              XP = X(NPT)
              YP = Y(NPT)
              CALL GRSCI(SAVCOL)
              CALL GRTERM
              NPT = NPT-1
          END IF
C
C X (EXIT) command:
C
      ELSE IF (LETTER.EQ.'X') THEN
          CALL GRETXT
          RETURN
C
C Illegal command:
C
      ELSE
          CALL GRMSG('Commands are A (add), D (delete), X (exit).')
      END IF
C
      GOTO 100
      END
C*PGOPEN -- open a graphics device
C%int cpgopen(const char *device);
C+
      INTEGER FUNCTION PGOPEN (DEVICE)
      CHARACTER*(*) DEVICE
C
C Open a graphics device for PGPLOT output. If the device is
C opened successfully, it becomes the selected device to which
C graphics output is directed until another device is selected
C with PGSLCT or the device is closed with PGCLOS.
C
C The value returned by PGOPEN should be tested to ensure that
C the device was opened successfully, e.g.,
C
C       ISTAT = PGOPEN('plot.ps/PS')
C       IF (ISTAT .LE. 0 ) STOP
C
C Note that PGOPEN must be declared INTEGER in the calling program.
C
C The DEVICE argument is a character constant or variable; its value
C should be one of the following:
C
C (1) A complete device specification of the form 'device/type' or
C     'file/type', where 'type' is one of the allowed PGPLOT device
C     types (installation-dependent) and 'device' or 'file' is the 
C     name of a graphics device or disk file appropriate for this type.
C     The 'device' or 'file' may contain '/' characters; the final
C     '/' delimits the 'type'. If necessary to avoid ambiguity,
C     the 'device' part of the string may be enclosed in double
C     quotation marks.
C (2) A device specification of the form '/type', where 'type' is one
C     of the allowed PGPLOT device types. PGPLOT supplies a default
C     file or device name appropriate for this device type.
C (3) A device specification with '/type' omitted; in this case
C     the type is taken from the environment variable PGPLOT_TYPE,
C     if defined (e.g., setenv PGPLOT_TYPE PS). Because of possible
C     confusion with '/' in file-names, omitting the device type
C     in this way is not recommended.
C (4) A blank string (' '); in this case, PGOPEN will use the value
C     of environment variable PGPLOT_DEV as the device specification,
C     or '/NULL' if the environment variable is undefined.
C (5) A single question mark, with optional trailing spaces ('?'); in
C     this case, PGPLOT will prompt the user to supply the device
C     specification, with a prompt string of the form
C         'Graphics device/type (? to see list, default XXX):'
C     where 'XXX' is the default (value of environment variable
C     PGPLOT_DEV).
C (6) A non-blank string in which the first character is a question
C     mark (e.g., '?Device: '); in this case, PGPLOT will prompt the
C     user to supply the device specification, using the supplied
C     string as the prompt (without the leading question mark but
C     including any trailing spaces).
C
C In cases (5) and (6), the device specification is read from the
C standard input. The user should respond to the prompt with a device
C specification of the form (1), (2), or (3). If the user types a 
C question-mark in response to the prompt, a list of available device
C types is displayed and the prompt is re-issued. If the user supplies
C an invalid device specification, the prompt is re-issued. If the user
C responds with an end-of-file character, e.g., ctrl-D in UNIX, program
C execution is aborted; this  avoids the possibility of an infinite
C prompting loop.  A programmer should avoid use of PGPLOT-prompting
C if this behavior is not desirable.
C
C The device type is case-insensitive (e.g., '/ps' and '/PS' are 
C equivalent). The device or file name may be case-sensitive in some
C operating systems.
C
C Examples of valid DEVICE arguments:
C
C (1)  'plot.ps/ps', 'dir/plot.ps/ps', '"dir/plot.ps"/ps', 
C      'user:[tjp.plots]plot.ps/PS'
C (2)  '/ps'      (PGPLOT interprets this as 'pgplot.ps/ps')
C (3)  'plot.ps'  (if PGPLOT_TYPE is defined as 'ps', PGPLOT
C                  interprets this as 'plot.ps/ps')
C (4)  '   '      (if PGPLOT_DEV is defined)
C (5)  '?  '
C (6)  '?Device specification for PGPLOT: '
C
C [This routine was added to PGPLOT in Version 5.1.0. Older programs
C use PGBEG instead.]
C
C Returns:
C  PGOPEN          : returns either a positive value, the
C                    identifier of the graphics device for use with
C                    PGSLCT, or a 0 or negative value indicating an
C                    error. In the event of error a message is
C                    written on the standard error unit.
C Arguments:
C  DEVICE  (input) : the 'device specification' for the plot device
C                    (see above).
C--
C 22-Dec-1995 - new routine [TJP].
C 14-May-1996 - device '? ' should not give a blank prompt [TJP].
C-----------------------------------------------------------------------
      INCLUDE       'pgplot.inc'
      INTEGER       DEFTYP,GRDTYP,GROPEN,L,LR,IC1, LPROMP
      INTEGER       GRGCOM, IER, LDEFDE, UNIT, ISTAT
      REAL          DUMMY,DUMMY2,XCSZ, XSZ, YSZ
      CHARACTER*128 DEFDEV, PROMPT
      CHARACTER*20  DEFSTR
      CHARACTER*256 REQ
      LOGICAL JUNK
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Get the default device/type (environment variable PGPLOT_DEV).
C
      CALL GRGENV('DEV', DEFDEV, LDEFDE)
      IF (LDEFDE.EQ.0) THEN
         DEFDEV = '/NULL'
         LDEFDE = 5
      END IF
C
C Open the plot file; default type is given by environment variable
C PGPLOT_TYPE.
C
      CALL GRGENV('TYPE', DEFSTR, L)
      IF (L.EQ.0) THEN
          DEFTYP = 0
      ELSE
          CALL GRTOUP(DEFSTR, DEFSTR)
          DEFTYP = GRDTYP(DEFSTR(1:L))
      END IF
      IF (DEVICE.EQ.' ') THEN
C        -- Blank device string: use default device and type.
         ISTAT = GROPEN(DEFTYP,UNIT,DEFDEV(1:LDEFDE),PGID)
      ELSE IF (DEVICE(1:1).EQ.'?') THEN
         IF (DEVICE.EQ.'?') THEN
C           -- Device string is a ingle question mark: prompt user
C           -- for device/type
            PROMPT = 'Graphics device/type (? to see list, default '
     :           //DEFDEV(1:LDEFDE)//'): '
            LPROMP = LDEFDE + 48
         ELSE
C           -- Device string starts with a question mark: use it
C           -- as a prompt
            PROMPT = DEVICE(2:)
            LPROMP = LEN(DEVICE)-1
         END IF
   10    IER = GRGCOM(REQ, PROMPT(1:LPROMP), LR)
         IF (IER.NE.1) THEN
            CALL GRWARN('Error reading device specification')
            PGOPEN = -1
            RETURN
         END IF
         IF (LR.LT.1 .OR. REQ.EQ.' ') THEN
            REQ = DEFDEV(1:LDEFDE)
         ELSE IF (REQ(1:1).EQ.'?') THEN
            CALL PGLDEV
            GOTO 10
         END IF
         ISTAT = GROPEN(DEFTYP,UNIT,REQ,PGID)
         IF (ISTAT.NE.1) GOTO 10
      ELSE
          ISTAT = GROPEN(DEFTYP,UNIT,DEVICE,PGID)
      END IF
C
C Failed to open plot file?
C
      IF (ISTAT.NE.1) THEN
         PGOPEN = - 1
         RETURN
      END IF
C
C Success: determine device characteristics.
C
      IF (PGID.LT.0 .OR. PGID.GT.PGMAXD) CALL
     1       GRWARN('Something terribly wrong in PGOPEN')
      PGDEVS(PGID) = 1
      PGADVS(PGID) = 0
      PGPFIX(PGID) = .FALSE.
      CALL GRSIZE(PGID,XSZ,YSZ,DUMMY,DUMMY2,
     1            PGXPIN(PGID),PGYPIN(PGID))
      CALL GRCHSZ(PGID,XCSZ,DUMMY,PGXSP(PGID),PGYSP(PGID))
      PGROWS(PGID)= .TRUE.
      PGNX(PGID)  = 1
      PGNY(PGID)  = 1
      PGXSZ(PGID) = XSZ
      PGYSZ(PGID) = YSZ
      PGNXC(PGID) = 1
      PGNYC(PGID) = 1
      CALL GRQTYP(DEFSTR,JUNK)
C
C Set the prompt state to ON, so that terminal devices pause between
C pages; this can be changed with PGASK.
C
      CALL PGASK(.TRUE.)
C
C If environment variable PGPLOT_BUFFER is defined (any value),
C start buffering output.
C
      PGBLEV(PGID) = 0
      CALL GRGENV('BUFFER', DEFSTR, L)
      IF (L.GT.0) CALL PGBBUF
C
C Set background and foreground colors if requested.
C
      CALL GRGENV('BACKGROUND', DEFSTR, L)
      IF (L.GT.0) CALL PGSCRN(0, DEFSTR(1:L), IER)
      CALL GRGENV('FOREGROUND', DEFSTR, L)
      IF (L.GT.0) CALL PGSCRN(1, DEFSTR(1:L), IER)
C
C Set default attributes.
C
      CALL PGSCI(1)
      CALL PGSLS(1)
      CALL PGSLW(1)
      CALL PGSCH(1.0)
      CALL PGSCF(1)
      CALL PGSFS(1)
      CALL PGSAH(1, 45.0, 0.3)
      CALL PGSTBG(-1)
      CALL PGSHS(45.0, 1.0, 0.0)
      CALL PGSCLP(1)
C
C Set the default range of color indices available for images (16 to
C device maximum, if device maximum >= 16; otherwise not possible).
C Select linear transfer function.
C
      CALL GRQCOL(IC1, PGMXCI(PGID))
      PGMNCI(PGID) = 16
      IF (PGMXCI(PGID).LT.16) PGMXCI(PGID) = 0
      PGITF(PGID) = 0
C
C Set the default window (unit square).
C
      PGXBLC(PGID) = 0.0
      PGXTRC(PGID) = 1.0
      PGYBLC(PGID) = 0.0
      PGYTRC(PGID) = 1.0
C
C Set the default viewport.
C
      CALL PGVSTD
C
      PGOPEN = PGID
      RETURN
      END
C*PGPAGE -- advance to new page
C%void cpgpage(void);
C+
      SUBROUTINE PGPAGE
C
C Advance plotter to a new page or panel, clearing the screen if
C necessary. If the "prompt state" is ON (see PGASK), confirmation is
C requested from the user before clearing the screen. If the view
C surface has been subdivided into panels with PGBEG or PGSUBP, then
C PGPAGE advances to the next panel, and if the current panel is the
C last on the page, PGPAGE clears the screen or starts a new sheet of
C paper.  PGPAGE does not change the PGPLOT window or the viewport
C (in normalized device coordinates); but note that if the size of the
C view-surface is changed externally (e.g., by a workstation window
C manager) the size of the viewport is changed in proportion.
C
C Arguments: none
C--
C  7-Feb-1983
C 23-Sep-1984 - correct bug: call GRTERM at end (if flush mode set).
C 31-Jan-1985 - make closer to Fortran-77.
C 19-Nov-1987 - explicitly clear the screen if device is interactive;
C               this restores the behavior obtained with older versions
C               of GRPCKG.
C  9-Feb-1988 - move prompting into routine GRPROM.
C 11-Apr-1989 - change name to PGPAGE.
C 10-Sep-1990 - add identification labelling.
C 11-Feb-1992 - check if device size has changed.
C  3-Sep-1992 - allow column ordering of panels.
C 17-Nov-1994 - move identification to drivers.
C 23-Nov-1994 - fix bug: character size not getting reset.
C 23-Jan-1995 - rescale viewport if size of view surface  has changed.
C  4-Feb-1997 - bug fix; character size was not correctly indexed by
C               device ID.
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      CHARACTER*16 STR
      LOGICAL      INTER, PGNOTO
      REAL DUM1, DUM2, XS, YS, XVP1, XVP2, YVP1, YVP2
C
      IF (PGNOTO('PGPAGE')) RETURN
C
      IF (PGROWS(PGID)) THEN
        PGNXC(PGID) = PGNXC(PGID) + 1
        IF (PGNXC(PGID).GT.PGNX(PGID)) THEN
          PGNXC(PGID) = 1
          PGNYC(PGID) = PGNYC(PGID) + 1
          IF (PGNYC(PGID).GT.PGNY(PGID)) PGNYC(PGID) = 1
        END IF
      ELSE
        PGNYC(PGID) = PGNYC(PGID) + 1
        IF (PGNYC(PGID).GT.PGNY(PGID)) THEN
          PGNYC(PGID) = 1
          PGNXC(PGID) = PGNXC(PGID) + 1
          IF (PGNXC(PGID).GT.PGNX(PGID)) PGNXC(PGID) = 1
        END IF
      END IF
      IF (PGNXC(PGID).EQ.1 .AND. PGNYC(PGID).EQ.1) THEN
          IF (PGADVS(PGID).EQ.1 .AND. PGPRMP(PGID)) THEN
              CALL GRTERM
              CALL GRPROM
          END IF
          CALL GRPAGE
          IF (.NOT.PGPFIX(PGID)) THEN
C             -- Get current viewport in NDC.
              CALL PGQVP(0, XVP1, XVP2, YVP1, YVP2)
C             -- Reset view surface size if it has changed
              CALL GRSIZE(PGID, XS,YS, DUM1,DUM2,
     1                    PGXPIN(PGID), PGYPIN(PGID))
              PGXSZ(PGID) = XS/PGNX(PGID)
              PGYSZ(PGID) = YS/PGNY(PGID)
C             -- and character size
              CALL PGSCH(PGCHSZ(PGID))
C             -- and viewport
              CALL PGSVP(XVP1, XVP2, YVP1, YVP2)
          END IF
C
C If the device is interactive, call GRBPIC to clear the page.
C (If the device is not interactive, GRBPIC will be called
C automatically before the first output; omitting the call here
C ensures that a blank page is not output.)
C
          CALL GRQTYP(STR,INTER)
          IF (INTER) CALL GRBPIC
      END IF
      PGXOFF(PGID) = PGXVP(PGID) + (PGNXC(PGID)-1)*PGXSZ(PGID)
      PGYOFF(PGID) = PGYVP(PGID) + 
     1               (PGNY(PGID)-PGNYC(PGID))*PGYSZ(PGID)
C
C Window the plot in the new viewport.
C
      CALL PGVW
      PGADVS(PGID) = 1
      CALL GRTERM
      END
C*PGPANL -- switch to a different panel on the view surface
C%void cpgpanl(int nxc, int nyc);
C+
      SUBROUTINE PGPANL(IX, IY)
      INTEGER IX, IY
C
C Start plotting in a different panel. If the view surface has been
C divided into panels by PGBEG or PGSUBP, this routine can be used to
C move to a different panel. Note that PGPLOT does not remember what
C viewport and window were in use in each panel; these should be reset
C if necessary after calling PGPANL. Nor does PGPLOT clear the panel:
C call PGERAS after calling PGPANL to do this.
C
C Arguments:
C  IX     (input)  : the horizontal index of the panel (in the range
C                    1 <= IX <= number of panels in horizontal
C                    direction).
C  IY     (input)  : the vertical index of the panel (in the range
C                    1 <= IY <= number of panels in horizontal
C                    direction).
C--
C  1-Dec-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      LOGICAL PGNOTO
C
C Check that a device is open.
C
      IF (PGNOTO('PGPANL')) RETURN
C
C Check arguments.
C
      IF (IX.LT.1 .OR. IX.GT.PGNX(PGID) .OR.
     :    IY.LT.1 .OR. IY.GT.PGNY(PGID)) THEN
         CALL GRWARN('PGPANL: the requested panel does not exist')
C
C Adjust the viewport to the new panel and window the plot
C in the new viewport.
C
      ELSE
         PGNXC(PGID)  = IX
         PGNYC(PGID)  = IY
         PGXOFF(PGID) = PGXVP(PGID) + (IX-1)*PGXSZ(PGID)
         PGYOFF(PGID) = PGYVP(PGID) + (PGNY(PGID)-IY)*PGYSZ(PGID)
         CALL PGVW
      END IF
C
      END
C*PGPAP -- change the size of the view surface 
C%void cpgpap(float width, float aspect);
C+
      SUBROUTINE PGPAP (WIDTH, ASPECT)
      REAL WIDTH, ASPECT
C
C This routine changes the size of the view surface ("paper size") to a
C specified width and aspect ratio (height/width), in so far as this is
C possible on the specific device. It is always possible to obtain a
C view surface smaller than the default size; on some devices (e.g.,
C printers that print on roll or fan-feed paper) it is possible to 
C obtain a view surface larger than the default.
C 
C This routine should be called either immediately after PGBEG or
C immediately before PGPAGE. The new size applies to all subsequent
C images until the next call to PGPAP.
C
C Arguments:
C  WIDTH  (input)  : the requested width of the view surface in inches;
C                    if WIDTH=0.0, PGPAP will obtain the largest view
C                    surface available consistent with argument ASPECT.
C                    (1 inch = 25.4 mm.)
C  ASPECT (input)  : the aspect ratio (height/width) of the view
C                    surface; e.g., ASPECT=1.0 gives a square view
C                    surface, ASPECT=0.618 gives a horizontal
C                    rectangle, ASPECT=1.618 gives a vertical rectangle.
C--
C (22-Apr-1983; bug fixed 7-Jun-1988)
C  6-Oct-1990 Modified to work correctly on interactive devices.
C 13-Dec-1990 Make errors non-fatal [TJP].
C 14-Sep-1994 Fix bug to do with drivers changing view surface size.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     HDEF, HMAX, HREQ, WDEF, WMAX, WREQ
      REAL     XSMAX, YSMAX, XSZ, YSZ
C
      IF (PGNOTO('PGPAP'))  RETURN
      IF (WIDTH.LT.0.0 .OR. ASPECT.LE.0.0) THEN
          CALL GRWARN('PGPAP ignored: invalid arguments')
          RETURN
      END IF
C
      PGPFIX(PGID) = .TRUE.
C     -- Find default size WDEF, HDEF and maximum size WMAX, HMAX
C        of view surface (inches)
      CALL GRSIZE(PGID,XSZ,YSZ,XSMAX,YSMAX,
     1            PGXPIN(PGID),PGYPIN(PGID))
      WDEF = XSZ/PGXPIN(PGID)
      HDEF = YSZ/PGYPIN(PGID)
      WMAX = XSMAX/PGXPIN(PGID)
      HMAX = YSMAX/PGYPIN(PGID)
C     -- Find desired size WREQ, HREQ of view surface (inches)
      IF (WIDTH.NE.0.0) THEN
          WREQ = WIDTH
          HREQ = WIDTH*ASPECT
      ELSE
          WREQ = WDEF
          HREQ = WDEF*ASPECT
          IF (HREQ.GT.HDEF) THEN
              WREQ = HDEF/ASPECT
              HREQ = HDEF
          END IF
      END IF
C     -- Scale the requested view surface to fit the maximum
C        dimensions
      IF (WMAX.GT.0.0 .AND. WREQ.GT.WMAX) THEN
          WREQ = WMAX
          HREQ = WMAX*ASPECT
      END IF
      IF (HMAX.GT.0.0 .AND. HREQ.GT.HMAX) THEN
          WREQ = HMAX/ASPECT
          HREQ = HMAX
      END IF
C     -- Establish the new view surface dimensions
      XSZ = WREQ*PGXPIN(PGID)
      YSZ = HREQ*PGYPIN(PGID)
      CALL GRSETS(PGID,XSZ,YSZ)
      PGXSZ(PGID) = XSZ/PGNX(PGID)
      PGYSZ(PGID) = YSZ/PGNY(PGID)
      PGNXC(PGID) = PGNX(PGID)
      PGNYC(PGID) = PGNY(PGID)
      CALL PGSCH(1.0)
      CALL PGVSTD
      END
C*PGPAPER -- non-standard alias for PGPAP
C+
      SUBROUTINE PGPAPER (WIDTH, ASPECT)
      REAL WIDTH, ASPECT
C
C See description of PGPAP.
C--
      CALL PGPAP (WIDTH, ASPECT)
      END
C*PGPIXL -- draw pixels
C%void cpgpixl(const int *ia, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGPIXL (IA, IDIM, JDIM, I1, I2, J1, J2, 
     1                   X1, X2, Y1, Y2)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X1, X2, Y1, Y2
C
C Draw lots of solid-filled (tiny) rectangles aligned with the
C coordinate axes. Best performance is achieved when output is
C directed to a pixel-oriented device and the rectangles coincide
C with the pixels on the device. In other cases, pixel output is
C emulated.
C
C The subsection of the array IA defined by indices (I1:I2, J1:J2)
C is mapped onto world-coordinate rectangle defined by X1, X2, Y1
C and Y2. This rectangle is divided into (I2 - I1 + 1) * (J2 - J1 + 1)
C small rectangles. Each of these small rectangles is solid-filled
C with the color index specified by the corresponding element of 
C IA.
C
C On most devices, the output region is "opaque", i.e., it obscures
C all graphical elements previously drawn in the region. But on
C devices that do not have erase capability, the background shade
C is "transparent" and allows previously-drawn graphics to show
C through.
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X1, Y1 (input)  : world coordinates of one corner of the output
C                    region
C  X2, Y2 (input)  : world coordinates of the opposite corner of the
C                    output region
C--
C 16-Jan-1991 - [GvG]
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
C Check inputs.
C
      IF (PGNOTO('PGPIXL')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
         CALL GRWARN('PGPIXL: invalid range I1:I2, J1:J2')
      ELSE
C
C Call lower-level routine to do the work.
C
         CALL PGBBUF
         CALL GRPIXL(IA, IDIM, JDIM, I1, I2, J1, J2, X1, X2, Y1, Y2)
         CALL PGEBUF
      END IF
C-----------------------------------------------------------------------
      END
C*PGPNTS -- draw several graph markers, not all the same
C%void cpgpnts(int n, const float *x, const float *y, \
C% const int *symbol, int ns);
C+
      SUBROUTINE PGPNTS (N, X, Y, SYMBOL, NS)
      INTEGER N, NS
      REAL X(*), Y(*)
      INTEGER SYMBOL(*)
C
C Draw Graph Markers. Unlike PGPT, this routine can draw a different
C symbol at each point. The markers are drawn using the current values
C of attributes color-index, line-width, and character-height
C (character-font applies if the symbol number is >31).  If the point
C to be marked lies outside the window, no marker is drawn.  The "pen 
C position" is changed to (XPTS(N),YPTS(N)) in world coordinates
C (if N > 0).
C
C Arguments:
C  N      (input)  : number of points to mark.
C  X      (input)  : world x-coordinate of the points.
C  Y      (input)  : world y-coordinate of the points.
C  SYMBOL (input)  : code number of the symbol to be plotted at each
C                    point (see PGPT).
C  NS     (input)  : number of values in the SYMBOL array.  If NS <= N,
C                    then the first NS points are drawn using the value
C                    of SYMBOL(I) at (X(I), Y(I)) and SYMBOL(1) for all
C                    the values of (X(I), Y(I)) where I > NS.
C
C Note: the dimension of arrays X and Y must be greater than or equal
C to N and the dimension of the array SYMBOL must be greater than or
C equal to NS.  If N is 1, X and Y may be scalars (constants or
C variables).  If NS is 1, then SYMBOL may be a scalar.  If N is
C less than 1, nothing is drawn.
C--
C 11-Mar-1991 - new routine [JM].
C 26-Feb-1997 - revised to use PGPT1 [TJP].
C-----------------------------------------------------------------------
      INTEGER I, SYMB
C
      IF (N.LT.1) RETURN
      CALL PGBBUF
      DO 10 I=1,N
          IF (I .LE. NS) THEN
              SYMB = SYMBOL(I)
          ELSE
              SYMB = SYMBOL(1)
          END IF
          CALL PGPT1(X(I), Y(I), SYMB)
   10 CONTINUE
      CALL PGEBUF
      END
C*PGPOINT -- non-standard alias for PGPT
C+
      SUBROUTINE PGPOINT (N, XPTS, YPTS, SYMBOL)
      INTEGER N
      REAL XPTS(*), YPTS(*)
      INTEGER SYMBOL
C
C See description of PGPT.
C--
      CALL PGPT (N, XPTS, YPTS, SYMBOL)
      END
C*PGPOLY -- draw a polygon, using fill-area attributes
C%void cpgpoly(int n, const float *xpts, const float *ypts);
C+
      SUBROUTINE PGPOLY (N, XPTS, YPTS)
      INTEGER N
      REAL XPTS(*), YPTS(*)
C
C Fill-area primitive routine: shade the interior of a closed
C polygon in the current window.  The action of this routine depends
C on the setting of the Fill-Area Style attribute (see PGSFS).
C The polygon is clipped at the edge of the
C window. The pen position is changed to (XPTS(1),YPTS(1)) in world
C coordinates (if N > 1).  If the polygon is not convex, a point is
C assumed to lie inside the polygon if a straight line drawn to
C infinity intersects and odd number of the polygon's edges.
C
C Arguments:
C  N      (input)  : number of points defining the polygon; the
C                    line consists of N straight-line segments,
C                    joining points 1 to 2, 2 to 3,... N-1 to N, N to 1.
C                    N should be greater than 2 (if it is 2 or less,
C                    nothing will be drawn).
C  XPTS   (input)  : world x-coordinates of the vertices.
C  YPTS   (input)  : world y-coordinates of the vertices.
C                    Note: the dimension of arrays XPTS and YPTS must be
C                    greater than or equal to N.
C--
C 21-Nov-1983 - [TJP].
C 16-Jul-1984 - revised to shade polygon with GRFA [TJP].
C 21-Oct-1985 - test PGFAS [TJP].
C 25-Nov-1994 - implement clipping [TJP].
C 13-Jan-1994 - fix bug in clipping [TJP].
C  6-Mar-1995 - add support for fill styles 3 and 4 [TJP].
C 12-Sep-1995 - fix another bug in clipping [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXOUT
      PARAMETER (MAXOUT=1000)
      LOGICAL CLIP
      INTEGER I, N1, N2, N3, N4
      REAL    QX(MAXOUT), QY(MAXOUT), RX(MAXOUT), RY(MAXOUT)
      REAL    XL, XH, YL, YH
      LOGICAL PGNOTO
      INCLUDE 'pgplot.inc'
C
      IF (PGNOTO('PGPOLY')) RETURN
      IF (N.LT.1) RETURN
C
C Outline style, or polygon of less than 3 vertices.
C
      IF (PGFAS(PGID).EQ.2 .OR. N.LT.3) THEN
         CALL PGBBUF
         CALL GRMOVA(XPTS(N),YPTS(N))
         DO 10 I=1,N
            CALL GRLINA(XPTS(I),YPTS(I))
 10      CONTINUE
C
C Hatched style.
C
      ELSE IF (PGFAS(PGID).EQ.3) THEN
         CALL PGBBUF
         CALL PGHTCH(N, XPTS, YPTS, 0.0)
      ELSE IF (PGFAS(PGID).EQ.4) THEN
         CALL PGBBUF
         CALL PGHTCH(N, XPTS, YPTS, 0.0)
         CALL PGHTCH(N, XPTS, YPTS, 90.0)
      ELSE
C     
C Test whether polygon lies completely in the window.
C     
         CLIP = .FALSE.
         XL = MIN(PGXBLC(PGID),PGXTRC(PGID))
         XH = MAX(PGXBLC(PGID),PGXTRC(PGID))
         YL = MIN(PGYBLC(PGID),PGYTRC(PGID))
         YH = MAX(PGYBLC(PGID),PGYTRC(PGID))
         DO 20 I=1,N
            IF (XPTS(I).LT.XL .OR. XPTS(I).GT.XH .OR.
     :           YPTS(I).LT.YL .OR. YPTS(I).GT.YH) THEN
               CLIP = .TRUE.
               GOTO 30
            END IF
 20      CONTINUE
 30      CONTINUE
C     
C Filled style, no clipping required.
C     
         CALL PGBBUF
         IF (.NOT.CLIP) THEN
            CALL GRFA(N,XPTS,YPTS)
C     
C Filled style, clipping required: the vertices of the clipped
C polygon are put in temporary arrays QX,QY, RX, RY.
C     
         ELSE
            CALL GRPOCL(N,  XPTS, YPTS, 1, XL, MAXOUT, N1, QX, QY)
            IF (N1.GT.MAXOUT) GOTO 40
            IF (N1.LT.3) GOTO 50
            CALL GRPOCL(N1, QX,   QY,   2, XH, MAXOUT, N2, RX, RY)
            IF (N2.GT.MAXOUT) GOTO 40
            IF (N2.LT.3) GOTO 50
            CALL GRPOCL(N2, RX,   RY,   3, YL, MAXOUT, N3, QX, QY)
            IF (N3.GT.MAXOUT) GOTO 40
            IF (N3.LT.3) GOTO 50
            CALL GRPOCL(N3, QX,   QY,   4, YH, MAXOUT, N4, RX, RY)
            IF (N4.GT.MAXOUT) GOTO 40
            IF (N4.GT.0) CALL GRFA(N4,RX,RY)
            GOTO 50
 40         CALL GRWARN('PGPOLY: polygon is too complex')
 50         CONTINUE
         END IF
      END IF
C
C Set the current pen position.
C
      CALL GRMOVA(XPTS(1),YPTS(1))
      CALL PGEBUF
C
      END
C*PGPT1 -- draw one graph marker
C%void cpgpt1(float xpt, float ypt, int symbol);
C+
      SUBROUTINE PGPT1 (XPT, YPT, SYMBOL)
      REAL XPT, YPT
      INTEGER SYMBOL
C
C Primitive routine to draw a single Graph Marker at a specified point.
C The marker is drawn using the current values of attributes
C color-index, line-width, and character-height (character-font applies
C if the symbol number is >31).  If the point to be marked lies outside
C the window, no marker is drawn.  The "pen position" is changed to
C (XPT,YPT) in world coordinates.
C
C To draw several markers with coordinates specified by X and Y
C arrays, use routine PGPT.
C
C Arguments:
C  XPT    (input)  : world x-coordinate of the point.
C  YPT    (input)  : world y-coordinate of the point.
C  SYMBOL (input)  : code number of the symbol to be drawn:
C                    -1, -2  : a single dot (diameter = current
C                              line width).
C                    -3..-31 : a regular polygon with ABS(SYMBOL)
C                              edges (style set by current fill style).
C                    0..31   : standard marker symbols.
C                    32..127 : ASCII characters (in current font).
C                              e.g. to use letter F as a marker, let
C                              SYMBOL = ICHAR('F'). 
C                    > 127  :  a Hershey symbol number.
C--
C  4-Feb-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
      REAL XPTS(1), YPTS(1)
C
      IF (PGNOTO('PGPT1')) RETURN
      XPTS(1) = XPT
      YPTS(1) = YPT
      CALL PGPT(1, XPTS, YPTS, SYMBOL)
      END
C*PGPT -- draw several graph markers
C%void cpgpt(int n, const float *xpts, const float *ypts, int symbol);
C+
      SUBROUTINE PGPT (N, XPTS, YPTS, SYMBOL)
      INTEGER N
      REAL XPTS(*), YPTS(*)
      INTEGER SYMBOL
C
C Primitive routine to draw Graph Markers (polymarker). The markers
C are drawn using the current values of attributes color-index,
C line-width, and character-height (character-font applies if the symbol
C number is >31).  If the point to be marked lies outside the window,
C no marker is drawn.  The "pen position" is changed to
C (XPTS(N),YPTS(N)) in world coordinates (if N > 0).
C
C Arguments:
C  N      (input)  : number of points to mark.
C  XPTS   (input)  : world x-coordinates of the points.
C  YPTS   (input)  : world y-coordinates of the points.
C  SYMBOL (input)  : code number of the symbol to be drawn at each 
C                    point:
C                    -1, -2  : a single dot (diameter = current
C                              line width).
C                    -3..-31 : a regular polygon with ABS(SYMBOL)
C                              edges (style set by current fill style).
C                    0..31   : standard marker symbols.
C                    32..127 : ASCII characters (in current font).
C                              e.g. to use letter F as a marker, let
C                              SYMBOL = ICHAR('F'). 
C                    > 127  :  a Hershey symbol number.
C
C Note: the dimension of arrays X and Y must be greater than or equal
C to N. If N is 1, X and Y may be scalars (constants or variables). If
C N is less than 1, nothing is drawn.
C--
C 27-Nov-1986
C 17-Dec-1990 - add polygons [PAH].
C 14-Mar-1997 - optimization: use GRDOT1 [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (N.LT.1) RETURN
      IF (PGNOTO('PGPT')) RETURN
C
      CALL PGBBUF
      IF (SYMBOL.GE.0 .OR. SYMBOL.LE.-3) THEN
          CALL GRMKER(SYMBOL,.FALSE.,N,XPTS,YPTS)
      ELSE
          CALL GRDOT1(N,XPTS,YPTS)
      END IF
      CALL PGEBUF
      END
C*PGPTEXT -- non-standard alias for PGPTXT
C+
      SUBROUTINE PGPTEXT (X, Y, ANGLE, FJUST, TEXT)
      REAL X, Y, ANGLE, FJUST
      CHARACTER*(*) TEXT
C
C See description of PGPTXT.
C--
      CALL PGPTXT (X, Y, ANGLE, FJUST, TEXT)
      END
C*PGPTXT -- write text at arbitrary position and angle
C%void cpgptxt(float x, float y, float angle, float fjust, \
C% const char *text);
C+
      SUBROUTINE PGPTXT (X, Y, ANGLE, FJUST, TEXT)
      REAL X, Y, ANGLE, FJUST
      CHARACTER*(*) TEXT
C
C Primitive routine for drawing text. The text may be drawn at any
C angle with the horizontal, and may be centered or left- or right-
C justified at a specified position.  Routine PGTEXT provides a
C simple interface to PGPTXT for horizontal strings. Text is drawn
C using the current values of attributes color-index, line-width,
C character-height, and character-font.  Text is NOT subject to
C clipping at the edge of the window.
C
C Arguments:
C  X      (input)  : world x-coordinate.
C  Y      (input)  : world y-coordinate. The string is drawn with the
C                    baseline of all the characters passing through
C                    point (X,Y); the positioning of the string along
C                    this line is controlled by argument FJUST.
C  ANGLE  (input)  : angle, in degrees, that the baseline is to make
C                    with the horizontal, increasing counter-clockwise
C                    (0.0 is horizontal).
C  FJUST  (input)  : controls horizontal justification of the string.
C                    If FJUST = 0.0, the string will be left-justified
C                    at the point (X,Y); if FJUST = 0.5, it will be
C                    centered, and if FJUST = 1.0, it will be right
C                    justified. [Other values of FJUST give other
C                    justifications.]
C  TEXT   (input)  : the character string to be plotted.
C--
C (2-May-1983)
C 31-Jan-1985 - convert to Fortran-77 standard...
C 13-Feb-1988 - correct a PGBBUF/PGEBUF mismatch if string is blank.
C 16-Oct-1993 - erase background of opaque text.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER CI, I, L, GRTRIM
      REAL D, XP, YP
      REAL XBOX(4), YBOX(4)
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGPTXT')) RETURN
      CALL PGBBUF
C
      L = GRTRIM(TEXT)
      D = 0.0
      IF (FJUST.NE.0.0) CALL GRLEN(TEXT(1:L),D)
      XP = PGXORG(PGID)+X*PGXSCL(PGID) - D*FJUST*COS(ANGLE/57.29578)
      YP = PGYORG(PGID)+Y*PGYSCL(PGID) - D*FJUST*SIN(ANGLE/57.29578)
      IF (PGTBCI(PGID).GE.0) THEN
          CALL GRQTXT (ANGLE, XP, YP, TEXT(1:L), XBOX, YBOX)
          DO 25 I=1,4
              XBOX(I) = (XBOX(I)-PGXORG(PGID))/PGXSCL(PGID)
              YBOX(I) = (YBOX(I)-PGYORG(PGID))/PGYSCL(PGID)
   25     CONTINUE
          CALL PGQCI(CI)
          CALL PGSCI(PGTBCI(PGID))
          CALL GRFA(4, XBOX, YBOX)
          CALL PGSCI(CI)
      END IF
      CALL GRTEXT(.TRUE. ,ANGLE, .TRUE., XP, YP, TEXT(1:L))
   30 CALL PGEBUF
      END
C*PGQAH -- inquire arrow-head style
C%void cpgqah(int *fs, float *angle, float *barb);
C+
      SUBROUTINE PGQAH (FS, ANGLE, BARB)
      INTEGER  FS
      REAL ANGLE, BARB
C
C Query the style to be used for arrowheads drawn with routine PGARRO.
C
C Argument:
C  FS     (output) : FS = 1 => filled; FS = 2 => outline.
C  ANGLE  (output) : the acute angle of the arrow point, in degrees.
C  BARB   (output) : the fraction of the triangular arrow-head that
C                    is cut away from the back. 
C--
C 13-Oct-1992 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      FS = PGAHS(PGID)
      ANGLE = PGAHA(PGID)
      BARB = PGAHV(PGID)
C
      END
C*PGQCF -- inquire character font
C%void cpgqcf(int *font);
C+
      SUBROUTINE PGQCF (FONT)
      INTEGER  FONT
C
C Query the current Character Font (set by routine PGSCF).
C
C Argument:
C  FONT   (output)   : the current font number (in range 1-4).
C--
C  5-Nov-1985 - new routine [TJP].
C 25-OCT-1993 - changed name of argument [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQCF')) THEN
         FONT = 1
      ELSE
         CALL GRQFNT(FONT)
      END IF
      END
C*PGQCH -- inquire character height
C%void cpgqch(float *size);
C+
      SUBROUTINE PGQCH (SIZE)
      REAL SIZE
C
C Query the Character Size attribute (set by routine PGSCH).
C
C Argument:
C  SIZE   (output) : current character size (dimensionless multiple of
C                    the default size).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
C
      IF (PGNOTO('PGQCH')) THEN
          SIZE = 1.0
      ELSE
          SIZE = PGCHSZ(PGID)
      END IF
      END
C*PGQCI -- inquire color index
C%void cpgqci(int *ci);
C+
      SUBROUTINE PGQCI (CI)
      INTEGER  CI
C
C Query the Color Index attribute (set by routine PGSCI).
C
C Argument:
C  CI     (output) : the current color index (in range 0-max). This is
C                    the color index actually in use, and may differ
C                    from the color index last requested by PGSCI if
C                    that index is not available on the output device.
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQCI')) THEN
         CI = 1
      ELSE
         CALL GRQCI(CI)
      END IF
      END
C*PGQCIR -- inquire color index range
C%void cpgqcir(int *icilo, int *icihi);
C+
      SUBROUTINE PGQCIR(ICILO, ICIHI)
      INTEGER   ICILO, ICIHI
C
C Query the color index range to be used for producing images with
C PGGRAY or PGIMAG, as set by routine PGSCIR or by device default.
C
C Arguments:
C  ICILO  (output) : the lowest color index to use for images
C  ICIHI  (output) : the highest color index to use for images
C--
C 1994-Mar-17 : new routine [AFT/TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C---
      ICILO = PGMNCI(PGID)
      ICIHI = PGMXCI(PGID)
C
      END
C*PGQCLP -- inquire clipping status
C%void cpgqclp(int *state);
C+
      SUBROUTINE PGQCLP(STATE)
      INTEGER  STATE
C
C Query the current clipping status (set by routine PGSCLP).
C
C Argument:
C  STATE  (output) : receives the clipping status (0 => disabled,
C                    1 => enabled).
C--
C 25-Feb-1997 [TJP] - new routine.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQCLP')) THEN
         STATE = 1
      ELSE
         STATE = PGCLP(PGID)
      END IF
      END
C*PGQCOL -- inquire color capability
C%void cpgqcol(int *ci1, int *ci2);
C+
      SUBROUTINE PGQCOL (CI1, CI2)
      INTEGER  CI1, CI2
C
C Query the range of color indices available on the current device.
C
C Argument:
C  CI1    (output) : the minimum available color index. This will be
C                    either 0 if the device can write in the
C                    background color, or 1 if not.
C  CI2    (output) : the maximum available color index. This will be
C                    1 if the device has no color capability, or a
C                    larger number (e.g., 3, 7, 15, 255).
C--
C 31-May-1989 - new routine [TJP].
C-----------------------------------------------------------------------
      CALL GRQCOL(CI1, CI2)
      END
C*PGQCR  -- inquire color representation
C%void cpgqcr(int ci, float *cr, float *cg, float *cb);
C+
      SUBROUTINE PGQCR (CI, CR, CG, CB)
      INTEGER CI
      REAL    CR, CG, CB
C
C Query the RGB colors associated with a color index.
C
C Arguments:
C  CI  (input)  : color index
C  CR  (output) : red, green and blue intensities
C  CG  (output)   in the range 0.0 to 1.0
C  CB  (output)
C--
C 7-Apr-1992 - new routine [DLT]
C-----------------------------------------------------------------------
      CALL GRQCR(CI, CR, CG, CB)
      END
C*PGQCS  -- inquire character height in a variety of units
C%void cpgqcs(int units, float *xch, float *ych);
C+
      SUBROUTINE PGQCS(UNITS, XCH, YCH)
      INTEGER UNITS
      REAL XCH, YCH
C
C Return the current PGPLOT character height in a variety of units.
C This routine provides facilities that are not available via PGQCH.
C Use PGQCS if the character height is required in units other than
C those used in PGSCH.
C
C The PGPLOT "character height" is a dimension that scales with the
C size of the view surface and with the scale-factor specified with
C routine PGSCH. The default value is 1/40th of the height or width
C of the view surface (whichever is less); this value is then
C multiplied by the scale-factor supplied with PGSCH. Note that it
C is a nominal height only; the actual character size depends on the
C font and is usually somewhat smaller.
C
C Arguments:
C  UNITS  (input)  : Used to specify the units of the output value:
C                    UNITS = 0 : normalized device coordinates
C                    UNITS = 1 : inches
C                    UNITS = 2 : millimeters
C                    UNITS = 3 : pixels
C                    UNITS = 4 : world coordinates
C                    Other values give an error message, and are
C                    treated as 0.
C  XCH    (output) : The character height for text written with a
C                    vertical baseline.
C  YCH    (output) : The character height for text written with
C                    a horizontal baseline (the usual case).
C
C The character height is returned in both XCH and YCH.
C
C If UNITS=1 or UNITS=2, XCH and YCH both receive the same value.
C
C If UNITS=3, XCH receives the height in horizontal pixel units, and YCH
C receives the height in vertical pixel units; on devices for which the
C pixels are not square, XCH and YCH will be different.
C
C If UNITS=4, XCH receives the height in horizontal world coordinates
C (as used for the x-axis), and YCH receives the height in vertical
C world coordinates (as used for the y-axis). Unless special care has
C been taken to achive equal world-coordinate scales on both axes, the
C values of XCH and YCH will be different.
C
C If UNITS=0, XCH receives the character height as a fraction of the
C horizontal dimension of the view surface, and YCH receives the
C character height as a fraction of the vertical dimension of the view
C surface.
C--
C 15-Oct-1992 - new routine [MCS].
C  4-Dec-1992 - added more explanation [TJP].
C  5-Sep-1995 - add UNITS=4; correct error for non-square pixels [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL RATIO
C                                        Conversion factor inches -> mm
      REAL INTOMM
      PARAMETER (INTOMM=25.4)
C-----------------------------------------------------------------------
      IF (PGNOTO('PGQCS')) RETURN
      RATIO = PGYPIN(PGID)/PGXPIN(PGID)
C
C Return the character height in the required units.
C
C                                        Inches.
      IF (UNITS.EQ.1) THEN
        XCH = PGYSP(PGID)/PGXPIN(PGID)
        YCH = XCH
C                                        Millimeters.
      ELSE IF (UNITS.EQ.2) THEN
        XCH = PGYSP(PGID)/PGXPIN(PGID) * INTOMM
        YCH = XCH
C                                        Pixels.
      ELSE IF (UNITS.EQ.3) THEN
        XCH = PGYSP(PGID)
        YCH = PGYSP(PGID)*RATIO
C                                        World coordinates.
      ELSE IF (UNITS.EQ.4) THEN
         XCH = PGYSP(PGID)/PGXSCL(PGID)
         YCH = PGYSP(PGID)*RATIO/PGYSCL(PGID)
C                                        Normalized device coords, or
C                                        unknown.
      ELSE
        XCH = PGYSP(PGID)/PGXSZ(PGID)
        YCH = PGYSP(PGID)*RATIO/PGYSZ(PGID)
        IF (UNITS.NE.0)
     :       CALL GRWARN('Invalid "UNITS" argument in PGQCS.')
      END IF
      END
C*PGQDT -- inquire name of nth available device type
C%void cpgqdt(int n, char *type, int *type_length, char *descr, \
C% int *descr_length, int *inter);
C+
      SUBROUTINE PGQDT(N, TYPE, TLEN, DESCR, DLEN, INTER)
      INTEGER N
      CHARACTER*(*) TYPE, DESCR
      INTEGER TLEN, DLEN, INTER
C
C Return the name of the Nth available device type as a character
C string. The number of available types can be determined by calling
C PGQNDT. If the value of N supplied is outside the range from 1 to
C the number of available types, the routine returns DLEN=TLEN=0.
C
C Arguments:
C  N      (input)  : the number of the device type (1..maximum).
C  TYPE   (output) : receives the character device-type code of the
C                    Nth device type. The argument supplied should be
C                    large enough for at least 8 characters. The first
C                    character in the string is a '/' character.
C  TLEN   (output) : receives the number of characters in TYPE,
C                    excluding trailing blanks.
C  DESCR  (output) : receives a description of the device type. The
C                    argument supplied should be large enough for at
C                    least 64 characters.
C  DLEN   (output) : receives the number of characters in DESCR,
C                    excluding trailing blanks.
C  INTER  (output) : receives 1 if the device type is an interactive
C                    one, 0 otherwise.
C--
C 17-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      INTEGER NDEV, NBUF, LCHR, L1, L2
      REAL RBUF(2)
      CHARACTER*80 CHR
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
      TYPE = 'error'
      TLEN = 0
      DESCR = ' '
      DLEN = 0
      INTER = 1
      CALL PGQNDT(NDEV)
      IF (N.GE.1 .AND. N.LE.NDEV) THEN
         NBUF = 0
         CALL GREXEC(N, 1, RBUF, NBUF, CHR, LCHR)
         IF (LCHR.GT.0) THEN
            L1 = INDEX(CHR(1:LCHR), ' ')
            IF (L1.GT.1) THEN
               TYPE(1:1) = '/'
               IF (LEN(TYPE).GT.1) TYPE(2:) = CHR(1:L1-1)
               TLEN = MIN(L1,LEN(TYPE))
            END IF
            L2 = INDEX(CHR(1:LCHR), '(')
            IF (L2.GT.0) DESCR = CHR(L2:LCHR)
            DLEN = MIN(LCHR-L2+1,LEN(DESCR))
            CALL GREXEC(N, 4, RBUF, NBUF, CHR, LCHR)
            IF (CHR(1:1).EQ.'H') INTER = 0
         END IF
      END IF
C
      END
C*PGQFS -- inquire fill-area style
C%void cpgqfs(int *fs);
C+
      SUBROUTINE PGQFS (FS)
      INTEGER  FS
C
C Query the current Fill-Area Style attribute (set by routine
C PGSFS).
C
C Argument:
C  FS     (output) : the current fill-area style:
C                      FS = 1 => solid (default)
C                      FS = 2 => outline
C                      FS = 3 => hatched
C                      FS = 4 => cross-hatched
C--
C  5-Nov-1985 - new routine [TJP].
C  6-Mar-1995 - add styles 3 and 4 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQFS')) THEN
          FS = 1
      ELSE
          FS = PGFAS(PGID)
      END IF
      END
C*PGQHS -- inquire hatching style
C%void cpgqhs(float *angle, float *sepn, float* phase);
C+
      SUBROUTINE PGQHS (ANGLE, SEPN, PHASE)
      REAL ANGLE, SEPN, PHASE
C
C Query the style to be used hatching (fill area with fill-style 3).
C
C Arguments:
C  ANGLE  (output) : the angle the hatch lines make with the
C                    horizontal, in degrees, increasing 
C                    counterclockwise (this is an angle on the
C                    view surface, not in world-coordinate space).
C  SEPN   (output) : the spacing of the hatch lines. The unit spacing
C                    is 1 percent of the smaller of the height or
C                    width of the view surface.
C  PHASE  (output) : a real number between 0 and 1; the hatch lines
C                    are displaced by this fraction of SEPN from a
C                    fixed reference.  Adjacent regions hatched with the
C                    same PHASE have contiguous hatch lines.
C--
C 26-Feb-1995 - new routine [TJP].
C 19-Jun-1995 - correct synopsis [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      ANGLE = PGHSA(PGID)
      SEPN  = PGHSS(PGID)
      PHASE = PGHSP(PGID)
C
      END
C*PGQID -- inquire current device identifier
C%void cpgqid(int *id);
C+
      SUBROUTINE PGQID (ID)
      INTEGER  ID
C
C This subroutine returns the identifier of the currently
C selected device, or 0 if no device is selected.  The identifier is
C assigned when PGOPEN is called to open the device, and may be used
C as an argument to PGSLCT.  Each open device has a different
C identifier.
C
C [This routine was added to PGPLOT in Version 5.1.0.]
C
C Argument:
C  ID     (output) : the identifier of the current device, or 0 if
C                    no device is currently selected.
C--
C 22-Dec-1995 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      ID = PGID
      END
C*PGQINF -- inquire PGPLOT general information
C%void cpgqinf(const char *item, char *value, int *value_length);
C+
      SUBROUTINE PGQINF (ITEM, VALUE, LENGTH)
      CHARACTER*(*) ITEM, VALUE
      INTEGER LENGTH
C
C This routine can be used to obtain miscellaneous information about
C the PGPLOT environment. Input is a character string defining the
C information required, and output is a character string containing the
C requested information.
C
C The following item codes are accepted (note that the strings must
C match exactly, except for case, but only the first 8 characters are
C significant). For items marked *, PGPLOT must be in the OPEN state
C for the inquiry to succeed. If the inquiry is unsuccessful, either
C because the item code is not recognized or because the information
C is not available, a question mark ('?') is returned.
C
C   'VERSION'     - version of PGPLOT software in use.
C   'STATE'       - status of PGPLOT ('OPEN' if a graphics device
C                   is open for output, 'CLOSED' otherwise).
C   'USER'        - the username associated with the calling program.
C   'NOW'         - current date and time (e.g., '17-FEB-1986 10:04').
C   'DEVICE'    * - current PGPLOT device or file.
C   'FILE'      * - current PGPLOT device or file.
C   'TYPE'      * - device-type of the current PGPLOT device.
C   'DEV/TYPE'  * - current PGPLOT device and type, in a form which
C                   is acceptable as an argument for PGBEG.
C   'HARDCOPY'  * - is the current device a hardcopy device? ('YES' or
C                   'NO').
C   'TERMINAL'  * - is the current device the user's interactive
C                   terminal? ('YES' or 'NO').
C   'CURSOR'    * - does the current device have a graphics cursor?
C                   ('YES' or 'NO').
C   'SCROLL'    * - does current device have rectangle-scroll
C                   capability ('YES' or 'NO'); see PGSCRL.
C
C Arguments:
C  ITEM  (input)  : character string defining the information to
C                   be returned; see above for a list of possible
C                   values.
C  VALUE (output) : returns a character-string containing the
C                   requested information, truncated to the length 
C                   of the supplied string or padded on the right with 
C                   spaces if necessary.
C  LENGTH (output): the number of characters returned in VALUE
C                   (excluding trailing blanks).
C--
C 18-Feb-1988 - [TJP].
C 30-Aug-1988 - remove pseudo logical use of IER.
C 12-Mar-1992 - change comments for clarity.
C 17-Apr-1995 - clean up some zero-length string problems [TJP].
C  7-Jul-1995 - get cursor information directly from driver [TJP].
C 24-Feb-1997 - add SCROLL request.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER IER, L1, GRTRIM
      LOGICAL INTER, SAME
      CHARACTER*8 TEST
      CHARACTER*64 DEV1
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
      CALL GRTOUP(TEST,ITEM)
      IF (TEST.EQ.'USER') THEN
          CALL GRUSER(VALUE, LENGTH)
          IER = 1
      ELSE IF (TEST.EQ.'NOW') THEN
          CALL GRDATE(VALUE, LENGTH)
          IER = 1
      ELSE IF (TEST.EQ.'VERSION') THEN
          VALUE = 'v5.2.0'
          LENGTH = 6
          IER = 1
      ELSE IF (TEST.EQ.'STATE') THEN
          IF (PGID.LT.1 .OR. PGID.GT.PGMAXD) THEN
             VALUE = 'CLOSED'
             LENGTH = 6
          ELSE IF (PGDEVS(PGID).EQ.0) THEN
             VALUE = 'CLOSED'
             LENGTH = 6
          ELSE
             VALUE = 'OPEN'
             LENGTH = 4
          END IF
          IER = 1
      ELSE IF (PGID.LT.1 .OR. PGID.GT.PGMAXD) THEN
          IER = 0
      ELSE IF (PGDEVS(PGID).EQ.0) THEN
          IER = 0
      ELSE IF (TEST.EQ.'DEV/TYPE') THEN
          CALL GRQDT(VALUE)
          LENGTH = GRTRIM(VALUE)
          IER = 0
          IF (LENGTH.GT.0) IER = 1
      ELSE IF (TEST.EQ.'DEVICE' .OR. TEST.EQ.'FILE') THEN
          CALL GRQDEV(VALUE, LENGTH)
          IER = 1
      ELSE IF (TEST.EQ.'TERMINAL') THEN
          CALL GRQDEV(DEV1, L1)
          IF (L1.GE.1) THEN
             CALL GRTTER(DEV1(1:L1), SAME)
          ELSE
             SAME = .FALSE.
          END IF
          IF (SAME) THEN
              VALUE = 'YES'
              LENGTH = 3
          ELSE
              VALUE = 'NO'
              LENGTH = 2
          END IF
          IER = 1
      ELSE IF (TEST.EQ.'TYPE') THEN
          CALL GRQTYP(VALUE,INTER)
          LENGTH = GRTRIM(VALUE)
          IER = 0
          IF (LENGTH.GT.0) IER = 1
      ELSE IF (TEST.EQ.'HARDCOPY') THEN
          CALL GRQTYP(VALUE,INTER)
          IF (INTER) THEN
              VALUE = 'NO'
              LENGTH = 2
          ELSE
              VALUE = 'YES'
              LENGTH = 3
          END IF
          IER = 1
      ELSE IF (TEST.EQ.'CURSOR') THEN
          CALL GRQCAP(DEV1)
          IF (DEV1(2:2).EQ.'N') THEN
              VALUE = 'NO'
              LENGTH = 2
          ELSE
              VALUE = 'YES'
              LENGTH = 3
          END IF
          IER = 1
      ELSE IF (TEST.EQ.'SCROLL') THEN
          CALL GRQCAP(DEV1)
          IF (DEV1(11:11).NE.'S') THEN
              VALUE = 'NO'
              LENGTH = 2
          ELSE
              VALUE = 'YES'
              LENGTH = 3
          END IF
          IER = 1
      ELSE
          IER = 0
      END IF
      IF (IER.NE.1) THEN
         VALUE = '?'
         LENGTH = 1
      ELSE IF (LENGTH.LT.1) THEN
         LENGTH = 1
         VALUE = ' '
      END IF
      END
C*PGQITF -- inquire image transfer function
C%void cpgqitf(int *itf);
C+
      SUBROUTINE PGQITF (ITF)
      INTEGER  ITF
C
C Return the Image Transfer Function as set by default or by a previous
C call to PGSITF. The Image Transfer Function is used by routines
C PGIMAG, PGGRAY, and PGWEDG.
C
C Argument:
C  ITF    (output) : type of transfer function (see PGSITF)
C--
C 15-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQITF')) THEN
          ITF = 0
      ELSE
          ITF = PGITF(PGID)
      END IF
      END
C*PGQLS -- inquire line style
C%void cpgqls(int *ls);
C+
      SUBROUTINE PGQLS (LS)
      INTEGER  LS
C
C Query the current Line Style attribute (set by routine PGSLS).
C
C Argument:
C  LS     (output) : the current line-style attribute (in range 1-5).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQLS')) THEN
         LS = 1
      ELSE
         CALL GRQLS(LS)
      END IF
      END
C*PGQLW -- inquire line width
C%void cpgqlw(int *lw);
C+
      SUBROUTINE PGQLW (LW)
      INTEGER  LW
C
C Query the current Line-Width attribute (set by routine PGSLW).
C
C Argument:
C  LW     (output)  : the line-width (in range 1-201).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C     
      IF (PGNOTO('PGQLW')) THEN
         LW = 1
      ELSE
         CALL GRQLW(LW)
      END IF
      END
C*PGQNDT -- inquire number of available device types
C%void cpgqndt(int *n);
C+
      SUBROUTINE PGQNDT(N)
      INTEGER N
C
C Return the number of available device types. This routine is
C usually used in conjunction with PGQDT to get a list of the
C available device types.
C
C Arguments:
C  N      (output) : the number of available device types.
C--
C 17-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      INTEGER NBUF, LCHR
      REAL RBUF(2)
      CHARACTER CHR
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Find number of device types.
C
      CALL GREXEC(0, 0, RBUF, NBUF, CHR, LCHR)
      N = NINT(RBUF(1))
C
      END
C*PGQPOS -- inquire current pen position
C%void cpgqpos(float *x, float *y);
C+
      SUBROUTINE PGQPOS (X, Y)
      REAL X, Y
C
C Query the current "pen" position in world C coordinates (X,Y).
C
C Arguments:
C  X      (output)  : world x-coordinate of the pen position.
C  Y      (output)  : world y-coordinate of the pen position.
C--
C  1-Mar-1991 - new routine [JM].
C-----------------------------------------------------------------------
      CALL GRQPOS(X,Y)
      END
C*PGQTBG -- inquire text background color index
C%void cpgqtbg(int *tbci);
C+
      SUBROUTINE PGQTBG (TBCI)
      INTEGER  TBCI
C
C Query the current Text Background Color Index (set by routine
C PGSTBG).
C
C Argument:
C  TBCI   (output) : receives the current text background color index.
C--
C 16-Oct-1993 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQTBG')) THEN
          TBCI = 0
      ELSE
          TBCI = PGTBCI(PGID)
      END IF
      END
C*PGQTXT -- find bounding box of text string
C%void cpgqtxt(float x, float y, float angle, float fjust, \
C% const char *text, float *xbox, float *ybox);
C+
      SUBROUTINE PGQTXT (X, Y, ANGLE, FJUST, TEXT, XBOX, YBOX)
      REAL X, Y, ANGLE, FJUST
      CHARACTER*(*) TEXT
      REAL XBOX(4), YBOX(4)
C
C This routine returns a bounding box for a text string. Instead
C of drawing the string as routine PGPTXT does, it returns in XBOX
C and YBOX the coordinates of the corners of a rectangle parallel
C to the string baseline that just encloses the string. The four
C corners are in the order: lower left, upper left, upper right,
C lower right (where left and right refer to the first and last
C characters in the string).
C
C If the string is blank or contains no drawable characters, all
C four elements of XBOX and YBOX are assigned the starting point
C of the string, (X,Y).
C
C Arguments:
C  X, Y, ANGLE, FJUST, TEXT (input) : these arguments are the same as
C                    the corrresponding arguments in PGPTXT.
C  XBOX, YBOX (output) : arrays of dimension 4; on output, they
C                    contain the world coordinates of the bounding
C                    box in (XBOX(1), YBOX(1)), ..., (XBOX(4), YBOX(4)).
C--
C 12-Sep-1993 - new routine [TJP].
C  8-Nov-1994 - return something for blank string [TJP].
C 14-Jan-1997 - additional explanation [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      INTEGER I, L, GRTRIM
      REAL D, XP, YP, XPBOX(4), YPBOX(4), XOFFS, YOFFS
C
      IF (PGNOTO('PGQTXT')) RETURN
C
      L = GRTRIM(TEXT)
      IF (L.LE.0) THEN
         DO 15 I=1,4
            XBOX(I) = X
            YBOX(I) = Y
 15      CONTINUE
      ELSE
         D = 0.0
         IF (FJUST.NE.0.0) CALL GRLEN(TEXT(1:L),D)
         XOFFS = PGXORG(PGID) - D*FJUST*COS(ANGLE/57.29578)
         YOFFS = PGYORG(PGID) - D*FJUST*SIN(ANGLE/57.29578)
         XP = X*PGXSCL(PGID) + XOFFS
         YP = Y*PGYSCL(PGID) + YOFFS
         CALL GRQTXT(ANGLE, XP, YP, TEXT(1:L), XPBOX, YPBOX)
         DO 25 I=1,4
            XBOX(I) = (XPBOX(I) - PGXORG(PGID))/PGXSCL(PGID)
            YBOX(I) = (YPBOX(I) - PGYORG(PGID))/PGYSCL(PGID)
 25      CONTINUE
      END IF
      END
C*PGQVP -- inquire viewport size and position
C%void cpgqvp(int units, float *x1, float *x2, float *y1, float *y2);
C+
      SUBROUTINE PGQVP (UNITS, X1, X2, Y1, Y2)
      INTEGER UNITS
      REAL    X1, X2, Y1, Y2
C
C Inquiry routine to determine the current viewport setting.
C The values returned may be normalized device coordinates, inches, mm,
C or pixels, depending on the value of the input parameter CFLAG.
C
C Arguments:
C  UNITS  (input)  : used to specify the units of the output parameters:
C                    UNITS = 0 : normalized device coordinates
C                    UNITS = 1 : inches
C                    UNITS = 2 : millimeters
C                    UNITS = 3 : pixels
C                    Other values give an error message, and are
C                    treated as 0.
C  X1     (output) : the x-coordinate of the bottom left corner of the
C                    viewport.
C  X2     (output) : the x-coordinate of the top right corner of the
C                    viewport.
C  Y1     (output) : the y-coordinate of the bottom left corner of the
C                    viewport.
C  Y2     (output) : the y-coordinate of the top right corner of the
C                    viewport.
C--
C 26-Sep-1985 - new routine (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      REAL SX, SY
C
      IF (UNITS.EQ.0) THEN
          SX = PGXSZ(PGID)
          SY = PGYSZ(PGID)
      ELSE IF (UNITS.EQ.1) THEN
          SX = PGXPIN(PGID)
          SY = PGYPIN(PGID)
      ELSE IF (UNITS.EQ.2) THEN
          SX = (PGXPIN(PGID)/25.4)
          SY = (PGYPIN(PGID)/25.4)
      ELSE IF (UNITS.EQ.3) THEN
          SX = 1.0
          SY = 1.0
      ELSE
          CALL GRWARN(
     1        'Illegal value for parameter UNITS in routine PGQVP')
          SX = PGXSZ(PGID)
          SY = PGYSZ(PGID)
      END IF
      X1 = PGXVP(PGID)/SX
      X2 = (PGXVP(PGID)+PGXLEN(PGID))/SX
      Y1 = PGYVP(PGID)/SY
      Y2 = (PGYVP(PGID)+PGYLEN(PGID))/SY
      END
C*PGQVSZ -- inquire size of view surface
C%void cpgqvsz(int units, float *x1, float *x2, float *y1, float *y2);
C+
      SUBROUTINE PGQVSZ (UNITS, X1, X2, Y1, Y2)
      INTEGER UNITS
      REAL X1, X2, Y1, Y2
C
C This routine returns the dimensions of the view surface (the maximum
C plottable area) of the currently selected graphics device, in 
C a variety of units. The size of the view surface is device-dependent
C and is established when the graphics device is opened. On some 
C devices, it can be changed by calling PGPAP before starting a new
C page with PGPAGE. On some devices, the size can be changed (e.g.,
C by a workstation window manager) outside PGPLOT, and PGPLOT detects
C the change when PGPAGE is used. Call this routine after PGPAGE to 
C find the current size.
C
C Note 1: the width and the height of the view surface in normalized
C device coordinates are both always equal to 1.0.
C
C Note 2: when the device is divided into panels (see PGSUBP), the
C view surface is a single panel.
C
C Arguments:
C  UNITS  (input)  : 0,1,2,3 for output in normalized device coords, 
C                    inches, mm, or device units (pixels)
C  X1     (output) : always returns 0.0
C  X2     (output) : width of view surface
C  Y1     (output) : always returns 0.0
C  Y2     (output) : height of view surface
C--
C 28-Aug-1992 - new routine [Neil Killeen].
C  2-Dec-1992 - changed to avoid resetting the viewport [TJP].
C 26-Feb-1997 - revised description [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL SX, SY
C
      IF (PGNOTO('PGQVSZ')) THEN
         X1 = 0.0
         X2 = 0.0
         Y1 = 0.0
         Y2 = 0.0
         RETURN
      END IF
C
      IF (UNITS.EQ.0) THEN
          SX = PGXSZ(PGID)
          SY = PGYSZ(PGID)
      ELSE IF (UNITS.EQ.1) THEN
          SX = PGXPIN(PGID)
          SY = PGYPIN(PGID)
      ELSE IF (UNITS.EQ.2) THEN
          SX = (PGXPIN(PGID)/25.4)
          SY = (PGYPIN(PGID)/25.4)
      ELSE IF (UNITS.EQ.3) THEN
          SX = 1.0
          SY = 1.0
      ELSE
          CALL GRWARN(
     1        'Illegal value for parameter UNITS in routine PGQVSZ')
          SX = PGXSZ(PGID)
          SY = PGYSZ(PGID)
      END IF
      X1 = 0.0
      X2 = PGXSZ(PGID)/SX
      Y1 = 0.0
      Y2 = PGYSZ(PGID)/SY
      END
C*PGQWIN -- inquire window boundary coordinates
C%void cpgqwin(float *x1, float *x2, float *y1, float *y2);
C+
      SUBROUTINE PGQWIN (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Inquiry routine to determine the current window setting.
C The values returned are world coordinates.
C
C Arguments:
C  X1     (output) : the x-coordinate of the bottom left corner
C                    of the window.
C  X2     (output) : the x-coordinate of the top right corner
C                    of the window.
C  Y1     (output) : the y-coordinate of the bottom left corner
C                    of the window.
C  Y2     (output) : the y-coordinate of the top right corner
C                    of the window.
C--
C 26-Sep-1985 - new routine (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      X1 = PGXBLC(PGID)
      X2 = PGXTRC(PGID)
      Y1 = PGYBLC(PGID)
      Y2 = PGYTRC(PGID)
      END
C*PGRECT -- draw a rectangle, using fill-area attributes
C%void cpgrect(float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGRECT (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C This routine can be used instead of PGPOLY for the special case of
C drawing a rectangle aligned with the coordinate axes; only two
C vertices need be specified instead of four.  On most devices, it is
C faster to use PGRECT than PGPOLY for drawing rectangles.  The
C rectangle has vertices at (X1,Y1), (X1,Y2), (X2,Y2), and (X2,Y1).
C
C Arguments:
C  X1, X2 (input) : the horizontal range of the rectangle.
C  Y1, Y2 (input) : the vertical range of the rectangle.
C--
C 21-Nov-1986 - [TJP].
C 22-Mar-1988 - use GRRECT for fill [TJP].
C  6-Mar-1995 - add hatching (by calling PGHTCH) [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL XP(4), YP(4)
C
      CALL PGBBUF
C
C Outline only.
C
      IF (PGFAS(PGID).EQ.2) THEN
         CALL GRMOVA(X1,Y1)
         CALL GRLINA(X1,Y2)
         CALL GRLINA(X2,Y2)
         CALL GRLINA(X2,Y1)
         CALL GRLINA(X1,Y1)
C
C Hatching.
C
      ELSE IF (PGFAS(PGID).EQ.3 .OR. PGFAS(PGID).EQ.4) THEN
         XP(1) = X1
         XP(2) = X1
         XP(3) = X2
         XP(4) = X2
         YP(1) = Y1
         YP(2) = Y2
         YP(3) = Y2
         YP(4) = Y1
         CALL PGHTCH(4, XP, YP, 0.0)
         IF (PGFAS(PGID).EQ.4) CALL PGHTCH(4, XP, YP, 90.0)
C
C Solid fill.
C
      ELSE
          CALL GRRECT(X1,Y1,X2,Y2)
          CALL GRMOVA(X1,Y1)
      END IF
      CALL PGEBUF
      END
C*PGRND -- find the smallest `round' number greater than x
C%float cpgrnd(float x, int *nsub);
C+
      REAL FUNCTION PGRND (X, NSUB)
      REAL X
      INTEGER NSUB
C
C Routine to find the smallest "round" number larger than x, a
C "round" number being 1, 2 or 5 times a power of 10. If X is negative,
C PGRND(X) = -PGRND(ABS(X)). eg PGRND(8.7) = 10.0,
C PGRND(-0.4) = -0.5.  If X is zero, the value returned is zero.
C This routine is used by PGBOX for choosing  tick intervals.
C
C Returns:
C  PGRND         : the "round" number.
C Arguments:
C  X      (input)  : the number to be rounded.
C  NSUB   (output) : a suitable number of subdivisions for
C                    subdividing the "nice" number: 2 or 5.
C--
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C  2-Dec-1991 - Fix for bug found on Fujitsu [TJP].
C-----------------------------------------------------------------------
      INTEGER  I,ILOG
      REAL     FRAC,NICE(3),PWR,XLOG,XX
      INTRINSIC ABS, LOG10, SIGN
      DATA     NICE/2.0,5.0,10.0/
C
      IF (X.EQ.0.0) THEN
          PGRND = 0.0
          NSUB = 2
          RETURN
      END IF
      XX   = ABS(X)
      XLOG = LOG10(XX)
      ILOG = XLOG
      IF (XLOG.LT.0) ILOG=ILOG-1
      PWR  = 10.0**ILOG
      FRAC = XX/PWR
      I = 3
      IF (FRAC.LE.NICE(2)) I = 2
      IF (FRAC.LE.NICE(1)) I = 1
      PGRND = SIGN(PWR*NICE(I),X)
      NSUB = 5
      IF (I.EQ.1) NSUB = 2
      END
C*PGRNGE -- choose axis limits
C%void cpgrnge(float x1, float x2, float *xlo, float *xhi);
C+
      SUBROUTINE PGRNGE (X1, X2, XLO, XHI)
      REAL X1, X2, XLO, XHI
C
C Choose plotting limits XLO and XHI which encompass the data
C range X1 to X2.
C
C Arguments:
C  X1, X2 (input)  : the data range (X1<X2), ie, the min and max values
C                    to be plotted.
C  XLO, XHI (output) : suitable values to use as the extremes of a graph
C                    axis (XLO <= X1, XHI >= X2).
C--
C 10-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      XLO = X1 - 0.1*(X2-X1)
      XHI = X2 + 0.1*(X2-X1)
      IF (XLO.LT.0.0 .AND. X1.GE.0.0) XLO = 0.0
      IF (XHI.GT.0.0 .AND. X2.LE.0.0) XHI = 0.0
      END
C*PGSAH -- set arrow-head style
C%void cpgsah(int fs, float angle, float barb);
C+
      SUBROUTINE PGSAH (FS, ANGLE, BARB)
      INTEGER  FS
      REAL ANGLE, BARB
C
C Set the style to be used for arrowheads drawn with routine PGARRO.
C
C Argument:
C  FS     (input)  : FS = 1 => filled; FS = 2 => outline.
C                    Other values are treated as 2. Default 1.
C  ANGLE  (input)  : the acute angle of the arrow point, in degrees;
C                    angles in the range 20.0 to 90.0 give reasonable
C                    results. Default 45.0.
C  BARB   (input)  : the fraction of the triangular arrow-head that
C                    is cut away from the back. 0.0 gives a triangular
C                    wedge arrow-head; 1.0 gives an open >. Values 0.3
C                    to 0.7 give reasonable results. Default 0.3.
C--
C 13-Oct-1992 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      PGAHS(PGID) = FS
      IF (PGAHS(PGID).NE.1) PGAHS(PGID) = 2
      PGAHA(PGID) = ANGLE
      PGAHV(PGID) = BARB
C
      END
C*PGSAVE -- save PGPLOT attributes
C%void cpgsave(void);
C+
      SUBROUTINE PGSAVE
C
C This routine saves the current PGPLOT attributes in a private storage
C area. They can be restored by calling PGUNSA (unsave). Attributes
C saved are: character font, character height, color index, fill-area
C style, line style, line width, pen position, arrow-head style, 
C hatching style, and clipping state. Color representation is not saved.
C
C Calls to PGSAVE and PGUNSA should always be paired. Up to 20 copies
C of the attributes may be saved. PGUNSA always retrieves the last-saved
C values (last-in first-out stack).
C
C Note that when multiple devices are in use, PGUNSA retrieves the
C values saved by the last PGSAVE call, even if they were for a
C different device.
C
C Arguments: none
C--
C 20-Apr-1992 - new routine [TJP].
C 27-Nov-1992 - add arrowhead style [TJP].
C  6-Oct-1993 - add text opacity [TJP].
C 28-Feb-1994 - correct bug (variable not saved) [TJP].
C 26-Feb-1995 - add hatching attributes.
C 19-Jun-1996 - correction in header comments [TJP].
C 26-Feb-1997 - add clipping state [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXS
      PARAMETER (MAXS=20)
C
      INTEGER LEV
      INTEGER CF(MAXS), CI(MAXS), FS(MAXS), LS(MAXS), LW(MAXS)
      INTEGER AHFS(MAXS), TBG(MAXS), CLP(MAXS)
      REAL    CH(MAXS), POS(2,MAXS)
      REAL    AHANG(MAXS), AHBARB(MAXS), HSA(MAXS), HSS(MAXS), HSP(MAXS)
      SAVE    LEV, CF, CI, FS, LS, LW, AHFS, TBG, CH, POS
      SAVE    AHANG, AHBARB, HSA, HSS, HSP, CLP
      DATA    LEV /0/
C
      IF (LEV.GE.MAXS) THEN
          CALL GRWARN('Too many unmatched calls to PGSAVE')
      ELSE
          LEV = LEV+1
          CALL PGQCF(CF(LEV))
          CALL PGQCH(CH(LEV))
          CALL PGQCI(CI(LEV))
          CALL PGQFS(FS(LEV))
          CALL PGQLS(LS(LEV))
          CALL PGQLW(LW(LEV))
C          CALL PGQVP(0, VP(1,LEV), VP(2,LEV), VP(3,LEV), VP(4,LEV))
C          CALL PGQWIN(WIN(1,LEV), WIN(2,LEV), WIN(3,LEV), WIN(4,LEV))
          CALL PGQPOS(POS(1,LEV), POS(2,LEV))
          CALL PGQAH(AHFS(LEV), AHANG(LEV), AHBARB(LEV))
          CALL PGQTBG(TBG(LEV))
          CALL PGQHS(HSA(LEV), HSS(LEV), HSP(LEV))
          CALL PGQCLP(CLP(LEV))
      END IF
      RETURN     
C
C*PGUNSA -- restore PGPLOT attributes
C%void cpgunsa(void);
C+
      ENTRY PGUNSA
C
C This routine restores the PGPLOT attributes saved in the last call to
C PGSAVE. Usage: CALL PGUNSA (no arguments). See PGSAVE.
C
C Arguments: none
C-----------------------------------------------------------------------
      IF (LEV.LE.0) THEN
          CALL GRWARN('PGUNSA: nothing has been saved')
      ELSE
          CALL PGSCF(CF(LEV))
          CALL PGSCH(CH(LEV))
          CALL PGSCI(CI(LEV))
          CALL PGSFS(FS(LEV))
          CALL PGSLS(LS(LEV))
          CALL PGSLW(LW(LEV))
C          CALL PGSVP(VP(1,LEV), VP(2,LEV), VP(3,LEV), VP(4,LEV))
C          CALL PGSWIN(WIN(1,LEV), WIN(2,LEV), WIN(3,LEV), WIN(4,LEV))
          CALL PGMOVE(POS(1,LEV), POS(2,LEV))
          CALL PGSAH(AHFS(LEV), AHANG(LEV), AHBARB(LEV))
          CALL PGSTBG(TBG(LEV))
          CALL PGSHS(HSA(LEV), HSS(LEV), HSP(LEV))
          CALL PGSCLP(CLP(LEV))
          LEV = LEV-1
      END IF
      RETURN     
      END
C*PGSCF -- set character font
C%void cpgscf(int font);
C+
      SUBROUTINE PGSCF (FONT)
      INTEGER  FONT
C
C Set the Character Font for subsequent text plotting. Four different
C fonts are available:
C   1: (default) a simple single-stroke font ("normal" font)
C   2: roman font
C   3: italic font
C   4: script font
C This call determines which font is in effect at the beginning of
C each text string. The font can be changed (temporarily) within a text
C string by using the escape sequences \fn, \fr, \fi, and \fs for fonts
C 1, 2, 3, and 4, respectively.
C
C Argument:
C  FONT   (input)  : the font number to be used for subsequent text
C                    plotting (in range 1-4).
C--
C 26-Sep-1985 - new routine [TJP].
C 25-OCT-1993 - changed name of argument [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCF')) RETURN
      CALL GRSFNT(FONT)
      END
C*PGSCH -- set character height
C%void cpgsch(float size);
C+
      SUBROUTINE PGSCH (SIZE)
      REAL SIZE
C
C Set the character size attribute. The size affects all text and graph
C markers drawn later in the program. The default character size is
C 1.0, corresponding to a character height about 1/40 the height of
C the view surface.  Changing the character size also scales the length
C of tick marks drawn by PGBOX and terminals drawn by PGERRX and PGERRY.
C
C Argument:
C  SIZE   (input)  : new character size (dimensionless multiple of
C                    the default size).
C--
C (1-Mar-1983)
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     XC, XCNEW, YC, XS, YS
C
      IF (PGNOTO('PGSCH')) RETURN
C
      CALL GRCHSZ(PGID, XC, YC, XS, YS)
      IF (PGXSZ(PGID)/PGXPIN(PGID) .GT.
     1    PGYSZ(PGID)/PGYPIN(PGID)) THEN
          XCNEW = SIZE*XC*PGYSZ(PGID)/YS/40.0
      ELSE
          XCNEW = SIZE*XC*(PGXSZ(PGID)*PGYPIN(PGID)/PGXPIN(PGID))
     1            /YS/40.0
      END IF
      CALL GRSETC(PGID,XCNEW)
      PGXSP(PGID) = XS*XCNEW/XC
      PGYSP(PGID) = YS*XCNEW/XC
      PGCHSZ(PGID) = SIZE
      END
C*PGSCI -- set color index
C%void cpgsci(int ci);
C+
      SUBROUTINE PGSCI (CI)
      INTEGER  CI
C
C Set the Color Index for subsequent plotting, if the output device
C permits this. The default color index is 1, usually white on a black
C background for video displays or black on a white background for
C printer plots. The color index is an integer in the range 0 to a
C device-dependent maximum. Color index 0 corresponds to the background
C color; lines may be "erased" by overwriting them with color index 0
C (if the device permits this).
C
C If the requested color index is not available on the selected device,
C color index 1 will be substituted.
C
C The assignment of colors to color indices can be changed with
C subroutine PGSCR (set color representation).  Color indices 0-15
C have predefined color representations (see the PGPLOT manual), but
C these may be changed with PGSCR.  Color indices above 15  have no
C predefined representations: if these indices are used, PGSCR must
C be called to define the representation.
C
C Argument:
C  CI     (input)  : the color index to be used for subsequent plotting
C                    on the current device (in range 0-max). If the
C                    index exceeds the device-dependent maximum, the
C                    default color index (1) is used.
C--
C 26-Sep-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCI')) RETURN
      CALL GRSCI(CI)
      END
C*PGSCIR -- set color index range
C%void cpgscir(int icilo, int icihi);
C+
      SUBROUTINE PGSCIR(ICILO, ICIHI)
      INTEGER   ICILO, ICIHI
C
C Set the color index range to be used for producing images with
C PGGRAY or PGIMAG. If the range is not all within the range supported
C by the device, a smaller range will be used. The number of
C different colors available for images is ICIHI-ICILO+1.
C
C Arguments:
C  ICILO  (input)  : the lowest color index to use for images
C  ICIHI  (input)  : the highest color index to use for images
C--
C 1994-Mar-17 : new routine [AFT/TJP].
C---
      INCLUDE 'pgplot.inc'
      INTEGER IC1, IC2
C---
      CALL GRQCOL(IC1,IC2)
      PGMNCI(PGID) = MIN(IC2,MAX(IC1,ICILO))
      PGMXCI(PGID) = MIN(IC2,MAX(IC1,ICIHI))
C
      END
C*PGSCLP -- enable or disable clipping at edge of viewport
C%void cpgsclp(int state);
C+
      SUBROUTINE PGSCLP(STATE)
      INTEGER STATE
C
C Normally all PGPLOT primitives except text are ``clipped'' at the
C edge of the viewport: parts of the primitives that lie outside
C the viewport are not drawn. If clipping is disabled by calling this
C routine, primitives are visible wherever they lie on the view
C surface. The default (clipping enabled) is appropriate for almost
C all applications.
C
C Argument:
C  STATE  (input)  : 0 to disable clipping, or 1 to enable clipping.
C 
C 25-Feb-1997 [TJP] - new routine.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCLP')) RETURN
C
C Disable clipping.
C
      IF (STATE.EQ.0) THEN
         CALL GRAREA(PGID,0.,0.,-1.,-1.)
         PGCLP(PGID) = 0
C
C Enable clipping.
C
      ELSE
         CALL GRAREA(PGID,PGXOFF(PGID),PGYOFF(PGID),
     :               PGXLEN(PGID),PGYLEN(PGID))
         PGCLP(PGID) = 1
      END IF
      END
C*PGSCR -- set color representation
C%void cpgscr(int ci, float cr, float cg, float cb);
C+
      SUBROUTINE PGSCR (CI, CR, CG, CB)
      INTEGER CI
      REAL    CR, CG, CB
C
C Set color representation: i.e., define the color to be
C associated with a color index.  Ignored for devices which do not
C support variable color or intensity.  Color indices 0-15
C have predefined color representations (see the PGPLOT manual), but
C these may be changed with PGSCR.  Color indices 16-maximum have no
C predefined representations: if these indices are used, PGSCR must
C be called to define the representation. On monochrome output
C devices (e.g. VT125 terminals with monochrome monitors), the
C monochrome intensity is computed from the specified Red, Green, Blue
C intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television
C systems, NTSC encoding.  Note that most devices do not have an
C infinite range of colors or monochrome intensities available;
C the nearest available color is used.  Examples: for black,
C set CR=CG=CB=0.0; for white, set CR=CG=CB=1.0; for medium gray,
C set CR=CG=CB=0.5; for medium yellow, set CR=CG=0.5, CB=0.0.
C
C Argument:
C  CI     (input)  : the color index to be defined, in the range 0-max.
C                    If the color index greater than the device
C                    maximum is specified, the call is ignored. Color
C                    index 0 applies to the background color.
C  CR     (input)  : red, green, and blue intensities,
C  CG     (input)    in range 0.0 to 1.0.
C  CB     (input)
C--
C 5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCR')) RETURN
      CALL GRSCR(CI,CR,CG,CB)
      END
C*PGSCRL -- scroll window
C%void cpgscrl(float dx, float dy);
C+
      SUBROUTINE PGSCRL (DX, DY)
      REAL DX, DY
C
C This routine moves the window in world-coordinate space while
C leaving the viewport unchanged. On devices that have the
C capability, the pixels within the viewport are scrolled
C horizontally, vertically or both in such a way that graphics
C previously drawn in the window are shifted so that their world
C coordinates are unchanged.
C
C If the old window coordinate range was (X1, X2, Y1, Y2), the new
C coordinate range will be approximately (X1+DX, X2+DX, Y1+DY, Y2+DY).
C The size and scale of the window are unchanged.
C
C Thee window can only be shifted by a whole number of pixels
C (device coordinates). If DX and DY do not correspond to integral
C numbers of pixels, the shift will be slightly different from that
C requested. The new window-coordinate range, and hence the exact
C amount of the shift, can be determined by calling PGQWIN after this
C routine.
C
C Pixels that are moved out of the viewport by this operation are
C lost completely; they cannot be recovered by scrolling back.
C Pixels that are ``scrolled into'' the viewport are filled with
C the background color (color index 0).
C
C If the absolute value of DX is bigger than the width of the window,
C or the aboslute value of DY is bigger than the height of the window,
C the effect will be the same as zeroing all the pixels in the
C viewport.
C
C Not all devices have the capability to support this routine.
C It is only available on some interactive devices that have discrete
C pixels. To determine whether the current device has scroll capability,
C call PGQINF.
C
C Arguments:
C  DX     (input)  : distance (in world coordinates) to shift the
C                    window horizontally (positive shifts window to the
C                    right and scrolls to the left).
C  DY     (input)  : distance (in world coordinates) to shift the
C                    window vertically (positive shifts window up and
C                    scrolls down).
C--
C 25-Feb-97: new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL X1, X2, Y1, Y2, DDX, DDY
      INTEGER NDX, NDY
C
      IF (PGNOTO('PGSCRL')) RETURN
C
C Shift must be a whole number of pixels.
C
      NDX = NINT(DX*PGXSCL(PGID))
      NDY = NINT(DY*PGYSCL(PGID))
C
      IF (NDX.NE.0 .OR. NDY.NE.0) THEN
         CALL PGBBUF
         DDX = NDX/PGXSCL(PGID)
         DDY = NDY/PGYSCL(PGID)
C
C        -- Set new world-ccordinate window.
C
         X1 = PGXBLC(PGID)
         X2 = PGXTRC(PGID)
         Y1 = PGYBLC(PGID)
         Y2 = PGYTRC(PGID)
         PGXBLC(PGID) = X1+DDX
         PGXTRC(PGID) = X2+DDX
         PGYBLC(PGID) = Y1+DDY
         PGYTRC(PGID) = Y2+DDY
         CALL PGVW
C
C        -- Do hardware scroll.
C
         CALL GRSCRL(NDX, NDY)
         CALL PGEBUF
      END IF
      END
C*PGSCRN -- set color representation by name
C%void cpgscrn(int ci, const char *name, int *ier);
C+
      SUBROUTINE PGSCRN(CI, NAME, IER)
      INTEGER CI
      CHARACTER*(*) NAME
      INTEGER IER
C
C Set color representation: i.e., define the color to be
C associated with a color index.  Ignored for devices which do not
C support variable color or intensity.  This is an alternative to
C routine PGSCR. The color representation is defined by name instead
C of (R,G,B) components.
C
C Color names are defined in an external file which is read the first
C time that PGSCRN is called. The name of the external file is
C found as follows:
C 1. if environment variable (logical name) PGPLOT_RGB is defined,
C    its value is used as the file name;
C 2. otherwise, if environment variable PGPLOT_DIR is defined, a
C    file "rgb.txt" in the directory named by this environment
C    variable is used;
C 3. otherwise, file "rgb.txt" in the current directory is used.
C If all of these fail to find a file, an error is reported and
C the routine does nothing.
C
C Each line of the file
C defines one color, with four blank- or tab-separated fields per
C line. The first three fields are the R, G, B components, which
C are integers in the range 0 (zero intensity) to 255 (maximum
C intensity). The fourth field is the color name. The color name
C may include embedded blanks. Example:
C
C 255   0   0 red
C 255 105 180 hot pink
C 255 255 255 white
C   0   0   0 black
C
C Arguments:
C  CI     (input)  : the color index to be defined, in the range 0-max.
C                    If the color index greater than the device
C                    maximum is specified, the call is ignored. Color
C                    index 0 applies to the background color.
C  NAME   (input)  : the name of the color to be associated with
C                    this color index. This name must be in the
C                    external file. The names are not case-sensitive.
C                    If the color is not listed in the file, the
C                    color representation is not changed.
C  IER    (output) : returns 0 if the routine was successful, 1
C                    if an error occurred (either the external file
C                    could not be read, or the requested color was
C                    not defined in the file).
C--
C 12-Oct-1992 [TJP]
C 31-May-1993 [TJP] use GROPTX to open file.
C  7-Nov-1994 [TJP] better error messages.
C-----------------------------------------------------------------------
      INTEGER MAXCOL
      PARAMETER (MAXCOL=1000)
      INTEGER I, IR, IG, IB, J, L, NCOL, UNIT, IOS
      INTEGER GRCTOI, GROPTX, GRTRIM
      REAL RR(MAXCOL), RG(MAXCOL), RB(MAXCOL)
      CHARACTER*20 CREQ, CNAME(MAXCOL)
      CHARACTER*255 TEXT
      SAVE NCOL, CNAME, RR, RG, RB
      DATA NCOL/0/
C
C On first call, read the database.
C
      IF (NCOL.EQ.0) THEN
          CALL GRGFIL('RGB', TEXT)
          L = GRTRIM(TEXT)
          IF (L.LT.1) L = 1
          CALL GRGLUN(UNIT)
          IOS = GROPTX(UNIT, TEXT(1:L), 'rgb.txt', 0)
          IF (IOS.NE.0) GOTO 40
          DO 10 I=1,MAXCOL
              READ (UNIT, '(A)', ERR=15, END=15) TEXT
              J = 1
              CALL GRSKPB(TEXT, J)
              IR = GRCTOI(TEXT, J)
              CALL GRSKPB(TEXT, J)
              IG = GRCTOI(TEXT, J)
              CALL GRSKPB(TEXT, J)
              IB = GRCTOI(TEXT, J)
              CALL GRSKPB(TEXT, J)
              NCOL = NCOL+1
              CALL GRTOUP(CNAME(NCOL), TEXT(J:))
              RR(NCOL) = IR/255.0
              RG(NCOL) = IG/255.0
              RB(NCOL) = IB/255.0
   10     CONTINUE
   15     CLOSE (UNIT)
          CALL GRFLUN(UNIT)
      END IF
C
C Look up requested color and set color representation if found.
C
      CALL GRTOUP(CREQ, NAME)
      DO 20 I=1,NCOL
          IF (CREQ.EQ.CNAME(I)) THEN
              CALL PGSCR(CI, RR(I), RG(I), RB(I))
              IER = 0
              RETURN
          END IF
   20 CONTINUE
C
C Color not found.
C
      IER = 1
      TEXT = 'Color not found: '//NAME
      CALL GRWARN(TEXT)
      RETURN
C
C Database not found.
C
   40 IER = 1
      NCOL = -1
      CALL GRFLUN(UNIT)
      CALL GRWARN('Unable to read color file: '//TEXT(1:L))
      CALL GRWARN('Use environment variable PGPLOT_RGB to specify '//
     :            'the location of the PGPLOT rgb.txt file.')
      RETURN
      END
C
      SUBROUTINE PGSETC (SIZE)
      REAL SIZE
      CALL PGSCH(SIZE)
      END
C*PGSFS -- set fill-area style
C%void cpgsfs(int fs);
C+
      SUBROUTINE PGSFS (FS)
      INTEGER  FS
C
C Set the Fill-Area Style attribute for subsequent area-fill by
C PGPOLY, PGRECT, or PGCIRC.  Four different styles are available: 
C solid (fill polygon with solid color of the current color-index), 
C outline (draw outline of polygon only, using current line attributes),
C hatched (shade interior of polygon with parallel lines, using
C current line attributes), or cross-hatched. The orientation and
C spacing of hatch lines can be specified with routine PGSHS (set
C hatch style).
C
C Argument:
C  FS     (input)  : the fill-area style to be used for subsequent
C                    plotting:
C                      FS = 1 => solid (default)
C                      FS = 2 => outline
C                      FS = 3 => hatched
C                      FS = 4 => cross-hatched
C                    Other values give an error message and are
C                    treated as 2.
C--
C 21-Oct-1985 - new routine [TJP].
C 17-Dec-1990 - pass to GR level [TJP].
C  6-Mar-1995 - add styles 3 and 4 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSFS')) RETURN
      IF (FS.LT.1 .OR. FS.GT.4) THEN
          CALL GRWARN('illegal fill-area style requested')
          PGFAS(PGID) = 2
      ELSE
          PGFAS(PGID) = FS
      END IF
      END
C*PGSHLS -- set color representation using HLS system
C%void cpgshls(int ci, float ch, float cl, float cs);
C+
      SUBROUTINE PGSHLS (CI, CH, CL, CS)
      INTEGER CI
      REAL    CH, CL, CS
C
C Set color representation: i.e., define the color to be
C associated with a color index.  This routine is equivalent to
C PGSCR, but the color is defined in the Hue-Lightness-Saturation
C model instead of the Red-Green-Blue model. Hue is represented
C by an angle in degrees, with red at 120, green at 240,
C and blue at 0 (or 360). Lightness ranges from 0.0 to 1.0, with black
C at lightness 0.0 and white at lightness 1.0. Saturation ranges from
C 0.0 (gray) to 1.0 (pure color). Hue is irrelevant when saturation
C is 0.0.
C
C Examples:           H     L     S        R     G     B
C     black          any   0.0   0.0      0.0   0.0   0.0
C     white          any   1.0   0.0      1.0   1.0   1.0
C     medium gray    any   0.5   0.0      0.5   0.5   0.5
C     red            120   0.5   1.0      1.0   0.0   0.0
C     yellow         180   0.5   1.0      1.0   1.0   0.0
C     pink           120   0.7   0.8      0.94  0.46  0.46
C
C Reference: SIGGRAPH Status Report of the Graphic Standards Planning
C Committee, Computer Graphics, Vol.13, No.3, Association for
C Computing Machinery, New York, NY, 1979. See also: J. D. Foley et al,
C ``Computer Graphics: Principles and Practice'', second edition,
C Addison-Wesley, 1990, section 13.3.5.
C
C Argument:
C  CI     (input)  : the color index to be defined, in the range 0-max.
C                    If the color index greater than the device
C                    maximum is specified, the call is ignored. Color
C                    index 0 applies to the background color.
C  CH     (input)  : hue, in range 0.0 to 360.0.
C  CL     (input)  : lightness, in range 0.0 to 1.0.
C  CS     (input)  : saturation, in range 0.0 to 1.0.
C--
C 9-May-1988 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL CR, CG, CB
      CALL GRXRGB (CH,CL,CS,CR,CG,CB)
      CALL GRSCR(CI,CR,CG,CB)
      END
C*PGSHS -- set hatching style
C%void cpgshs(float angle, float sepn, float phase);
C+
      SUBROUTINE PGSHS (ANGLE, SEPN, PHASE)
      REAL ANGLE, SEPN, PHASE
C
C Set the style to be used for hatching (fill area with fill-style 3).
C The default style is ANGLE=45.0, SEPN=1.0, PHASE=0.0.
C
C Arguments:
C  ANGLE  (input)  : the angle the hatch lines make with the
C                    horizontal, in degrees, increasing 
C                    counterclockwise (this is an angle on the
C                    view surface, not in world-coordinate space).
C  SEPN   (input)  : the spacing of the hatch lines. The unit spacing
C                    is 1 percent of the smaller of the height or
C                    width of the view surface. This should not be
C                    zero.
C  PHASE  (input)  : a real number between 0 and 1; the hatch lines
C                    are displaced by this fraction of SEPN from a
C                    fixed reference.  Adjacent regions hatched with the
C                    same PHASE have contiguous hatch lines. To hatch
C                    a region with alternating lines of two colors,
C                    fill the area twice, with PHASE=0.0 for one color
C                    and PHASE=0.5 for the other color.
C--
C 26-Feb-1995 - new routine [TJP].
C 12-Feb-1996 - check for zero spacing [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSHS')) RETURN
      PGHSA(PGID) = ANGLE
      IF (SEPN.EQ.0.0) THEN
         CALL GRWARN('PGSHS: zero hatch line spacing requested')
         PGHSS(PGID) = 1.0
      ELSE
         PGHSS(PGID) = SEPN
      END IF
      IF (PHASE.LT.0.0 .OR. PHASE.GT.1.0) THEN
         CALL GRWARN('PGSHS: hatching phase must be in (0.0,1.0)')
      END IF
      PGHSP(PGID) = PHASE
C
      END
C*PGSITF -- set image transfer function
C%void cpgsitf(int itf);
C+
      SUBROUTINE PGSITF (ITF)
      INTEGER  ITF
C
C Set the Image Transfer Function for subsequent images drawn by
C PGIMAG, PGGRAY, or PGWEDG. The Image Transfer Function is used
C to map array values into the available range of color indices
C specified with routine PGSCIR or (for PGGRAY on some devices)
C into dot density.
C
C Argument:
C  ITF    (input)  : type of transfer function:
C                      ITF = 0 : linear
C                      ITF = 1 : logarithmic
C                      ITF = 2 : square-root
C--
C 15-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSITF')) RETURN
      IF (ITF.LT.0 .OR. ITF.GT.2) THEN
          PGITF(PGID) = 0
          CALL GRWARN('PGSITF: argument must be 0, 1, or 2')
      ELSE
          PGITF(PGID) = ITF
      END IF
      END
C
      SUBROUTINE PGSIZE (WIDTH, HEIGHT, SHIFTX, SHIFTY, DUMMY)
C
C PGPLOT (obsolete routine; use PGVSIZ in preference): Change the
C size and position of the viewport.
C
C Arguments:
C
C WIDTH (input, real) : width of viewport in inches.
C HEIGHT (input, real) : height of viewport in inches.
C SHIFTX (input, real) : horizontal offset of bottom left corner
C       from blc of page or panel, in inches.
C SHIFTY (input, real) : vertical offset of bottom left corner
C       from blc of page or panel, in inches.
C DUMMY (input, real) : reserved for future use (must be 0.0).
C--
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      REAL     WIDTH,HEIGHT,SHIFTX,SHIFTY,DUMMY
C
      IF (WIDTH.LE.0.0 .OR. HEIGHT.LE.0.0 .OR. DUMMY.NE.0.0) THEN
          CALL GRWARN('PGSIZE ignored: invalid arguments')
          RETURN
      END IF
C
      CALL PGVSIZ(SHIFTX, SHIFTX+WIDTH, SHIFTY, SHIFTY+HEIGHT)
      END
C*PGSLCT -- select an open graphics device
C%void cpgslct(int id);
C+
      SUBROUTINE PGSLCT(ID)
      INTEGER ID
C
C Select one of the open graphics devices and direct subsequent
C plotting to it. The argument is the device identifier returned by
C PGOPEN when the device was opened. If the supplied argument is not a
C valid identifier of an open graphics device, a warning message is
C issued and the current selection is unchanged.
C
C [This routine was added to PGPLOT in Version 5.1.0.]
C
C Arguments:
C
C ID (input, integer): identifier of the device to be selected.
C--
C 22-Dec-1995 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      IF (ID.LT.1 .OR. ID.GT.PGMAXD) THEN
         CALL GRWARN('PGSLCT: invalid argument')
      ELSE IF (PGDEVS(ID).NE.1) THEN
         CALL GRWARN('PGSLCT: requested device is not open')
      ELSE
C        -- Select the new device
         PGID = ID
         CALL GRSLCT(PGID)
      END IF
C
      END
C*PGSLS -- set line style
C%void cpgsls(int ls);
C+
      SUBROUTINE PGSLS (LS)
      INTEGER  LS
C
C Set the line style attribute for subsequent plotting. This
C attribute affects line primitives only; it does not affect graph
C markers, text, or area fill.
C Five different line styles are available, with the following codes:
C 1 (full line), 2 (dashed), 3 (dot-dash-dot-dash), 4 (dotted),
C 5 (dash-dot-dot-dot). The default is 1 (normal full line).
C
C Argument:
C  LS     (input)  : the line-style code for subsequent plotting
C                    (in range 1-5).
C--
C  8-Aug-1985 - new routine, equivalent to GRSLS [TJP].
C  3-Jun-1984 - add GMFILE device [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSLS')) RETURN
      CALL GRSLS(LS)
      END
C*PGSLW -- set line width
C%void cpgslw(int lw);
C+
      SUBROUTINE PGSLW (LW)
      INTEGER  LW
C
C Set the line-width attribute. This attribute affects lines, graph
C markers, and text. The line width is specified in units of 1/200 
C (0.005) inch (about 0.13 mm) and must be an integer in the range
C 1-201. On some devices, thick lines are generated by tracing each
C line with multiple strokes offset in the direction perpendicular to
C the line.
C
C Argument:
C  LW     (input)  : width of line, in units of 0.005 inch (0.13 mm)
C                    in range 1-201.
C--
C  8-Aug-1985 - new routine, equivalent to GRSLW [TJP].
C  1-Feb-1995 - change comment [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSLW')) RETURN
      CALL GRSLW(LW)
      END
C*PGSTBG -- set text background color index
C%void cpgstbg(int tbci);
C+
      SUBROUTINE PGSTBG (TBCI)
      INTEGER  TBCI
C
C Set the Text Background Color Index for subsequent text. By default
C text does not obscure underlying graphics. If the text background
C color index is positive, however, text is opaque: the bounding box
C of the text is filled with the color specified by PGSTBG before
C drawing the text characters in the current color index set by PGSCI.
C Use color index 0 to erase underlying graphics before drawing text.
C
C Argument:
C  TBCI   (input)  : the color index to be used for the background
C                    for subsequent text plotting:
C                      TBCI < 0  => transparent (default)
C                      TBCI >= 0 => text will be drawn on an opaque
C                    background with color index TBCI.
C--
C 16-Oct-1993 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSTBG')) RETURN
      IF (TBCI.LT.0) THEN
          PGTBCI(PGID) = -1
      ELSE
          PGTBCI(PGID) = TBCI
      END IF
      END
C*PGSUBP -- subdivide view surface into panels
C%void cpgsubp(int nxsub, int nysub);
C+
      SUBROUTINE PGSUBP (NXSUB, NYSUB)
      INTEGER NXSUB, NYSUB
C
C PGPLOT divides the physical surface of the plotting device (screen,
C window, or sheet of paper) into NXSUB x NYSUB `panels'. When the 
C view surface is sub-divided in this way, PGPAGE moves to the next
C panel, not the next physical page. The initial subdivision of the
C view surface is set in the call to PGBEG. When PGSUBP is called,
C it forces the next call to PGPAGE to start a new physical page,
C subdivided in the manner indicated. No plotting should be done
C between a call of PGSUBP and a call of PGPAGE (or PGENV, which calls
C PGPAGE).
C
C If NXSUB > 0, PGPLOT uses the panels in row order; if <0, 
C PGPLOT uses them in column order, e.g.,
C      
C  NXSUB=3, NYSUB=2            NXSUB=-3, NYSUB=2   
C                                                
C +-----+-----+-----+         +-----+-----+-----+
C |  1  |  2  |  3  |         |  1  |  3  |  5  |
C +-----+-----+-----+         +-----+-----+-----+
C |  4  |  5  |  6  |         |  2  |  4  |  6  |
C +-----+-----+-----+         +-----+-----+-----+
C
C PGPLOT advances from one panels to the next when PGPAGE is called,
C clearing the screen or starting a new page when the last panel has
C been used. It is also possible to jump from one panel to another
C in random order by calling PGPANL.
C 
C Arguments:
C  NXSUB  (input)  : the number of subdivisions of the view surface in
C                    X (>0 or <0).
C  NYSUB  (input)  : the number of subdivisions of the view surface in
C                    Y (>0).
C--
C 15-Nov-1993 [TJP] - new routine.
C 19-Feb-1994 [TJP] - rescale viewport when panel size changes.
C 23-Sep-1996 [TJP] - correct bug in assignment of PGROWS.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL     CH, XFSZ, YFSZ
      LOGICAL  PGNOTO
      REAL     XVP1, XVP2, YVP1, YVP2

C
      IF (PGNOTO('PGSUBP')) RETURN
C
C Find current character size and viewport (NDC).
C
      CALL PGQCH(CH)
      CALL PGQVP(0, XVP1, XVP2, YVP1, YVP2)
C
C Set the subdivisions.
C
      XFSZ = PGNX(PGID)*PGXSZ(PGID)
      YFSZ = PGNY(PGID)*PGYSZ(PGID)
      PGROWS(PGID) = (NXSUB.GE.0)
      PGNX(PGID) = MAX(ABS(NXSUB),1)
      PGNY(PGID) = MAX(ABS(NYSUB),1)
      PGXSZ(PGID) = XFSZ/PGNX(PGID)
      PGYSZ(PGID) = YFSZ/PGNY(PGID)
C
C The current panel is the last on the physical page, to force
C a new physical page at next PGPAGE.
C
      PGNXC(PGID) = PGNX(PGID)
      PGNYC(PGID) = PGNY(PGID)
C
C Rescale the character size and viewport to the new panel size.
C
      CALL PGSCH(CH)
      CALL PGSVP(XVP1, XVP2, YVP1, YVP2)
C
      END
C*PGSVP -- set viewport (normalized device coordinates)
C%void cpgsvp(float xleft, float xright, float ybot, float ytop);
C+
      SUBROUTINE PGSVP (XLEFT, XRIGHT, YBOT, YTOP)
      REAL XLEFT, XRIGHT, YBOT, YTOP
C
C Change the size and position of the viewport, specifying
C the viewport in normalized device coordinates.  Normalized
C device coordinates run from 0 to 1 in each dimension. The
C viewport is the rectangle on the view surface "through"
C which one views the graph.  All the PG routines which plot lines
C etc. plot them within the viewport, and lines are truncated at
C the edge of the viewport (except for axes, labels etc drawn with
C PGBOX or PGLAB).  The region of world space (the coordinate
C space of the graph) which is visible through the viewport is
C specified by a call to PGSWIN.  It is legal to request a
C viewport larger than the view surface; only the part which
C appears on the view surface will be plotted.
C
C Arguments:
C  XLEFT  (input)  : x-coordinate of left hand edge of viewport, in NDC.
C  XRIGHT (input)  : x-coordinate of right hand edge of viewport,
C                    in NDC.
C  YBOT   (input)  : y-coordinate of bottom edge of viewport, in NDC.
C  YTOP   (input)  : y-coordinate of top  edge of viewport, in NDC.
C--
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     XS, YS
C
      IF (PGNOTO('PGSVP'))  RETURN
      IF (XLEFT.GE.XRIGHT .OR. YBOT.GE.YTOP) THEN
          CALL GRWARN('PGSVP ignored: invalid arguments')
          RETURN
      END IF
C
      XS = PGXSZ(PGID)/PGXPIN(PGID)
      YS = PGYSZ(PGID)/PGYPIN(PGID)
      CALL PGVSIZ(XLEFT*XS, XRIGHT*XS, YBOT*YS, YTOP*YS)
      END
C*PGSWIN -- set window
C%void cpgswin(float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGSWIN (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Change the window in world coordinate space that is to be mapped on
C to the viewport.  Usually PGSWIN is called automatically by PGENV,
C but it may be called directly by the user.
C
C Arguments:
C  X1     (input)  : the x-coordinate of the bottom left corner
C                    of the viewport.
C  X2     (input)  : the x-coordinate of the top right corner
C                    of the viewport (note X2 may be less than X1).
C  Y1     (input)  : the y-coordinate of the bottom left corner
C                    of the viewport.
C  Y2     (input)  : the y-coordinate of the top right corner
C                    of the viewport (note Y2 may be less than Y1).
C--
C 15-Nov-95: check arguments to prevent divide-by-zero [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSWIN')) RETURN
C
C If invalid arguments are specified, issue warning and leave window
C unchanged.
C
      IF (X1.EQ.X2) THEN
         CALL GRWARN('invalid x limits in PGSWIN: X1 = X2.')
      ELSE IF (Y1.EQ.Y2) THEN
         CALL GRWARN('invalid y limits in PGSWIN: Y1 = Y2.')
      ELSE
         PGXBLC(PGID) = X1
         PGXTRC(PGID) = X2
         PGYBLC(PGID) = Y1
         PGYTRC(PGID) = Y2
         CALL PGVW
      END IF
      END
C*PGTBOX -- draw frame and write (DD) HH MM SS.S labelling
C%void cpgtbox(const char *xopt, float xtick, int nxsub, \
C% const char *yopt, float ytick, int nysub);
C+
      SUBROUTINE PGTBOX (XOPT, XTICK, NXSUB, YOPT, YTICK, NYSUB)
C
      REAL XTICK, YTICK
      INTEGER NXSUB, NYSUB
      CHARACTER XOPT*(*), YOPT*(*)
C
C Draw a box and optionally label one or both axes with (DD) HH MM SS 
C style numeric labels (useful for time or RA - DEC plots).   If this 
C style of labelling is desired, then PGSWIN should have been called
C previously with the extrema in SECONDS of time.
C
C In the seconds field, you can have at most 3 places after the decimal
C point, so that 1 ms is the smallest time interval you can time label.
C
C Large numbers are coped with by fields of 6 characters long.  Thus 
C you could have times with days or hours as big as 999999.  However, 
C in practice, you might have trouble with labels overwriting  themselves
C with such large numbers unless you a) use a small time INTERVAL, 
C b) use a small character size or c) choose your own sparse ticks in 
C the call to PGTBOX.  
C
C PGTBOX will attempt, when choosing its own ticks, not to overwrite
C the labels, but this algorithm is not very bright and may fail.
C
C Note that small intervals but large absolute times such as
C TMIN = 200000.0 s and TMAX=200000.1 s will cause the algorithm
C to fail.  This is inherent in PGPLOT's use of single precision
C and cannot be avoided.  In such cases, you should use relative
C times if possible.
C
C PGTBOX's labelling philosophy is that the left-most or bottom tick of
C the axis contains a full label.  Thereafter, only changing fields are
C labelled.  Negative fields are given a '-' label, positive fields
C have none.   Axes that have the DD (or HH if the day field is not
C used) field on each major tick carry the sign on each field.  If the
C axis crosses zero, the zero tick will carry a full label and sign.
C
C This labelling style can cause a little confusion with some special
C cases, but as long as you know its philosophy, the truth can be divined.
C Consider an axis with TMIN=20s, TMAX=-20s.   The labels will look like
C
C        +----------+----------+----------+----------+
C     0h0m20s      10s      -0h0m0s      10s        20s
C
C Knowing that the left field always has a full label and that
C positive fields are unsigned, informs that time is decreasing
C from left to right, not vice versa.   This can become very 
C unclear if you have used the 'F' option, but that is your problem !
C
C Exceptions to this labelling philosophy are when the finest time
C increment being displayed is hours (with option 'Y') or days.  
C Then all fields carry a label.  For example,
C
C        +----------+----------+----------+----------+
C      -10h        -8h        -6h        -4h        -2h
C
C
C PGTBOX can be used in place of PGBOX; it calls PGBOX and only invokes 
C time labelling if requested. Other options are passed intact to PGBOX.
C
C Inputs:
C  XOPT   :  X-options for PGTBOX.  Same as for PGBOX plus 
C
C             'Z' for (DD) HH MM SS.S time labelling
C             'Y' means don't include the day field so that labels
C                 are HH MM SS.S rather than DD HH MM SS.S   The hours
C                 will accumulate beyond 24 if necessary in this case.
C             'X' label the HH field as modulo 24.  Thus, a label
C                 such as 25h 10m would come out as 1h 10m
C             'H' means superscript numbers with d, h, m, & s  symbols
C             'D' means superscript numbers with    o, ', & '' symbols 
C             'F' causes the first label (left- or bottom-most) to
C                 be omitted. Useful for sub-panels that abut each other.
C                 Care is needed because first label carries sign as well.
C             'O' means omit leading zeros in numbers < 10
C                 E.g.  3h 3m 1.2s rather than 03h 03m 01.2s  Useful
C                 to help save space on X-axes. The day field does not 
C                 use this facility.
C
C  YOPT   :  Y-options for PGTBOX.  See above.
C  XTICK  :  X-axis major tick increment.  0.0 for default. 
C  YTICK  :  Y-axis major tick increment.  0.0 for default. 
C            If the 'Z' option is used then XTICK and/or YTICK must
C            be in seconds.
C  NXSUB  :  Number of intervals for minor ticks on X-axis. 0 for default
C  NYSUB  :  Number of intervals for minor ticks on Y-axis. 0 for default
C
C  The regular XOPT and YOPT axis options for PGBOX are
C
C  A : draw Axis (X axis is horizontal line Y=0, Y axis is vertical
C      line X=0).
C  B : draw bottom (X) or left (Y) edge of frame.
C  C : draw top (X) or right (Y) edge of frame.
C  G : draw Grid of vertical (X) or horizontal (Y) lines.
C  I : Invert the tick marks; ie draw them outside the viewport
C      instead of inside.
C  L : label axis Logarithmically (see below).
C  N : write Numeric labels in the conventional location below the
C      viewport (X) or to the left of the viewport (Y).
C  P : extend ("Project") major tick marks outside the box (ignored if
C      option I is specified).
C  M : write numeric labels in the unconventional location above the
C      viewport (X) or to the right of the viewport (Y).
C  T : draw major Tick marks at the major coordinate interval.
C  S : draw minor tick marks (Subticks).
C  V : orient numeric labels Vertically. This is only applicable to Y.
C      The default is to write Y-labels parallel to the axis.
C  1 : force decimal labelling, instead of automatic choice (see PGNUMB).
C  2 : force exponential labelling, instead of automatic.
C
C      The default is to write Y-labels parallel to the axis
C  
C
C        ******************        EXCEPTIONS       *******************
C
C        Note that 
C          1) PGBOX option 'L' (log labels) is ignored with option 'Z'
C          2) The 'O' option will be ignored for the 'V' option as it 
C             makes it impossible to align the labels nicely
C          3) Option 'Y' is forced with option 'D'
C
C        ***************************************************************
C
C
C--
C 05-Sep-1988 - new routine (Neil Killeen)
C 20-Apr-1991 - add support for new DD (day) field and implement
C               labelling on any axis (bottom,top,left,right) [nebk]
C 10-Jun-1993 - add option 'O' for leading zeros, correctly deal with 
C               user ticks, fully support 'V' and 'NM' options, modify
C               slightly meaning of 'F' option [nebk]
C 16-Jan-1995 - add option 'X' [nebk]
C 16-Aug-1996 - Bring axis labelling displacements more in line with 
C               those of pgbox.f [nebk]
C-----------------------------------------------------------------------
      REAL XTICKD, YTICKD, XMIN, XMAX, YMIN, YMAX
      INTEGER IPT, TSCALX, TSCALY, NXSUBD, NYSUBD
      CHARACTER XXOPT*15, YYOPT*15, SUPTYP*4
      LOGICAL XTIME, YTIME, FIRST, DODAYX, DODAYY, DO2, DOPARA, MOD24
C------------------------------------------------------------------------
C
C  Copy inputs
C
      XTICKD = XTICK
      YTICKD = YTICK
      NXSUBD = NXSUB
      NYSUBD = NYSUB
C
C  Get window in world coordinates
C 
      CALL PGQWIN (XMIN, XMAX, YMIN, YMAX)
C
C  X-axis first
C
      CALL GRTOUP (XXOPT, XOPT)
      XTIME = .FALSE.
      IF (INDEX(XXOPT,'Z').NE.0) THEN
C
C  Work out units for labelling and find the tick increments.
C
        IF (ABS(XMAX-XMIN).LT.0.001) THEN
          CALL GRWARN ('PGTBOX: X-axis time interval too small '//
     *                 '(< 1 ms) for time labels')
        ELSE
          XTIME = .TRUE.
          DODAYX = .TRUE.
          IF (INDEX(XXOPT,'Y').NE.0 .OR. INDEX(XXOPT,'D').NE.0) 
     *        DODAYX = .FALSE.
C
          DOPARA = .TRUE.
          CALL PGTBX1 ('X', DODAYX, DOPARA, XMIN, XMAX, XTICKD, 
     *                 NXSUBD, TSCALX)
        END IF
      END IF
C
C  Same again for Y-axis
C
      CALL GRTOUP (YYOPT, YOPT)
      YTIME = .FALSE.
      IF (INDEX(YYOPT,'Z').NE.0) THEN
        IF (ABS(YMAX-YMIN).LT.0.001) THEN
          CALL GRWARN ('PGTBOX: Y-axis time interval too small '//
     *                 '(< 1ms) for time labels')
        ELSE
          YTIME = .TRUE.
          DODAYY = .TRUE.
          IF (INDEX(YYOPT,'Y').NE.0 .OR. INDEX(YYOPT,'D').NE.0)
     *        DODAYY = .FALSE.
C
          DOPARA = .TRUE.
          IF (INDEX(YYOPT,'V').NE.0) DOPARA = .FALSE.
C
          CALL PGTBX1 ('Y', DODAYY, DOPARA, YMIN, YMAX, YTICKD, 
     *                 NYSUBD, TSCALY)
        END IF
      END IF
C
C  Parse options list.  For call to PGBOX when doing time labelling, we 
C  don't want L (log), N or M (write numeric labels). 
C
      IF (XTIME) THEN
        IPT = INDEX(XXOPT,'L')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
        IPT = INDEX(XXOPT,'N')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
        IPT = INDEX(XXOPT,'M')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
      END IF
C
      IF (YTIME) THEN
        IPT = INDEX(YYOPT,'L')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
        IPT = INDEX(YYOPT,'N')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
        IPT = INDEX(YYOPT,'M')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
      END IF
C
C  Draw box and ticks
C
      CALL PGBOX (XXOPT, XTICKD, NXSUBD, YYOPT, YTICKD, NYSUBD)
C
C  Add (DD) HH MM SS labels if desired.  Go back to the original user
C  specified options list.
C
      XXOPT = ' '
      CALL GRTOUP (XXOPT, XOPT)
      IF (XTIME .AND. (INDEX(XXOPT,'N').NE.0 .OR.
     *                 INDEX(XXOPT,'M').NE.0)) THEN
        FIRST = .TRUE.
        IF (INDEX(XXOPT,'F').NE.0) FIRST = .FALSE.
C
        SUPTYP = 'NONE'
        IF (INDEX(XXOPT,'D').NE.0) SUPTYP = ' DMS'
        IF (INDEX(XXOPT,'H').NE.0) SUPTYP = 'DHMS'
C
        DO2 = .TRUE.
        IF (INDEX(XXOPT,'O').NE.0) DO2 = .FALSE.
C
        DOPARA = .TRUE.
C
        MOD24 = .FALSE.
        IF (INDEX(XXOPT,'X').NE.0) MOD24 = .TRUE.
C
        IF (INDEX(XXOPT,'N').NE.0)
     *    CALL PGTBX4 (DODAYX, SUPTYP, 'X', .TRUE., FIRST, 
     *      XMIN, XMAX, TSCALX, XTICKD, DO2, DOPARA, MOD24)
C
        IF (INDEX(XXOPT,'M').NE.0)
     *    CALL PGTBX4 (DODAYX, SUPTYP, 'X', .FALSE., FIRST, 
     *       XMIN, XMAX, TSCALX, XTICKD, DO2, DOPARA, MOD24)
      END IF
C
      YYOPT = ' '
      CALL GRTOUP (YYOPT, YOPT)
      IF (YTIME .AND. (INDEX(YYOPT,'N').NE.0 .OR.
     *                 INDEX(YYOPT,'M').NE.0)) THEN
        FIRST = .TRUE.
        IF (INDEX(YYOPT,'F').NE.0) FIRST = .FALSE.
C
        SUPTYP = 'NONE'
        IF (INDEX(YYOPT,'D').NE.0) SUPTYP = ' DMS'
        IF (INDEX(YYOPT,'H').NE.0) SUPTYP = 'DHMS'
C
        DOPARA = .TRUE.
        IF (INDEX(YYOPT,'V').NE.0) DOPARA = .FALSE.
C
        DO2 = .TRUE.
        IF (DOPARA .AND. INDEX(YYOPT,'O').NE.0) DO2 = .FALSE.
C
        MOD24 = .FALSE.
        IF (INDEX(YYOPT,'X').NE.0) MOD24 = .TRUE.
C
        IF (INDEX(YYOPT,'N').NE.0)
     *    CALL PGTBX4 (DODAYY, SUPTYP, 'Y', .TRUE., FIRST, 
     *       YMIN, YMAX, TSCALY, YTICKD, DO2, DOPARA, MOD24)
C
        IF (INDEX(YYOPT,'M').NE.0)
     *    CALL PGTBX4 (DODAYY, SUPTYP, 'Y', .FALSE., FIRST, 
     *       YMIN, YMAX, TSCALY, YTICKD, DO2, DOPARA, MOD24)
C
      END IF
C
      RETURN
      END
C PGTBX1 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX1 (AXIS, DODAY, DOPARA, TMIN, TMAX, TICK, 
     *                   NSUB, TSCALE)
C
      REAL TMIN, TMAX, TICK
      INTEGER NSUB, TSCALE
      LOGICAL DODAY, DOPARA
      CHARACTER AXIS*1
C
C Work out what the finest units the time labels will be in and
C return the tick increments if the user does not set them.
C
C This is a support routine for PGTBOX and should not 
C be called by the user.
C
C Input:
C  AXIS   :  'X' or 'Y' for use in determining if labels overwrite
C  TMIN   :  Start time in seconds 
C  TMAX   :  End   time in seconds
C  DOPARA :  True if label to be parallel to axis, else perpendicular
C Input/output:
C  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
C            hours ranging above 24.  Useful for declination labels
C  TICK   :  Major tick interval in seconds.  If 0.0 on input, will 
C            be set here.
C  NSUB   :  Number of minor ticks between major ticks. If 0 on input
C            will be set here.
C Outputs:
C  TSCALE :  Determines finest unit of labelling 
C            (1 => ss, 60 => mm, 3600 => hh, 3600*24 => dd)
C
C 05-Sep-1988 - new routine (Neil Killeen)
C 08-Apr-1991 - correctly work out HH MM SS when the time > 60 h [nebk]
C 20-Apr-1991 - revise to add support for new DD (day) field and
C               do lots of work on tick algorithm [nebk]
C 10-Jun-1993 - deal with user given ticks & rename from PGTIME [nebk/jm]
C-----------------------------------------------------------------------
      INTEGER NLIST1, NLIST2, NLIST3, NLIST4, NTICMX
      PARAMETER (NLIST1 = 19, NLIST2 = 10, NLIST3 = 6, NLIST4 = 8,
     *           NTICMX = 8)
C
      REAL TICKS1(NLIST1), TICKS2(NLIST2), TICKS3(NLIST3), 
     *TICKS4(NLIST4), TOCK, TOCK2, TINT, TINTS, TMINS, TMAXS
      INTEGER NSUBS1(NLIST1), NSUBS2(NLIST2), NSUBS3(NLIST3), 
     *NSUBS4(NLIST4), NPL, NTICK, ITICK, STRLEN
      CHARACTER STR*15
C
      SAVE TICKS1, TICKS2, TICKS3, TICKS4
      SAVE NSUBS1, NSUBS2, NSUBS3, NSUBS4
C
      DATA TICKS1 /0.001,  0.002,                 0.005,
     *             0.01,   0.02,                  0.05,  
     *             0.1,    0.2,                   0.5,  
     *             1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      DATA NSUBS1 / 4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
C
      DATA TICKS2 /1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      DATA NSUBS2 / 4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
C
      DATA TICKS3 /1.0,    2.0,   3.0,    4.0,    6.0,   12.0/
      DATA NSUBS3 / 4,      4,     3,      4,      3,      2/
C
      DATA TICKS4 /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 9.0/
      DATA NSUBS4 / 4,   4,   3,   4,   5,   3,   4,   3 /
C----------------------------------------------------------------------
C
C  Turn off DD (day) field if it has been unnecessarily asked for
C
      IF ((ABS(TMIN).LT.24.0*3600.0) .AND. (ABS(TMAX).LT.24.0*3600.0))
     *   DODAY = .FALSE.
C
C  If a tick size is provided, use it to determine TSCALE
C
      TINT = ABS(TMAX - TMIN)
      TICK = ABS(TICK)
      IF (TICK.NE.0.0) THEN
        IF (TICK.GE.TINT) THEN
          CALL GRWARN ('PGTBX1: user given tick bigger than time '
     *                 //'interval; will auto-tick')
          TICK = 0.0
        ELSE IF (TICK.LT.0.001) THEN
          CALL GRWARN ('PGTBX1: user given tick too small (< 1 ms); '
     *                 //'will auto-tick')
          TICK = 0.0
        ELSE 
          IF (MOD(TICK, 60.0) .NE. 0.0) THEN
            TSCALE = 1
          ELSE IF (MOD(TICK, 3600.0).NE.0.0) THEN
            TSCALE = 60
          ELSE IF (.NOT.DODAY) THEN
            TSCALE = 3600
          ELSE IF (MOD(TICK,(24.0*3600.0)).NE.0.0) THEN
            TSCALE = 3600
          ELSE
            TSCALE = 24 * 3600
          ENDIF
C
C  Make a simple default for the number of minor ticks and bug out
C
          IF (NSUB.EQ.0) NSUB = 2
          RETURN
        END IF
      END IF
C
C  Work out label units depending on time interval if user 
C  wants auto-ticking
C
      IF (TINT.LE.5*60) THEN
        TSCALE = 1
      ELSE IF (TINT.LE.5*3600) THEN
        TSCALE = 60
      ELSE 
        IF (.NOT.DODAY) THEN
          TSCALE = 3600
        ELSE
          IF (TINT.LE.5*24*3600) THEN
            TSCALE = 3600
          ELSE
            TSCALE = 3600*24
          END IF
        END IF
      END IF
C
CCCCC
C  Divide interval into NTICK major ticks and NSUB minor intervals
C  The tick choosing algorithm is not very robust, so watch out
C  if you fiddle anything. 
CCCCC
C
      TINTS = TINT / TSCALE
      IF (TSCALE.EQ.1) THEN
C
C  Time in seconds.  If the time interval is very small, may need to 
C  label with up to 3 decimal places.  Have less ticks to help prevent
C  label overwrite. STR is a dummy tick label to assess label 
C  overwrite potential
C
        IF (DOPARA) THEN
          IF (TINTS.LE.0.01) THEN
            NTICK = 4
            STR = '60.423'
            STRLEN = 6
          ELSE IF (TINTS.LE.0.1) THEN
            NTICK = 5
            STR = '60.42'
            STRLEN = 5
          ELSE IF (TINTS.LE.1.0) THEN
            NTICK = 6
            STR = '60.4'
            STRLEN = 4
          ELSE
            NTICK = 6
            STR = '60s'
            STRLEN = 3
          END IF
        ELSE
          NTICK = 6
          STR = ' '
          STRLEN = 1
        END IF
        TOCK = TINTS / NTICK
C
C  Select nearest tick to TOCK from list.
C
        CALL PGTBX2 (TOCK, NLIST1, TICKS1, NSUBS1, TICK, NSUB, ITICK)
C
C  Check label overwrite and/or too many ticks.
C
        CALL PGTBX3 (DODAY, 0, TSCALE, TINTS, NTICMX, NLIST1, TICKS1,
     *               NSUBS1, ITICK, AXIS, DOPARA, STR(1:STRLEN),
     *               TICK, NSUB)
      ELSE IF (TSCALE.EQ.60) THEN
C
C  Time in minutes 
C
        NTICK = 6
        TOCK = TINTS / NTICK
C
C  Select nearest tick from list
C
        CALL PGTBX2 (TOCK, NLIST2, TICKS2, NSUBS2, TICK, NSUB, ITICK)
C
C  Check label overwrite and/or too many ticks.
C
        IF (DOPARA) THEN
          STR = '42m'
          STRLEN = 3
        ELSE
          STR = ' '
          STRLEN = 1
        END IF
        CALL PGTBX3 (DODAY, 0, TSCALE, TINTS, NTICMX, NLIST2, TICKS2,
     *               NSUBS2, ITICK, AXIS, DOPARA, STR(1:STRLEN),
     *               TICK, NSUB)
      ELSE 
        IF (TSCALE.EQ.3600 .AND. DODAY) THEN
C
C  Time in hours with the day field 
C
          NTICK = 6
          TOCK = TINTS / NTICK
C
C  Select nearest tick from list
C
          CALL PGTBX2 (TOCK, NLIST3, TICKS3, NSUBS3, TICK, NSUB, ITICK)
C
C   Check label overwrite and/or too many ticks.
C
          IF (DOPARA) THEN
            STR = '42h'
            STRLEN = 3
          ELSE
            STR = ' '
            STRLEN = 1
          END IF
          CALL PGTBX3 (DODAY, 0, TSCALE, TINTS, NTICMX, NLIST3, TICKS3,
     *                 NSUBS3, ITICK, AXIS, DOPARA, STR(1:STRLEN),
     *                 TICK, NSUB)
        ELSE
C
C  Time in hours with no day field or time in days. Have less
C  ticks for big numbers or the parallel labels will overwrite.

          IF (DOPARA) THEN
            TMINS = ABS(TMIN) / TSCALE
            TMAXS = ABS(TMAX) / TSCALE            
            CALL PGNPL (-1, NINT(MAX(TINTS,TMINS,TMAXS)), NPL)
            IF (NPL.LE.3) THEN
              NTICK = 6
            ELSE IF (NPL.EQ.4) THEN
              NTICK = 5
            ELSE
              NTICK = 4
            END IF
            STR = '345678912'
            STR(NPL+1:) = 'd'
            STRLEN = NPL + 1
          ELSE
            STR = ' '
            STRLEN = 1
            NTICK = 6
          END IF
          TOCK = TINTS / NTICK
C
C   Select nearest tick from list; 1 choose nearest nice integer 
C   scaled by the appropriate power of 10
C
          CALL PGNPL (-1, NINT(TOCK), NPL)
          TOCK2 = TOCK / 10**(NPL-1)
C
          CALL PGTBX2 (TOCK2, NLIST4, TICKS4, NSUBS4, TICK, NSUB, ITICK)
          TICK = TICK * 10**(NPL-1)
C
C  Check label overwrite and/or too many ticks.
C
          CALL PGTBX3 (DODAY, NPL, TSCALE, TINTS, NTICMX, NLIST4, 
     *                 TICKS4, NSUBS4, ITICK, AXIS, DOPARA,
     *                 STR(1:STRLEN), TICK, NSUB)
        END IF
      END IF
C
C  Convert tick to seconds
C
      TICK = TICK * TSCALE
C
      RETURN
      END
C PGTBX2 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX2 (TOCK, NTICKS, TICKS, NSUBS, TICK, NSUB, ITICK)
C
      INTEGER NTICKS, NSUBS(NTICKS), NSUB, ITICK
      REAL TOCK, TICKS(NTICKS), TICK
C
C Find the nearest tick in a list to a given value.
C
C This is a support routine for PGTBOX and should not be called
C by the user.
C
C Input:
C  TOCK   :  Try to find the nearest tick in the list to TOCK
C  NTICKS :  Number of ticks in list
C  TICKS  :  List of ticks
C  NSUBS  :  List of number of minor ticks between ticks to go with TICKS
C Output:
C  TICK   :  The selected tick
C  ITICK  :  The index of the selected tick from the list TICKS
C Input/output
C  NSUB   :  Number of minor ticks between major ticks. If 0 on input
C            will be set here.
C
C 10-Jun-1993 - new routine [nebk]
C-----------------------------------------------------------------------
      INTEGER I, NSUBD
      REAL DMIN, DIFF
C----------------------------------------------------------------------
      NSUBD = NSUB
      DMIN = 1.0E30
      DO 100 I = 1, NTICKS
        DIFF = ABS(TOCK - TICKS(I))
        IF (DIFF.LT.DMIN) THEN
          TICK = TICKS(I)
          IF (NSUBD.EQ.0) NSUB = NSUBS(I)
          ITICK = I
C
          DMIN = DIFF
        END IF
 100  CONTINUE
C
      RETURN
      END
C PGTBX3 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX3 (DODAY, NPL, TSCALE, TINTS, NTICMX, NTICKS,
     *                   TICKS, NSUBS, ITICK, AXIS, DOPARA, STR,
     *                   TICK, NSUB)
C
      INTEGER TSCALE, NTICMX, NTICKS, ITICK, NSUB, NSUBS(NTICKS), NPL
      REAL TINTS, TICKS(NTICKS), TICK
      CHARACTER AXIS*1, STR*(*)
      LOGICAL DODAY, DOPARA
C
C Try to see if label overwrite is going to occur with this tick 
C selection, or if there are going to be more than a reasonable
C number of ticks in the displayed time range.  If so, choose, 
C if available, the next tick (bigger separation) up in the list.
C If the overwrite requires that we would need to go up to the bext
C TSCALE, give up.  They will need to choose a smaller character size
C
C This is a support routine for PGTBOX and should not 
C be called by the user.
C
C Input:
C  DODAY  :  True if day field being used
C  NPL    :  Number of characters needed to format TICK on input
C  TSCALE :  Dictates what the finest units of the labelling are.
C            1 = sec, 60 = min, 3600 = hr, 24*3600 = days
C  TINTS  :  Absolute time interval in units of TSCALE
C  NTICMX :  Max. reasonable number of ticks to allow in the time range
C  NTICKS :  Number of ticks in list of ticks to choose from
C  TICKS  :  List of ticks from which the current tick was chosen
C  NSUBS  :  List of number of minor ticks/major tick to choose NSUB from
C  ITICK  :  Index of chosen tick in list TICKS
C  AXIS   :  'X' or 'Y' axis
C  DOPARA :  Labels parallel or perpendicualr to axis
C  STR    :  A typical formatted string used for checking overwrite
C Input/output:
C  TICK   :  Current major tick interval in units of TSCALE. May be 
C            made larger if possible if overwrite likely.
C  NSUB   :  Number of minor ticks between major ticks. 
C
C 10-Jun-1993 - new routine [nebk]
C-----------------------------------------------------------------------
      INTEGER NTICK
      REAL LENS, LENX, LENY
C----------------------------------------------------------------------
      CALL PGLEN (4, STR, LENX, LENY)
      LENS = LENX
      IF ( (DOPARA .AND. AXIS.EQ.'Y') .OR.
     *     (.NOT.DOPARA .AND. AXIS.EQ.'X') ) LENS = LENY
C
      IF (TSCALE.EQ.1 .OR. TSCALE.EQ.60 .OR.
     *    (TSCALE.EQ.3600 .AND. DODAY)) THEN
C
C  Time in seconds or minutes, or in hours with a day field
C
        NTICK = INT(TINTS / TICK)
        IF ( (ITICK.LT.NTICKS)  .AND. 
     *       ((DOPARA .AND. (LENS/TSCALE).GT.0.9*TICK) .OR. 
     *       (NTICK.GT.NTICMX)) ) THEN
          IF (TICKS(ITICK+1).LT.TINTS) THEN
            NSUB = NSUBS(ITICK+1)
            TICK = TICKS(ITICK+1)
          END IF
        END IF
      ELSE
C
C  Time in hours and no day field or time in days
C
        NTICK = INT(TINTS / TICK)
        IF ( (DOPARA .AND. (LENS/TSCALE).GT.0.9*TICK) .OR. 
     *       (NTICK.GT.NTICMX) ) THEN
          IF (ITICK.LT.NTICKS) THEN
            IF (TICKS(ITICK+1)*10**(NPL-1).LT.TINTS) THEN
              NSUB = NSUBS(ITICK+1)
              TICK = TICKS(ITICK+1) * 10**(NPL-1)
            END IF
          ELSE
            IF (TICKS(1)*10**NPL.LT.TINTS) THEN
              NSUB = NSUBS(1)
              TICK = TICKS(1) * 10**NPL
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
C PGTBX4 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX4 (DODAY, SUPTYP, AXIS, CONVTL, FIRST, TMIN,
     *                   TMAX, TSCALE, TICK, DO2, DOPARA, MOD24)
C
      REAL TMIN, TMAX, TICK
      INTEGER TSCALE
      CHARACTER AXIS*(*), SUPTYP*(*)
      LOGICAL FIRST, DODAY, CONVTL, DO2, DOPARA, MOD24
C
C Label an axis in (DD) HH MM SS.S style.    This is the main 
C workhorse of the PGTBOX routines.
C
C This is a support subroutine for PGTBOX and should not be 
C called by the user. 
C
C Inputs:
C  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
C            hours ranging above 24.  Useful for declination labels
C  SUPTYP :  If 'DHMS' then superscript the fields with d, h, m, & s
C            If ' DMS' then superscript the fields with    o, '  & '' 
C              Good for declination plots.  You should obviously not 
C              ask for the day field for this to do anything sensible. 
C            If '    ' then no superscripting is done.
C  AXIS   :  'X' for x-axis, 'Y' for y-axis
C  CONVTL :  If .true., write the labels in the conventional axis 
C            locations (bottom and left for 'X' and 'Y').  Otherwise
C            write them on the top and right axes ('X' and 'Y')
C  FIRST  :  If .false. then omit the first label.
C  TMIN   :  Start time (seconds)
C  TMAX   :  End time (seconds)
C  TSCALE :  Determines finest units of axis
C              1 => ss, 60 => mm, 3600 => hh, 3600*24 => dd
C  TICK   :  Major tick interval in seconds
C  DO2    :  If .true., write labels less than 10 with a leading zero.
C  DOPARA :  Y axis label parallel to axis, else perpendicular
C  MOD24  :  HH field labelled as modulo 24
C
C 05-Sep-1988 - new routine (Neil Killeen)
C 20-Apr-1991 - add support for new DD (day) field [nebk]
C 10-Jun-1993 - complete rewrite & rename from PGTLAB. Fixes user given 
C               ticks bug too [nebk]
C 15-Jan-1995 - Add argument MOD24
C-----------------------------------------------------------------------
      INTEGER MAXTIK
      LOGICAL T, F
      PARAMETER (MAXTIK = 1000, T = .TRUE., F = .FALSE.)
C
      REAL SS(MAXTIK), TFRAC(MAXTIK)
      INTEGER DD(MAXTIK), HH(MAXTIK), MM(MAXTIK)
      CHARACTER*1 ASIGN(MAXTIK), ASIGNL
C
      REAL TIME, XLEN, YLEN, COORD, FJUST, RVAL, SSL, DISP,
     *XLEN2, YLEN2
      INTEGER IS, SD, NT, IZERO, IPOS, INEG, IT, I, J, K, SPREC,
     *JST(2), JEND(2), TLEN, LAST, IVAL(3), IVALO(3), IVALZ(3),
     *IVALF(3), IVALL(3), NPASS, INC, DDL, HHL, MML
      CHARACTER SIGNF*1, TEXT*80, AXLOC*2
      LOGICAL WRIT(4)
C-----------------------------------------------------------------------
      CALL PGBBUF
C
C  Direction signs
C
      SD = 1
      IF (TMAX.LT.TMIN) SD = -1
      IS = 1
      IF (TMIN.LT.0.0) IS = -1
C
C  Find first tick.  Return if none.
C
      NT = TMIN / TICK
      IF (IS*SD.EQ.1 .AND. ABS(TMIN).GT.ABS(NT)*TICK) NT = NT + SD
      TIME = NT * TICK
      IF ( (SD.EQ. 1.AND.(TIME.LT.TMIN.OR.TIME.GT.TMAX)) .OR.
     *     (SD.EQ.-1.AND.(TIME.GT.TMIN.OR.TIME.LT.TMAX)) ) RETURN
C
C  Now step through time range in TICK increments and convert
C  times in seconds at each tick to  +/- (DD) HH MM SS.S
C
      IZERO = 0
      IT = 1
 100  IF ( (SD.EQ.1  .AND. TIME.GT.(TMAX+1.0E-5)) .OR.
     *     (SD.EQ.-1 .AND. TIME.LT.(TMAX-1.0E-5)) ) GOTO 200
        IF (IT.GT.MAXTIK) THEN
          CALL GRWARN ('PGTBX4: storage exhausted -- you have'
     *                 //'asked for far too many ticks')
          GOTO 200
        END IF
C
C  Convert to (DD) HH MM SS.S and find fraction of window that this
C  tick falls at
C
        CALL PGTBX5 (DODAY, TIME, ASIGN(IT), DD(IT), HH(IT),
     *               MM(IT), SS(IT))
        TFRAC(IT) = (TIME - TMIN) / (TMAX - TMIN)
C
C  Note zero tick
C
        IF (NT.EQ.0) IZERO = IT
C
C  Increment time
C
        NT = NT + SD
        TIME = NT * TICK
        IT = IT + 1
C
        GOTO 100
 200  CONTINUE
      IT = IT - 1
C
C   Work out the precision with which to write fractional seconds 
C   labels into the SS.S field.   All other fields have integer labels.
C
      SPREC = 0
      IF (TSCALE.EQ.1) THEN
        IF (TICK.LT.0.01) THEN
          SPREC = 3
        ELSE IF (TICK.LT.0.1) THEN
          SPREC = 2
        ELSE IF (TICK.LT.1.0) THEN
          SPREC = 1
        END IF
      END IF
C
C  Label special case of first tick.  Prepare fields and label
C
      CALL PGTBX6 (DODAY, MOD24, TSCALE, DD(1), HH(1), MM(1), 
     *             SS(1), IVALF, RVAL, WRIT)
      SIGNF = 'H'
      IF (DODAY) SIGNF = 'D'
      CALL PGTBX7 (SUPTYP, SIGNF, ASIGN(1), IVALF, RVAL, WRIT,
     *             SPREC, DO2, TEXT, TLEN, LAST)
C
C   Set label displacements from axes.  This is messy for labels oriented
C   perpendicularly on the right hand axis as we need to know how long
C   the longest string we are going to write is before we write any 
C   labels as they are right justified.
C
      IF (AXIS.EQ.'X') THEN
        IF (CONVTL) THEN
          AXLOC = 'B'
          IF (SUPTYP.NE.'NONE') THEN
            DISP = 1.4
          ELSE
            DISP = 1.2
          END IF
        ELSE
          AXLOC = 'T'
          DISP = 0.7
        END IF
      ELSE IF (AXIS.EQ.'Y') THEN
        IF (CONVTL) THEN
          AXLOC = 'LV'
          IF (DOPARA) AXLOC = 'L'
          DISP = 0.7
        ELSE
          IF (DOPARA) THEN
            AXLOC = 'R'
            IF (SUPTYP.NE.'NONE') THEN
              DISP = 1.7
            ELSE
              DISP = 1.9
            END IF
          ELSE
C
C  Work out number of characters in first label
C
            AXLOC = 'RV'
            IF (ASIGN(1).NE.'-' .AND. TMIN*TMAX.LT.0.0) THEN
              CALL PGLEN (2, ' -'//TEXT(1:TLEN), XLEN, YLEN)
            ELSE
              CALL PGLEN (2, ' '//TEXT(1:TLEN), XLEN, YLEN)
            END IF
            CALL PGQCS (2, XLEN2, YLEN2)
            DISP = (XLEN/XLEN2)
          END IF
        END IF
      END IF
C
C  Now write the label to the plot.  The X-axis label for the first tick is
C  centred such that the last field of the label is centred on the tick
C
      IF (FIRST) THEN
        CALL PGLEN (5, TEXT(LAST:TLEN), XLEN, YLEN)
C
        IF (AXIS.EQ.'X') THEN
          COORD = TFRAC(1) + XLEN / 2.0
          FJUST = 1.0
        ELSE IF (AXIS.EQ.'Y') THEN
          IF (DOPARA) THEN
            COORD = TFRAC(1) + YLEN / 2.0
            FJUST = 1.0
          ELSE
            FJUST = 1.0
            COORD = TFRAC(1)
          END IF
        END IF
        CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
      END IF
      IF (IT.EQ.1) RETURN
C
C   Designate which field out of DD or HH will carry the sign, depending
C   on whether you want the day field or not for the rest of the ticks
C
      SIGNF = 'H'
      IF (DODAY) SIGNF = 'D'
C
C  Set up labelling justifications for the rest of the labels
C
      IF (AXIS.EQ.'X') THEN
        FJUST = 0.5
      ELSE IF (AXIS.EQ.'Y') THEN
        IF (DOPARA) THEN
          FJUST = 0.5
        ELSE
          FJUST = 1.0
        END IF
      END IF
C
C  Note zero crossings; IPOS is the first positive tick and
C  INEG is the first negative tick on either side of 0
C
      IPOS = 0
      INEG = 0
C
      IF (IZERO.NE.0) THEN
        J = IZERO - 1
        IF (J.GE.1) THEN
          IF (ASIGN(J).EQ.'-') THEN
            INEG = J
          ELSE IF (ASIGN(J).EQ.' ') THEN
            IPOS = J
          END IF
        END IF
        J = IZERO + 1
        IF (J.LE.IT) THEN
          IF (ASIGN(J).EQ.'-') THEN
            INEG = J
          ELSE IF (ASIGN(J).EQ.' ') THEN
            IPOS = J
          END IF
        END IF
      END IF
C
C  Now label special case of zero tick. It carries the sign change
C  when going from positive to negative time, left to right.
C
      IF (IZERO.NE.0 .AND. IZERO.NE.1) THEN
        CALL PGTBX6 (DODAY, MOD24, TSCALE, DD(IZERO), HH(IZERO), 
     *               MM(IZERO), SS(IZERO), IVALZ, RVAL, WRIT)
C
        IF (ASIGN(IZERO-1).EQ.' ') ASIGN(IZERO) = '-'
        CALL PGTBX7 (SUPTYP, SIGNF, ASIGN(IZERO), IVALZ, RVAL, WRIT,
     *               SPREC, DO2, TEXT, TLEN, LAST)
C
        COORD = TFRAC(IZERO)
        CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
      END IF
C
C   We may need an extra "virtual" tick if there is no zero crossing
C   and SD=-1 & IS=1 or SD=1 & IS=-1.  It is used to work out which
C   fields to label on the right most tick which is labelled first.
C
      IF (IZERO.EQ.0) THEN
        IF (SD*IS.EQ.-1) THEN 
          IF ( (SD.EQ.-1 .AND. TIME.LE.0.0) .OR.
     *         (SD.EQ. 1 .AND. TIME.GE.0.0) ) TIME = 0.0
          CALL PGTBX5 (DODAY, TIME, ASIGNL, DDL, HHL, MML, SSL)
          CALL PGTBX6 (DODAY, MOD24, TSCALE, DDL, HHL, MML, SSL,
     *                 IVALL, RVAL, WRIT)
        END IF
      END IF
C
C  We want to label in the direction(s) away from zero, so we may  need
C  two passes. Determine the start and end ticks for each required pass.
C
      JST(2) = 0
      JEND(2) = 0
      NPASS = 1
      IF (IZERO.EQ.0) THEN
        IF (IS*SD.EQ.1) THEN
          JST(1) = 1
          JEND(1) = IT
        ELSE
          JST(1) = IT
          JEND(1) = 1
        END IF
      ELSE
        IF (INEG.EQ.0 .OR. IPOS.EQ.0) THEN
          JST(1) = IZERO
          JEND(1) = IT
          IF (IZERO.EQ.IT) JEND(1) = 1
        ELSE
          NPASS = 2
          JST(1) = IZERO
          JEND(1) = 1
          JST(2) = IZERO
          JEND(2) = IT
        END IF
      END IF
C
C  Now label the rest of the ticks.  Always label away from 0
C
      DO 400 I = 1, NPASS
C
C  Initialize previous tick values.  Use virtual tick if labelling
C  left to right without a zero (one pass)
C
        DO 250 K = 1, 3
          IVALO(K) = IVALZ(K)
          IF (IZERO.EQ.0) THEN
            IVALO(K) = IVALL(K)
            IF (JST(I).EQ.1) IVALO(K) = IVALF(K)
          END IF
  250   CONTINUE
C
        INC = 1
        IF (JEND(I).LT.JST(I)) INC = -1
        DO 300 J = JST(I), JEND(I), INC
C
C  First and zero tick already labelled
C
          IF (J.NE.1 .AND. J.NE.IZERO) THEN
C
C  Prepare fields
C
            CALL PGTBX6 (DODAY, MOD24, TSCALE, DD(J), HH(J), MM(J),
     *                   SS(J), IVAL, RVAL, WRIT)
C
C  Don't write unchanging fields
C
            DO 275 K = 1, 3
              IF (IVAL(K).EQ.IVALO(K)) WRIT(K) = F
 275        CONTINUE
C
C  Prepare label
C
            CALL PGTBX7 (SUPTYP, SIGNF, ASIGN(J), IVAL, RVAL, WRIT,
     *                   SPREC, DO2, TEXT, TLEN, LAST)
C
C  Write label
C
            COORD = TFRAC(J)
            CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
C
C  Update old values
C
            DO 280 K = 1, 3
              IVALO(K) = IVAL(K)
  280       CONTINUE
          END IF
 300    CONTINUE
 400  CONTINUE
      CALL PGEBUF
C 
      RETURN
      END
C PGTBX5 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX5 (DODAY, TSEC, ASIGN, D, H, M, S)
C      
      REAL S, TSEC
      INTEGER  D, H, M
      LOGICAL DODAY
      CHARACTER*1 ASIGN
C
C  Convert time in seconds to (DD) HH MM SS.S
C
C Input
C  DODAY  :  Use day field if true, else hours accumulates beyond 24
C  TSEC   :  Time in seconds (signed)
C Output
C  ASIGN  :  Sign, ' ' or '-'
C  D,H,M  :  DD, HH, MM (unsigned)
C  S      :  SS.S       (unsigned)
C
C 10-Jun-1993 - new routine [nebk]
C-----------------------------------------------------------------------
      INTEGER IT
C----------------------------------------------------------------------
      ASIGN = ' '
      IF (TSEC.LT.0.0) ASIGN = '-'
C
      S = MOD(ABS(TSEC),60.0)
C
      IT = NINT(ABS(TSEC)-S) / 60
      M = MOD(IT,60)
C
      IT = (IT - M) / 60
      IF (DODAY) THEN
        H = MOD(IT,24)
        D = (IT-H) / 24
      ELSE
        H = IT
        D = 0
      END IF
C
      RETURN
      END
C PGTBX6 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX6 (DODAY, MOD24, TSCALE, DD, HH, MM, SS, IVAL, 
     *                   RVAL, WRIT)
C
      INTEGER TSCALE, IVAL(3), DD, HH, MM
      REAL SS, RVAL
      LOGICAL WRIT(4), DODAY, MOD24
C
C   Find out which of the DD HH MM SS.S fields we want to write
C   into the label according to TSCALE and make a round off
C   error check.
C
C  Input:
C    DODAY  :  Use day field if true else hours accrue beyond 24
C    MOD24  :  HH field labelled as modulo 24
C    TSCALE :  Dictates which fields appear in labels
C    DD     :  Day of time  (will be 0 if DODAY=F and HH will compensate)
C    HH     :  Hour of time
C    MM     :  Minute of time
C    SS     :  Second of time
C  Output:
C    IVAL(3):  DD HH MM to write into label
C    RVAL   :  SS.S to write into label
C    WRIT(4):  T or F if DD,HH,MM,SS are to be written into the label
C              or not.  IVAL and RVAL fields are set explicitly to
C              zero if the corresponding WRIT field is false.
C              This really is overkill.
C
C  10-Jun-1993 - New routine [nebk]
C  16-Jan-1995 - Add argument MOD24
C-----------------------------------------------------------------------
      LOGICAL T, F
      PARAMETER (T = .TRUE., F = .FALSE.)
      INTEGER WM
C-----------------------------------------------------------------------
      IVAL(1) = DD
      IVAL(2) = HH
      IVAL(3) = MM
      RVAL    = SS
C
C  SS should be 0.0; round off may get us 59.999 or the like but
C  not 60.001 (see PGTBX5)
C
      IF (TSCALE.GT.1) THEN
        WM = NINT(SS/60.0)
        IVAL(3) = IVAL(3) + WM
        IF (IVAL(3).EQ.60) THEN
          IVAL(3) = 0
          IVAL(2) = IVAL(2) + 1
          IF (DODAY .AND. IVAL(2).EQ.24) THEN
            IVAL(2) = 0
            IVAL(1) = IVAL(1) + 1
          END IF
        END IF
      END IF
C
C Make HH field modulo 24 if desired
C
      IF (MOD24) IVAL(2) = MOD(IVAL(2),24)
C
      IF (TSCALE.EQ.1) THEN
C
C  Label contains (DD) HH MM SS.S
C
        WRIT(1) = DODAY
        WRIT(2) = T
        WRIT(3) = T
        WRIT(4) = T
      ELSE IF (TSCALE.EQ.60) THEN
C
C  Label contains (DD) HH MM
C
        WRIT(1) = DODAY
        WRIT(2) = T
        WRIT(3) = T
C        
        RVAL    = 0.0
        WRIT(4) = F
      ELSE IF (TSCALE.EQ.3600) THEN
C
C  Label contains (DD) HH
C
        WRIT(1) = DODAY
        WRIT(2) = T
C
        IVAL(3) = 0
        WRIT(3) = F
C  
        RVAL    = 0.0
        WRIT(4) = F
      ELSE IF (TSCALE.EQ.3600*24) THEN
C
C  Label contains DD
C
        WRIT(1) = T
C
        IVAL(2) = 0
        WRIT(2) = F
C
        IVAL(3) = 0
        WRIT(3) = F
C
        RVAL    = 0.0
        WRIT(4) = F
      END IF
C
      RETURN
      END
      SUBROUTINE PGTBX7 (SUPTYP, SIGNF, ASIGN, IVAL, RVAL, WRIT,
     *                   SPREC, DO2, TEXT, TLEN, LAST)
C
      REAL RVAL
      INTEGER IVAL(3), TLEN, SPREC, LAST
      CHARACTER ASIGN*1, TEXT*(*), SIGNF*1, SUPTYP*4
      LOGICAL WRIT(4), DO2
C
C Write (DD) HH MM SS.S time labels into a string
C
C This is a support routine for PGTBOX and should not be
C called by the user
C
C Inputs
C  SUPTYP :  '    ', 'DHMS', or ' DMS' for no superscript labelling,
C            d,h,m,s   or   o,','' superscripting
C  SIGNF  :  Tells which field the sign is associated with.  
C            One of 'D', 'H', 'M', or 'S'    
C  ASIGN  :  ' ' or '-' for positive or negative times
C  IVAL(3):  Day, hour, minutes of time
C  RVAL   :  Seconds of time
C  WRIT(4):  If .true. then write DD, HH, MM, SS  into label
C  SPREC  :  Number of places after the decimal to write seconds 
C            string to.  Must be in the range 0-3
C  DO2    :  If true, add a leading zero to numbers < 10
C Outputs
C  TEXT   :  Label
C  TLEN   :  Length of label
C  LAST   :  Is the location of the start character of the last 
C            field written into TEXT
C
C  05-Sep-1989 -- New routine (Neil Killeen)
C  20-Apr-1991 -- Complete rewrite; support for new DD (day) field and 
C                 superscripted labels [nebk]
C  14-May-1991 -- Removed BSL as a parameter (Char(92)) and made it
C                 a variable to appease Cray compiler [mjs/nebk]
C  10-Jun-1993 -- Rename from PGTLB1, add code to label superscript 
C                 seconds above the '.' and add DO2 option [nebk/jm]
C-----------------------------------------------------------------------
      INTEGER FLEN, FST, FMAX, TRLEN(3), SUPPNT, TMPNT, TLEN2, 
     *IR1, IR2, IP
      CHARACTER FIELD*30, FRMAT2(3)*2, SUPER(4,3)*11, TMP*100, 
     *BSL*1, FRMAT*30
C
      SAVE FRMAT2
      SAVE TRLEN
C
      DATA FRMAT2 /'I1', 'I2', 'I3'/
      DATA TRLEN /5, 11, 5/
C-----------------------------------------------------------------------
C
C   Initialize
C
      BSL = CHAR(92)
      TLEN = 0
      TEXT = ' '
C
C   Assign superscripting strings.  Use CHAR(92) for backslash as the
C   latter must be escaped on SUNs thus requiring preprocessing.  The
C   concatenator operator precludes the use of a data statement
C
      SUPER(1,1) = BSL//'ud'//BSL//'d'
      SUPER(2,1) = BSL//'uh'//BSL//'d'
      SUPER(3,1) = BSL//'um'//BSL//'d'
      SUPER(4,1) = BSL//'us'//BSL//'d'
C
      SUPER(1,2) = BSL//'u'//BSL//'(2199)'//BSL//'d'
      SUPER(2,2) = BSL//'u'//BSL//'(2729)'//BSL//'d'
      SUPER(3,2) = BSL//'u'//BSL//'(2727)'//BSL//'d'
      SUPER(4,2) = BSL//'u'//BSL//'(2728)'//BSL//'d'
C      
      SUPER(1,3) = BSL//'u'//' '//BSL//'d'
      SUPER(2,3) = BSL//'u'//' '//BSL//'d'
      SUPER(3,3) = BSL//'u'//' '//BSL//'d'
      SUPER(4,3) = BSL//'u'//' '//BSL//'d'
C
C   Point at correct superscript strings
C
      IF (SUPTYP.EQ.'DHMS') THEN
        SUPPNT = 1
      ELSE IF (SUPTYP.EQ.' DMS') THEN
        SUPPNT = 2
      ELSE
        SUPPNT = 3
      END IF
C
CCCC
C   Days field
CCCC
C
      IF (WRIT(1)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field
C
        FIELD = ' '
        CALL PGNPL (0, IVAL(1), FLEN)
        WRITE (FIELD, '(I6)') IVAL(1)
        FMAX = 6
        FST = FMAX - FLEN + 1
C
C   Write output text string with desired superscripting
C
        TMPNT = 2
        IF (SIGNF.EQ.'D' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TMP = ASIGN//FIELD(FST:FMAX)//SUPER(1,SUPPNT)
        TLEN2 = (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C
CCCC 
C   Hours field
CCCC
C
      IF (WRIT(2)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field
C
        FIELD = ' '
        CALL PGNPL (0, IVAL(2), FLEN)
        WRITE (FIELD, '(I6)') IVAL(2)
        FMAX = 6
        FST = FMAX - FLEN + 1
C
        IF (DO2 .AND. FLEN.EQ.1) THEN
          FLEN = FLEN + 1
          FST = FST - 1
          FIELD(FST:FST) = '0'
        END IF
C
C   Write output text string with desired superscripting
C
        TMPNT = 2
        IF (SIGNF.EQ.'H' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TMP = ASIGN//FIELD(FST:FMAX)//SUPER(2,SUPPNT)
        TLEN2 = (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C
CCCC
C   Minutes field
CCCC
C
      IF (WRIT(3)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field with desired superscripting
C
        FIELD = ' '
        WRITE (FIELD, '(I2, A)') IVAL(3), 
     *                           SUPER(3,SUPPNT)(1:TRLEN(SUPPNT))
        FMAX = 2 + TRLEN(SUPPNT)
C
        FST = 1
        IF (FIELD(FST:FST).EQ.' ') THEN
          IF (DO2) THEN
            FIELD(FST:FST) = '0'
          ELSE
            FST = FST + 1
          END IF
        END IF
        FLEN = FMAX - FST + 1
C
C   Write output text string
C
        TMPNT = 2
        IF (SIGNF.EQ.'M' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TMP = ASIGN//FIELD(FST:FMAX)
        TLEN2 = (2 - TMPNT) + FLEN
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C
CCCC
C   Seconds field
CCCC
C
      IF (WRIT(4)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field
C 
        FIELD = ' '
        FST = 1
        IF (SPREC.GE.1) THEN
C
C   Fractional label.  Upto 3 places after the decimal point allowed
C   Muck around to get the superscript on top of the decimal point
C
          IR1 = INT(RVAL)
          IR2 = NINT((RVAL - IR1) * 10**SPREC)
          FRMAT = '(I2, A1, A, '//FRMAT2(SPREC)//')'
          WRITE (FIELD, FRMAT(1:15)) 
     *                       IR1, '.',
     *                       BSL//'b'//SUPER(4,SUPPNT)(1:TRLEN(SUPPNT)),
     *                       IR2
          IP = 5 + TRLEN(SUPPNT) + 1
          IF (FIELD(IP:IP).EQ.' ') FIELD(IP:IP) = '0'
          IF (FIELD(IP+1:IP+1).EQ.' ') FIELD(IP+1:IP+1) = '0'
          FMAX = 1 + 2 + SPREC
        ELSE
C
C   Integer label.  
C
          WRITE (FIELD, '(I2,A)') NINT(RVAL), 
     *                            SUPER(4,SUPPNT)(1:TRLEN(SUPPNT))
          FMAX = 0
        END IF
        FMAX = FMAX + 2 + TRLEN(SUPPNT)
C
        IF (FIELD(FST:FST).EQ.' ') THEN
          IF (DO2) THEN
            FIELD(FST:FST) = '0'
          ELSE
            FST = FST + 1
          END IF
        END IF
        FLEN = FMAX - FST + 1
C
C   Write output text string
C
        TMPNT = 2
        IF (SIGNF.EQ.'S' .AND. ASIGN.NE.' ') TMPNT = 1
        TMP = ASIGN//FIELD(FST:FMAX)
        TLEN2 = (3 - TMPNT) + FLEN
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C  
C   A trailing blank will occur if no superscripting wanted
C
      IF (TLEN.GE.5 .AND. TEXT(TLEN-4:TLEN).EQ.BSL//'u'//' '//BSL//'d')
     *   TLEN = TLEN - 5
C      
      RETURN
      END
C*PGTEXT -- write text (horizontal, left-justified)
C%void cpgtext(float x, float y, const char *text);
C+
      SUBROUTINE PGTEXT (X, Y, TEXT)
      REAL X, Y
      CHARACTER*(*) TEXT
C
C Write text. The bottom left corner of the first character is placed
C at the specified position, and the text is written horizontally.
C This is a simplified interface to the primitive routine PGPTXT.
C For non-horizontal text, use PGPTXT.
C
C Arguments:
C  X      (input)  : world x-coordinate of start of string.
C  Y      (input)  : world y-coordinate of start of string.
C  TEXT   (input)  : the character string to be plotted.
C--
C (2-May-1983)
C-----------------------------------------------------------------------
      CALL PGPTXT(X, Y, 0.0, 0.0, TEXT)
      END
C*PGTICK -- draw a single tick mark on an axis
C%void cpgtick(float x1, float y1, float x2, float y2, float v, \
C% float tikl, float tikr, float disp, float orient, const char *str);
C+
      SUBROUTINE PGTICK (X1, Y1, X2, Y2, V, TIKL, TIKR, DISP, 
     :                   ORIENT, STR)
      REAL X1, Y1, X2, Y2, V, TIKL, TIKR, DISP, ORIENT
      CHARACTER*(*) STR
C
C Draw and label single tick mark on a graph axis. The tick mark is
C a short line perpendicular to the direction of the axis (which is not
C drawn by this routine). The optional text label is drawn with its
C baseline parallel to the axis and reading in the same direction as
C the axis (from point 1 to point 2). Current line and text attributes
C are used.
C
C Arguments:
C  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
C  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
C  V      (input)  : draw the tick mark at fraction V (0<=V<=1) along
C                    the line from (X1,Y1) to (X2,Y2).
C  TIKL   (input)  : length of tick mark drawn to left of axis
C                    (as seen looking from first endpoint to second), in
C                    units of the character height.
C  TIKR   (input)  : length of major tick marks drawn to right of axis,
C                    in units of the character height.
C  DISP   (input)  : displacement of label text to
C                    right of axis, in units of the character height.
C  ORIENT (input)  : orientation of label text, in degrees; angle between
C                    baseline of text and direction of axis (0-360°).
C  STR    (input)  : text of label (may be blank).
C--
C 25-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL X, Y, XV1, XV2, YV1, YV2, XW1, XW2, YW1, YW2
      REAL XPMM, YPMM, LENMM, ANGLE, XCH, YCH
      REAL TIKX, TIKY, FJUST, D, OR
C
C Check arguments.
C
      IF (X1.EQ.X2 .AND. Y1.EQ.Y2) RETURN
C
C Get current character height (mm) [note: XCH = YCH].
C
      CALL PGQCS(2, XCH, YCH)
C
C Get x and y scales (units per mm).
C
      CALL PGQVP(2, XV1, XV2, YV1, YV2)
      CALL PGQWIN(XW1, XW2, YW1, YW2)
      XPMM  = (XW2-XW1)/(XV2-XV1)
      YPMM  = (YW2-YW1)/(YV2-YV1)
C
C Length of axis in mm.
C
      LENMM = SQRT(((X2-X1)/XPMM)**2 + ((Y2-Y1)/YPMM)**2)
C
C Angle of axis to horizontal (device coordinates).
C
      ANGLE = ATAN2((Y2-Y1)/YPMM, (X2-X1)/XPMM)*57.29577951
C
C (x,y) displacement for 1 character height perpendicular to axis.
C
      TIKX = (Y1-Y2)*XCH*XPMM/(LENMM*YPMM)
      TIKY = (X2-X1)*XCH*YPMM/(LENMM*XPMM)
C
C Draw the tick mark at point (X,Y) on the axis.
C
      X = X1 + V*(X2-X1)
      Y = Y1 + V*(Y2-Y1)
      CALL PGMOVE(X - TIKR*TIKX, Y - TIKR*TIKY)
      CALL PGDRAW(X + TIKL*TIKX, Y + TIKL*TIKY)
C
C Label the tick mark.
C
      D = DISP
      IF (STR.EQ.' ') RETURN
      OR = MOD(ORIENT, 360.0)
      IF (OR.LT.0.0) OR=OR+360.0
      IF (OR.GT.45.0 .AND. OR.LE.135.0) THEN
         FJUST = 0.0
         IF (D.LT.0.0) FJUST = 1.0
      ELSE IF (OR.GT.135.0 .AND. OR.LE.225.0) THEN
         FJUST = 0.5
         IF (D.LT.0.0) D = D-1.0
      ELSE IF (OR.GT.225.0 .AND. OR.LE.315.0) THEN
         ANGLE = ANGLE+90.0
         FJUST = 1.0
         IF (D.LT.0.0) FJUST = 0.0
      ELSE
         FJUST = 0.5
         IF (D.GT.0.0) D = D+1.0
      END IF            
      CALL PGPTXT(X-D*TIKX, Y-D*TIKY, ANGLE-OR, FJUST, STR)
      END
C.PGTIKL -- length of error bar terminal
C
      SUBROUTINE PGTIKL (T, XL, YL)
      REAL T, XL, YL
C
C Return the length of the terminal of an error bar, in world
C coordinates.
C
C Arguments:
C  T      (input)  : terminal multiplier
C  XL     (output) : terminal lnegth in world x-coordinates
C  YL     (output) : terminal lnegth in world y-coordinates
C--
C 31-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
C
      XL = T*PGXSP(PGID)*0.15/PGXSCL(PGID)
      YL = T*PGXSP(PGID)*0.15/PGYSCL(PGID)
C
      END
C*PGUPDT -- update display
C%void cpgupdt(void);
C+
      SUBROUTINE PGUPDT
C
C Update the graphics display: flush any pending commands to the
C output device. This routine empties the buffer created by PGBBUF,
C but it does not alter the PGBBUF/PGEBUF counter. The routine should
C be called when it is essential that the display be completely up to
C date (before interaction with the user, for example) but it is not
C known if output is being buffered.
C
C Arguments: none
C--
C 27-Nov-1986
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGUPDT')) RETURN
      CALL GRTERM
      END
C*PGVECT -- vector map of a 2D data array, with blanking
C%void cpgvect(const float *a, const float *b, int idim, int jdim, \
C% int i1, int i2, int j1, int j2, float c, int nc, \
C% const float *tr, float blank);
C+
      SUBROUTINE PGVECT (A, B, IDIM, JDIM, I1, I2, J1, J2, C, NC, TR,
     1                   BLANK)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, NC
      REAL    A(IDIM,JDIM), B(IDIM, JDIM), TR(6), BLANK, C
C
C Draw a vector map of two arrays.  This routine is similar to
C PGCONB in that array elements that have the "magic value" defined by
C the argument BLANK are ignored, making gaps in the vector map.  The
C routine may be useful for data measured on most but not all of the
C points of a grid. Vectors are displayed as arrows; the style of the
C arrowhead can be set with routine PGSAH, and the the size of the
C arrowhead is determined by the current character size, set by PGSCH.
C
C Arguments:
C  A      (input)  : horizontal component data array.
C  B      (input)  : vertical component data array.
C  IDIM   (input)  : first dimension of A and B.
C  JDIM   (input)  : second dimension of A and B.
C  I1,I2  (input)  : range of first index to be mapped (inclusive).
C  J1,J2  (input)  : range of second index to be mapped (inclusive).
C  C      (input)  : scale factor for vector lengths, if 0.0, C will be
C                    set so that the longest vector is equal to the
C                    smaller of TR(2)+TR(3) and TR(5)+TR(6).
C  NC     (input)  : vector positioning code.
C                    <0 vector head positioned on coordinates
C                    >0 vector base positioned on coordinates
C                    =0 vector centered on the coordinates
C  TR     (input)  : array defining a transformation between the I,J
C                    grid of the array and the world coordinates. The
C                    world coordinates of the array point A(I,J) are
C                    given by:
C                      X = TR(1) + TR(2)*I + TR(3)*J
C                      Y = TR(4) + TR(5)*I + TR(6)*J
C                    Usually TR(3) and TR(5) are zero - unless the
C                    coordinate transformation involves a rotation
C                    or shear.
C  BLANK   (input) : elements of arrays A or B that are exactly equal to
C                    this value are ignored (blanked).
C--
C  4-Sep-1992: derived from PGCONB [J. Crane].
C 26-Nov-1992: revised to use PGARRO [TJP].
C 25-Mar-1994: correct error for NC not =0 [G. Gonczi].
C  5-Oct-1996: correct error in computing max vector length [TJP;
C              thanks to David Singleton].
C-----------------------------------------------------------------------
      INTEGER  I, J
      REAL X, Y, X1, Y1, X2, Y2
      REAL CC
      INTRINSIC SQRT, MAX, MIN
C
C Define grid to world transformation
C
      X(I,J) = TR(1) + TR(2)*I + TR(3)*J
      Y(I,J) = TR(4) + TR(5)*I + TR(6)*J
C
C Check arguments.
C
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) THEN
C        CALL GRWARN('PGVECT: invalid range I1:I2, J1:J2')
         RETURN
      END IF
C
C Check for scale factor C.
C
      CC = C
      IF (CC.EQ.0.0) THEN
         DO 20 J=J1,J2
            DO 10 I=I1,I2
               IF (A(I,J).NE.BLANK .AND. B(I,J).NE.BLANK)
     1              CC = MAX(CC,SQRT(A(I,J)**2+B(I,J)**2))
 10         CONTINUE
 20      CONTINUE
         IF (CC.EQ.0.0) RETURN
         CC = SQRT(MIN(TR(2)**2+TR(3)**2,TR(5)**2+TR(6)**2))/CC
      END IF
C
      CALL PGBBUF
C
      DO 40 J=J1,J2
         DO 30 I=I1,I2
C
C Ignore vector if element of A and B are both equal to BLANK
C
            IF (.NOT.(A(I,J).EQ.BLANK .AND. B(I,J).EQ.BLANK)) THEN
 
C
C Define the vector starting and end points according to NC.
C
               IF (NC.LT.0) THEN
                  X2 = X(I,J)
                  Y2 = Y(I,J)
                  X1 = X2 - A(I,J)*CC
                  Y1 = Y2 - B(I,J)*CC
               ELSE IF (NC.EQ.0) THEN
                  X2 = X(I,J) + 0.5*A(I,J)*CC
                  Y2 = Y(I,J) + 0.5*B(I,J)*CC
                  X1 = X2 - A(I,J)*CC
                  Y1 = Y2 - B(I,J)*CC
               ELSE
                  X1 = X(I,J)
                  Y1 = Y(I,J)
                  X2 = X1 + A(I,J)*CC
                  Y2 = Y1 + B(I,J)*CC
               END IF
C     
C Draw vector.
C
               CALL PGARRO(X1, Y1, X2, Y2)
            END IF
 30      CONTINUE
 40   CONTINUE
C
      CALL PGEBUF
      END
C*PGVPORT -- non-standard alias for PGSVP
C+
      SUBROUTINE PGVPORT (XLEFT, XRIGHT, YBOT, YTOP)
      REAL XLEFT, XRIGHT, YBOT, YTOP
C
C See description of PGSVP.
C--
      CALL PGSVP (XLEFT, XRIGHT, YBOT, YTOP)
      END
C*PGVSIZ -- set viewport (inches)
C%void cpgvsiz(float xleft, float xright, float ybot, float ytop);
C+
      SUBROUTINE PGVSIZ (XLEFT, XRIGHT, YBOT, YTOP)
      REAL XLEFT, XRIGHT, YBOT, YTOP
C
C Change the size and position of the viewport, specifying
C the viewport in physical device coordinates (inches).  The
C viewport is the rectangle on the view surface "through"
C which one views the graph.  All the PG routines which plot lines
C etc. plot them within the viewport, and lines are truncated at
C the edge of the viewport (except for axes, labels etc drawn with
C PGBOX or PGLAB).  The region of world space (the coordinate
C space of the graph) which is visible through the viewport is
C specified by a call to PGSWIN.  It is legal to request a
C viewport larger than the view surface; only the part which
C appears on the view surface will be plotted.
C
C Arguments:
C  XLEFT  (input)  : x-coordinate of left hand edge of viewport, in
C                    inches from left edge of view surface.
C  XRIGHT (input)  : x-coordinate of right hand edge of viewport, in
C                    inches from left edge of view surface.
C  YBOT   (input)  : y-coordinate of bottom edge of viewport, in
C                    inches from bottom of view surface.
C  YTOP   (input)  : y-coordinate of top  edge of viewport, in inches
C                    from bottom of view surface.
C--
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
C
      IF (PGNOTO('PGVSIZ'))  RETURN
      IF (XLEFT.GE.XRIGHT .OR. YBOT.GE.YTOP) THEN
          CALL GRWARN('PGVSIZ ignored: invalid arguments')
          RETURN
      END IF
C
      PGXLEN(PGID) = (XRIGHT-XLEFT)*PGXPIN(PGID)
      PGYLEN(PGID) = (YTOP-YBOT)*PGYPIN(PGID)
      PGXVP(PGID)  = XLEFT*PGXPIN(PGID)
      PGYVP(PGID)  = YBOT*PGYPIN(PGID)
      PGXOFF(PGID) = PGXVP(PGID) + (PGNXC(PGID)-1)*PGXSZ(PGID)
      PGYOFF(PGID) = PGYVP(PGID) + 
     1                (PGNY(PGID)-PGNYC(PGID))*PGYSZ(PGID)
      CALL PGVW
      END
C*PGVSIZE -- non-standard alias for PGVSIZ
C+
      SUBROUTINE PGVSIZE (XLEFT, XRIGHT, YBOT, YTOP)
      REAL XLEFT, XRIGHT, YBOT, YTOP
C
C See description of PGVSIZ.
C--
      CALL PGVSIZ (XLEFT, XRIGHT, YBOT, YTOP)
      END
C*PGVSTAND -- non-standard alias for PGVSTD
C+
      SUBROUTINE PGVSTAND
C
C See description of PGVSTD.
C--
      CALL PGVSTD
      END
C*PGVSTD -- set standard (default) viewport
C%void cpgvstd(void);
C+
      SUBROUTINE PGVSTD
C
C Define the viewport to be the standard viewport.  The standard
C viewport is the full area of the view surface (or panel),
C less a margin of 4 character heights all round for labelling.
C It thus depends on the current character size, set by PGSCH.
C
C Arguments: none.
C--
C 22-Apr-1983: [TJP].
C  2-Aug-1995: [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     XLEFT, XRIGHT, YBOT, YTOP, R
C
      IF (PGNOTO('PGVSIZ')) RETURN
C
      R = 4.0*PGYSP(PGID)
      XLEFT  = R/PGXPIN(PGID)
      XRIGHT = XLEFT + (PGXSZ(PGID)-2.0*R)/PGXPIN(PGID)
      YBOT   = R/PGYPIN(PGID)
      YTOP   = YBOT + (PGYSZ(PGID)-2.0*R)/PGYPIN(PGID)
      CALL PGVSIZ(XLEFT, XRIGHT, YBOT, YTOP)
      END
C
      SUBROUTINE PGVW
C
C PGPLOT (internal routine): set the GRPCKG scaling transformation
C and window appropriate for the current window and viewport. This
C routine is called whenever the viewport or window is changed.
C
C Arguments: none
C
C (11-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
C Scale plotter in world coordinates.
C
      PGXSCL(PGID) = PGXLEN(PGID)/ABS(PGXTRC(PGID)-PGXBLC(PGID))
      PGYSCL(PGID) = PGYLEN(PGID)/ABS(PGYTRC(PGID)-PGYBLC(PGID))
      IF (PGXBLC(PGID).GT.PGXTRC(PGID)) THEN
          PGXSCL(PGID) = -PGXSCL(PGID)
      END IF
      IF (PGYBLC(PGID).GT.PGYTRC(PGID)) THEN
          PGYSCL(PGID) = -PGYSCL(PGID)
      END IF
      PGXORG(PGID) = PGXOFF(PGID)-PGXBLC(PGID)*PGXSCL(PGID)
      PGYORG(PGID) = PGYOFF(PGID)-PGYBLC(PGID)*PGYSCL(PGID)
      CALL GRTRN0(PGXORG(PGID),PGYORG(PGID),
     1            PGXSCL(PGID),PGYSCL(PGID))
C
C Window plotter in viewport.
C
      CALL GRAREA(PGID,PGXOFF(PGID),PGYOFF(PGID),
     1            PGXLEN(PGID),PGYLEN(PGID))
      END
C*PGWEDG -- annotate an image plot with a wedge
C%void cpgwedg(const char *side, float disp, float width, \
C% float fg, float bg, const char *label);
C+
      SUBROUTINE PGWEDG(SIDE, DISP, WIDTH, FG, BG, LABEL)
      CHARACTER *(*) SIDE,LABEL
      REAL DISP, WIDTH, FG, BG
C
C Plot an annotated grey-scale or color wedge parallel to a given axis
C of the the current viewport. This routine is designed to provide a
C brightness/color scale for an image drawn with PGIMAG or PGGRAY.
C The wedge will be drawn with the transfer function set by PGSITF
C and using the color index range set by PGSCIR.
C
C Arguments:
C  SIDE   (input)  : The first character must be one of the characters
C                    'B', 'L', 'T', or 'R' signifying the Bottom, Left,
C                    Top, or Right edge of the viewport.
C                    The second character should be 'I' to use PGIMAG
C                    to draw the wedge, or 'G' to use PGGRAY.
C  DISP   (input)  : the displacement of the wedge from the specified
C                    edge of the viewport, measured outwards from the
C                    viewport in units of the character height. Use a
C                    negative value to write inside the viewport, a
C                    positive value to write outside.
C  WIDTH  (input)  : The total width of the wedge including annotation,
C                    in units of the character height.
C  FG     (input)  : The value which is to appear with shade
C                    1 ("foreground"). Use the values of FG and BG
C                    that were supplied to PGGRAY or PGIMAG.
C  BG     (input)  : the value which is to appear with shade
C                    0 ("background").
C  LABEL  (input)  : Optional units label. If no label is required
C                    use ' '.
C--
C  15-Oct-1992: New routine (MCS)
C   2-Aug-1995: no longer needs common (TJP).
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C                                        Temporary window coord storage.
      REAL WXA,WXB,WYA,WYB, XA,XB,YA,YB
C                                        Viewport coords of wedge.
      REAL VXA,VXB,VYA,VYB
C                          Original and anotation character heights.
      REAL OLDCH, NEWCH
C                          Size of unit character height (NDC units).
      REAL NDCSIZ
C                          True if wedge plotted horizontally.
      LOGICAL HORIZ
C                          Use PGIMAG (T) or PGGRAY (F).
      LOGICAL IMAGE
C                          Symbolic version of SIDE.
      INTEGER NSIDE,BOT,TOP,LFT,RGT
      PARAMETER (BOT=1,TOP=2,LFT=3,RGT=4)
      INTEGER I
      REAL WEDWID, WDGINC, VWIDTH, VDISP, XCH, YCH, LABWID, FG1, BG1
C                          Set the fraction of WIDTH used for anotation.
      REAL TXTFRC
      PARAMETER (TXTFRC=0.6)
C                          Char separation between numbers and LABEL.
      REAL TXTSEP
      PARAMETER (TXTSEP=2.2)
C                          Array to draw wedge in.
      INTEGER WDGPIX
      PARAMETER (WDGPIX=100)
      REAL WDGARR(WDGPIX)
C                          Define the coordinate-mapping function.
      REAL TR(6)
      SAVE TR
      DATA TR /0.0,1.0,0.0,0.0,0.0,1.0/
C-----------------------------------------------------------------------
      IF(PGNOTO('PGWEDG')) RETURN
C
C Get a numeric version of SIDE.
C
      IF(SIDE(1:1).EQ.'B' .OR. SIDE(1:1).EQ.'b') THEN
        NSIDE = BOT
        HORIZ = .TRUE.
      ELSE IF(SIDE(1:1).EQ.'T' .OR. SIDE(1:1).EQ.'t') THEN
        NSIDE = TOP
        HORIZ = .TRUE.
      ELSE IF(SIDE(1:1).EQ.'L' .OR. SIDE(1:1).EQ.'l') THEN
        NSIDE = LFT
        HORIZ = .FALSE.
      ELSE IF(SIDE(1:1).EQ.'R' .OR. SIDE(1:1).EQ.'r') THEN
        NSIDE = RGT
        HORIZ = .FALSE.
      ELSE
        CALL GRWARN('Invalid "SIDE" argument in PGWEDG.')
        RETURN
      END IF
C
C Determine which routine to use.
C
      IF (LEN(SIDE).LT.2) THEN
         IMAGE = .FALSE.
      ELSE IF(SIDE(2:2).EQ.'I' .OR. SIDE(2:2).EQ.'i') THEN
         IMAGE = .TRUE.
      ELSE IF(SIDE(2:2).EQ.'G' .OR. SIDE(2:2).EQ.'g') THEN
         IMAGE = .FALSE.
      ELSE
         CALL GRWARN('Invalid "SIDE" argument in PGWEDG.')
      END IF
C
      CALL PGBBUF
C
C Store the current world and viewport coords and the character height.
C
      CALL PGQWIN(WXA, WXB, WYA, WYB)
      CALL PGQVP(0, XA, XB, YA, YB)
      CALL PGQCH(OLDCH)
C
C Determine the unit character height in NDC coords.
C
      CALL PGSCH(1.0)
      CALL PGQCS(0, XCH, YCH)
      IF(HORIZ) THEN
        NDCSIZ = YCH
      ELSE
        NDCSIZ = XCH
      END IF
C
C Convert 'WIDTH' and 'DISP' into viewport units.
C
      VWIDTH = WIDTH * NDCSIZ * OLDCH
      VDISP  = DISP * NDCSIZ * OLDCH
C
C Determine the number of character heights required under the wedge.
C
      LABWID = TXTSEP
      IF(LABEL.NE.' ') LABWID = LABWID + 1.0
C
C Determine and set the character height required to fit the wedge
C anotation text within the area allowed for it.
C
      NEWCH = TXTFRC*VWIDTH / (LABWID*NDCSIZ)
      CALL PGSCH(NEWCH)
C
C Determine the width of the wedge part of the plot minus the anotation.
C (NDC units).
C
      WEDWID = VWIDTH * (1.0-TXTFRC)
C
C Use these to determine viewport coordinates for the wedge + annotation.
C
      VXA = XA
      VXB = XB
      VYA = YA
      VYB = YB
      IF(NSIDE.EQ.BOT) THEN
        VYB = YA - VDISP
        VYA = VYB - WEDWID
      ELSE IF(NSIDE.EQ.TOP) THEN
        VYA = YB + VDISP
        VYB = VYA + WEDWID
      ELSE IF(NSIDE.EQ.LFT) THEN
        VXB = XA - VDISP
        VXA = VXB - WEDWID
      ELSE IF(NSIDE.EQ.RGT) THEN
        VXA = XB + VDISP
        VXB = VXA + WEDWID
      END IF
C
C Set the viewport for the wedge.
C
      CALL PGSVP(VXA, VXB, VYA, VYB)
C
C Swap FG/BG if necessary to get axis direction right.
C
      FG1 = MAX(FG,BG)
      BG1 = MIN(FG,BG)
C
C Create a dummy wedge array to be plotted.
C
      WDGINC = (FG1-BG1)/(WDGPIX-1)
      DO 1 I=1,WDGPIX
        WDGARR(I) = BG1 + (I-1) * WDGINC
 1    CONTINUE
C
C Draw the wedge then change the world coordinates for labelling.
C
      IF (HORIZ) THEN
        CALL PGSWIN(1.0, REAL(WDGPIX), 0.9, 1.1)
        IF (IMAGE) THEN
           CALL PGIMAG(WDGARR, WDGPIX,1, 1,WDGPIX, 1,1, FG,BG, TR)
        ELSE
           CALL PGGRAY(WDGARR, WDGPIX,1, 1,WDGPIX, 1,1, FG,BG, TR)
        END IF
        CALL PGSWIN(BG1,FG1,0.0,1.0)
      ELSE
        CALL PGSWIN(0.9, 1.1, 1.0, REAL(WDGPIX))
        IF (IMAGE) THEN
           CALL PGIMAG(WDGARR, 1,WDGPIX, 1,1, 1,WDGPIX, FG,BG, TR)
        ELSE
           CALL PGGRAY(WDGARR, 1,WDGPIX, 1,1, 1,WDGPIX, FG,BG, TR)
        END IF
        CALL PGSWIN(0.0, 1.0, BG1, FG1)
      ENDIF
C
C Draw a labelled frame around the wedge.
C
      IF(NSIDE.EQ.BOT) THEN
        CALL PGBOX('BCNST',0.0,0,'BC',0.0,0)
      ELSE IF(NSIDE.EQ.TOP) THEN
        CALL PGBOX('BCMST',0.0,0,'BC',0.0,0)
      ELSE IF(NSIDE.EQ.LFT) THEN
        CALL PGBOX('BC',0.0,0,'BCNST',0.0,0)
      ELSE IF(NSIDE.EQ.RGT) THEN
        CALL PGBOX('BC',0.0,0,'BCMST',0.0,0)
      ENDIF
C
C Write the units label.
C
      IF(LABEL.NE.' ') THEN
        CALL PGMTXT(SIDE,TXTSEP,1.0,1.0,LABEL)
      END IF
C
C Reset the original viewport and world coordinates.
C
      CALL PGSVP(XA,XB,YA,YB)
      CALL PGSWIN(WXA,WXB,WYA,WYB)
      CALL PGSCH(OLDCH)
      CALL PGEBUF
      RETURN
      END
C*PGWINDOW -- non-standard alias for PGSWIN
C+
      SUBROUTINE PGWINDOW (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C See description of PGSWIN.
C--
      CALL PGSWIN (X1, X2, Y1, Y2)
      END
C*PGWNAD -- set window and adjust viewport to same aspect ratio
C%void cpgwnad(float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGWNAD (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Change the window in world coordinate space that is to be mapped on
C to the viewport, and simultaneously adjust the viewport so that the
C world-coordinate scales are equal in x and y. The new viewport is
C the largest one that can fit within the previously set viewport
C while retaining the required aspect ratio.
C
C Arguments:
C  X1     (input)  : the x-coordinate of the bottom left corner
C                    of the viewport.
C  X2     (input)  : the x-coordinate of the top right corner
C                    of the viewport (note X2 may be less than X1).
C  Y1     (input)  : the y-coordinate of the bottom left corner
C                    of the viewport.
C  Y2     (input)  : the y-coordinate of the top right corner of the
C                    viewport (note Y2 may be less than Y1).
C--
C 25-Sep-1985 - new routine (TJP).
C 31-May-1989 - correct error: XVP and YVP not set (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL SCALE,OXLEN,OYLEN
C
      IF (PGNOTO('PGWNAD')) RETURN
C
C If invalid arguments are specified, issue warning and leave window
C unchanged.
C
      IF (X1.EQ.X2) THEN
         CALL GRWARN('invalid x limits in PGWNAD: X1 = X2.')
      ELSE IF (Y1.EQ.Y2) THEN
         CALL GRWARN('invalid y limits in PGWNAD: Y1 = Y2.')
      ELSE
         SCALE = MIN(PGXLEN(PGID)/ABS(X2-X1)/PGXPIN(PGID), 
     1               PGYLEN(PGID)/ABS(Y2-Y1)/PGYPIN(PGID))
         PGXSCL(PGID) = SCALE*PGXPIN(PGID)
         PGYSCL(PGID) = SCALE*PGYPIN(PGID)
         OXLEN = PGXLEN(PGID)
         OYLEN = PGYLEN(PGID)
         PGXLEN(PGID) = PGXSCL(PGID)*ABS(X2-X1)
         PGYLEN(PGID) = PGYSCL(PGID)*ABS(Y2-Y1)
         PGXVP(PGID)  = PGXVP(PGID) + 0.5*(OXLEN-PGXLEN(PGID))
         PGYVP(PGID)  = PGYVP(PGID) + 0.5*(OYLEN-PGYLEN(PGID))
         PGXOFF(PGID) = PGXVP(PGID) + (PGNXC(PGID)-1)*PGXSZ(PGID)
         PGYOFF(PGID) = PGYVP(PGID) +
     1                   (PGNY(PGID)-PGNYC(PGID))*PGYSZ(PGID)
         CALL PGSWIN(X1, X2, Y1, Y2)
      END IF
      END
C*LXDRIV -- PGPLOT driver for LaTeX Picture Environment
C+
      SUBROUTINE LXDRIV(OPCODE,RBUF,NBUF,CHR,LCHR)
      INTEGER OPCODE, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C Supported device:
C  This driver creates a text file containing commands for drawing
C  in the LaTeX picture environment, bracketted by \begin{picture} 
C  and \end{picture}. The file can be included in a LaTeX document.
C
C  If you have the option of including a PostScript file in your
C  LaTeX document, then that will usually give much better results
C  than using this driver, which has very limited capabilities.
C
C Device type code:
C  /LATEX
C
C Default file name:
C  pgplot.tex
C
C Default view surface dimensions:
C  The default picture size is 6 inches by 6 inches (which corresponds 
C  to 1728x1728 units where a unit is 0.25pt = 1/288 inch). The picture
C  size can be changed by using PGPAP in the PGPLOT program.
C
C Resolution:
C  The driver rounds coordinates to multiples of 0.25pt (1/288 inch).
C
C Limitations:
C  The LaTeX picture environment has a very limited set of primitives.
C  In particular, diagonal lines must be composed out of dots. This
C  can lead to very large files. For some graphs (especially with a 
C  lot of shaded areas), the capacity of many LaTeX systems can easily
C  be exceeded.
C
C Author:
C  Written by Grant McIntosh  95/02/14 (gmcint@relay.drev.dnd.ca).
C
C  Revised by T. Pearson 95/06/19.
C  Revised to allow picture size to be adjusted by PGPAP: TJP 97/5/16.
C-----------------------------------------------------------------------
      INTEGER LUN, IXO, IYO, IXPS, IYPS, I, J
      INTEGER INCR, NINC, IER, ISIGN, LENGTH, BX, BY, STATE
      INTEGER GROPTX
      REAL X1, Y1, X2, Y2, DELX, DELY, SLOPE
      CHARACTER*128 MSG
      CHARACTER*(*) DEVNAM
      PARAMETER (DEVNAM='LATEX (LaTeX picture environment)')
      CHARACTER*1   BS
      SAVE LUN, BS, BX, BY, STATE
C-----------------------------------------------------------------------
      GOTO(10,20,30,40,50,60,70,80,90,100,110,120,130,140)OPCODE
      NBUF=-1
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 1, Return device name -------------------------------------
   10 CHR=DEVNAM
      LCHR=LEN(DEVNAM)
      BS=CHAR(92)
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
   20 RBUF(1)=0
      RBUF(2)=32767
      RBUF(3)=0
      RBUF(4)=32767
      RBUF(5)=0
      RBUF(6)=1
      NBUF=6
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 3, Return device resolution -------------------------------
   30 RBUF(1)=72./0.25
      RBUF(2)=72./0.25
      RBUF(3)=1.
      NBUF=3
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 4, Return misc device info --------------------------------
   40 CHR='HNNNNNNNNN'
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 5, Return default file name -------------------------------
   50 CHR='pgplot.tex'
      LCHR=10
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 6, Return default physical size of plot -------------------
   60 RBUF(1)=0
      RBUF(2)=BX
      RBUF(3)=0
      RBUF(4)=BY
      NBUF=4
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 7, Return misc defaults -----------------------------------
   70 RBUF(1)=1
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 8, Select plot --------------------------------------------
 80   RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 9, Open workstation ---------------------------------------
 90   CONTINUE
      NBUF=2
C     -- check for concurrent access
      IF (STATE.EQ.1) THEN
         CALL GRWARN('a PGPLOT LaTeX file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
      CALL GRGLUN(LUN)
      IER = GROPTX(LUN, CHR(1:LCHR), 'pgplot.tex', 1)
      IF (IER.NE.0) THEN
         MSG = 'Cannot open output file for LaTeX picture: '//
     :         CHR(:LCHR)
         CALL GRWARN(MSG)
         RBUF(1)=0
         RBUF(2)=0
         CALL GRFLUN(LUN)
      ELSE
         RBUF(2)=1
         RBUF(1)=LUN
         STATE=1
         BX=1728
         BY=1728
      END IF
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=10, Close workstation ---------------------------------------
 100  CLOSE(UNIT=LUN)
      CALL GRFLUN(LUN)
      STATE=0
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=11, Begin picture -------------------------------------------
  110 CONTINUE
      BX = NINT(RBUF(1))
      BY = NINT(RBUF(2))
      WRITE(LUN,'(A)') BS//'setlength{'//BS//'unitlength}{0.25pt}'
      WRITE(LUN,'(A)') BS//'linethickness{1pt}'
      WRITE(LUN,'(A,I6,A,I6,A)')
     :     BS//'begin{picture}(',BX,',',BY,')(0,0)'
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=12, Draw line -----------------------------------------------
  120 X1=RBUF(1)
      Y1=RBUF(2)
      X2=RBUF(3)
      Y2=RBUF(4)
      IXO=X1
      IYO=Y1
      IXPS=X2
      IYPS=Y2
C vertical lines
      IF(IXPS.EQ.IXO) THEN
         LENGTH=ABS(IYPS-IYO)
         ISIGN=1
         IF(LENGTH.NE.0) ISIGN=(IYPS-IYO)/LENGTH
         WRITE(LUN,5000) BS,IXO,IYO,BS,ISIGN,LENGTH
 5000    FORMAT(A1,'put(',I4,',',I4,'){',A1,'line(0,',I4,'){',I4,'}}')
         RETURN
      ENDIF
C horizontal lines
      IF(IYPS.EQ.IYO) THEN
         LENGTH=ABS(IXPS-IXO)
         ISIGN=1
         IF(LENGTH.NE.0) ISIGN=(IXPS-IXO)/LENGTH
         WRITE(LUN,5100) BS,IXO,IYO,BS,ISIGN,LENGTH
 5100    FORMAT(A1,'put(',I4,',',I4,'){',A1,'line(',I4,',0){',I4,'}}')
         RETURN
      ENDIF
C other lines
      SLOPE=FLOAT(IYPS-IYO)/FLOAT(IXPS-IXO)
      INCR=1
      IF(IXPS.LT.IXO) INCR=-1
      NINC=MAX(1,ABS(IXPS-IXO))
      DELX=INCR
      DELY=SLOPE*INCR
  125 CONTINUE
      IF(ABS(DELY).GT.1) THEN
         NINC=NINC*2
         DELX=DELX/2.
         DELY=SLOPE*DELX
         GOTO 125
      ENDIF
      WRITE(LUN,5200) BS,IXO,IYO,DELX,DELY,NINC,BS
 5200 FORMAT(A1,'multiput(',I4,',',I4,')(',F8.3,',',F8.3,'){',I4,
     *     '}{',A1,'circle*{1}}')
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=13, Draw dot ------------------------------------------------
  130 I=NINT(RBUF(1))
      J=NINT(RBUF(2))
      WRITE(LUN,5300) BS,I,J,BS
 5300 FORMAT(A1,'put(',I4,',',I4,'){',A1,'circle*{1}}')
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=14, End picture ---------------------------------------------
  140 WRITE(LUN,'(A)') BS//'end{picture}'
      RETURN
C-----------------------------------------------------------------------
      END
C*NUDRIV -- PGPLOT Null device driver
C+
      SUBROUTINE NUDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Null device (no graphical output)
C
C Version 1.0  - 1987 May 26 - T. J. Pearson.
C Version 1.1  - 1988 Mar 23 - add rectangle fill.
C Version 1.2  - 1992 Sep  3 - add line-of-pixels.
C Version 1.3  - 1992 Sep 21 - add markers.
C Version 1.4  - 1993 Apr 22 - add optional debugging.
C Version 1.5  - 1994 Aug 31 - use image primitives.
C Version 2.0  - 1996 Jan 22 - allow multiple active devices;
C                              add QCR primitive.
C Version 2.1  - 1997 Jun 13 - correctly initialize STATE.
C
C Supported device: The ``null'' device can be used to suppress
C all graphic output from a program.  If environment variable
C PGPLOT_DEBUG is defined, some debugging information is
C reported on standard output.
C
C Device type code: /NULL.
C
C Default device name: None (the device name, if specified, is 
C ignored).
C
C Default view surface dimensions: Undefined (The device pretends to
C be a hardcopy device with 1000 pixels/inch and a view surface 8in 
C high by 10.5in wide.)
C
C Resolution: Undefined.
C
C Color capability: Color indices 0--255 are accepted.
C
C Input capability: None.
C
C File format: None.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
C Notes:
C  Up to MAXDEV "devices" may be open at once. ACTIVE is the number
C  of the currently selected device, or 0 if no devices are open.
C  STATE(i) is 0 if device i is not open, 1 if it is open but with
C  no current picture, or 2 if it is open with a current picture.
C
C  When debugging is enabled, open/close device and begin/end picture
C  calls are reported on stdout, and a cumulative count of all
C  driver calls is kept.
C-----------------------------------------------------------------------
      CHARACTER*(*) DEVICE
      PARAMETER (DEVICE='NULL  (Null device, no output)')
      INTEGER MAXDEV, MAXD1
      PARAMETER (MAXDEV=8)
      PARAMETER (MAXD1=MAXDEV+1)
      INTEGER NOPCOD
      PARAMETER (NOPCOD=29)
      CHARACTER*10 MSG
      CHARACTER*32 TEXT
      CHARACTER*8  LAB(NOPCOD)
      INTEGER COUNT(NOPCOD), I, STATE(0:MAXDEV), L, NPIC(MAXDEV)
      INTEGER ACTIVE
      LOGICAL DEBUG
      INTEGER CTABLE(3,0:255), CDEFLT(3,0:15)
      SAVE COUNT, STATE, NPIC, DEBUG, CTABLE, CDEFLT, ACTIVE
C
      DATA ACTIVE/-1/
      DATA STATE/MAXD1*0/
      DATA COUNT/NOPCOD*0/
      DATA DEBUG/.FALSE./
      DATA LAB  /'qdev    ', 'qmaxsize', 'qscale  ', 'qcapab  ',
     1           'qdefnam ', 'qdefsize', 'qmisc   ', 'select  ',
     2           'open    ', 'close   ', 'beginpic', 'line    ',
     3           'dot     ', 'endpic  ', 'set CI  ', 'flush   ',
     4           'cursor  ', 'eralpha ', 'set LS  ', 'polygon ',
     5           'set CR  ', 'set LW  ', 'escape  ', 'rectangl',
     6           'set patt', 'pix/imag', 'scaling ', 'marker  ',
     7           'query CR'/
      DATA CDEFLT /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
C-----------------------------------------------------------------------
C
      IF (ACTIVE.EQ.-1) THEN
           CALL GRGENV('DEBUG', TEXT, L)
           DEBUG = L.GT.0
           ACTIVE = 0
      END IF
C
      IF (IFUNC.LT.1 .OR. IFUNC.GT.NOPCOD) GOTO 900
      COUNT(IFUNC) = COUNT(IFUNC) + 1
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260,270,280,290), IFUNC
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN('Unimplemented function in NULL device driver: '//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = DEVICE
      LCHR = LEN(DEVICE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 65535
      RBUF(3) = 0
      RBUF(4) = 65535
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, Dashed lines, Area fill, Thick
C    lines, Rectangle fill, Images, , , Markers, query color rep)
C
   40 CHR = 'HNDATRQNYM'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = 'NL:'
      LCHR = 3
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 10499
      RBUF(3) = 0
      RBUF(4) = 7999
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 1
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      I = RBUF(2) - 67890
      IF (I.LT.1 .OR. I.GT.MAXDEV) THEN
         CALL GRWARN('internal error: NULL opcode 8')
      ELSE IF (STATE(I).GT.0) THEN
         ACTIVE = I
      ELSE
         CALL GRNU00(IFUNC,0)
      END IF
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
C     -- Find an inactive device, and select it
      DO 91 I=1,MAXDEV
         IF (STATE(I).EQ.0) THEN
            ACTIVE = I
            STATE(ACTIVE) = 1
            GOTO 92
         END IF
 91   CONTINUE
      IF (DEBUG) CALL GRWARN ('09 Open workstation')
      CALL GRWARN('maximum number of devices of type NULL exceeded')
      RBUF(1) = 0
      RBUF(2) = 0 
      NBUF = 2
      RETURN
C     -- Initialize the new device
 92   CONTINUE
      RBUF(1) = ACTIVE + 67890
      RBUF(2) = 1
      NBUF = 2
      NPIC(ACTIVE) = 0
C     -- Initialize color table
      DO 95 I=0,15
         CTABLE(1,I) = CDEFLT(1,I)
         CTABLE(2,I) = CDEFLT(2,I)
         CTABLE(3,I) = CDEFLT(3,I)
 95   CONTINUE
      DO 96 I=16,255
         CTABLE(1,I) = 128
         CTABLE(2,I) = 128
         CTABLE(3,I) = 128
 96   CONTINUE
      IF (DEBUG) THEN
         CALL GRFAO('09 Open workstation: device #',
     :        L, TEXT, ACTIVE, 0, 0, 0)
         CALL GRWARN(TEXT(1:L))
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      IF (STATE(ACTIVE).NE.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      STATE(ACTIVE) = 0
      IF (DEBUG) THEN
         CALL GRFAO('10 Close workstation: device #',
     :        L, TEXT, ACTIVE, 0, 0, 0)
         CALL GRWARN(TEXT(1:L))
         CALL GRWARN('Device driver calls:')
         DO 101 I=1,NOPCOD
            IF (COUNT(I).GT.0) THEN
               WRITE (TEXT,'(3X,I2,1X,A8,I10)') I, LAB(I), COUNT(I)
               CALL GRWARN(TEXT)
            END IF
 101     CONTINUE
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      IF (STATE(ACTIVE).NE.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      STATE(ACTIVE) = 2
      NPIC(ACTIVE) = NPIC(ACTIVE)+1
      IF (DEBUG) THEN
         CALL GRFAO('11   Begin picture # on device #',
     :        L, TEXT, NPIC(ACTIVE), ACTIVE, 0,0)
         CALL GRWARN(TEXT(:L))
      END IF
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      STATE(ACTIVE) = 1
      IF (DEBUG) THEN
         CALL GRFAO('14   End picture   # on device #',
     :        L, TEXT, NPIC(ACTIVE), ACTIVE, 0,0)
         CALL GRWARN(TEXT(:L))
      END IF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C
  190 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      I = RBUF(1)
      CTABLE(1, I) = NINT(RBUF(2)*255)
      CTABLE(2, I) = NINT(RBUF(3)*255)
      CTABLE(3, I) = NINT(RBUF(4)*255)
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      IF (DEBUG.AND.STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=25, Not implemented -----------------------------------------
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels ------------------------------------------
C
  260 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=27, Scaling info -- -----------------------------------------
C
  270 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=28, Draw marker ---------------------------------------------
C
  280 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
C     WRITE (*,'(1X,A,I4,1X,3F10.1)') 'MARKER', NINT(RBUF(1)), RBUF(2),
C    1      RBUF(3), RBUF(4)
      RETURN
C
C--- IFUNC=29, Query color representation. -----------------------------
C
  290 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      I = RBUF(1)
      RBUF(2) = CTABLE(1,I)/255.0
      RBUF(3) = CTABLE(2,I)/255.0
      RBUF(4) = CTABLE(3,I)/255.0
      NBUF = 4
      RETURN
C-----------------------------------------------------------------------
      END

      SUBROUTINE GRNU00(IFUNC, STATE)
      INTEGER IFUNC, STATE
C
C PGPLOT NULL device driver: report error
C-----------------------------------------------------------------------
      INTEGER L
      CHARACTER*80 MSG
C
      CALL GRFAO('++ internal error: driver in state # for opcode #',
     :           L, MSG, STATE, IFUNC, 0, 0)
      CALL GRWARN(MSG(1:L))
      RETURN
      END
C*PSDRIV -- PGPLOT PostScript drivers
C+
      SUBROUTINE PSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for PostScript devices.
C
C Version 1.2  - 1987 Aug  5 - T. J. Pearson.
C Version 1.3  - 1987 Nov 16 - add "bind" commands to prolog - TJP.
C Version 1.4  - 1988 Jan 28 - change dimensions so whole field can be
C                              plotted - TJP.
C Version 1.5  - 1988 Oct 27 - make EOF characters optional - TJP.
C Version 1.6  - 1988 Dec 15 - standard Fortran - TJP.
C Version 1.7  - 1989 Jul  5 - change color indices so most colors
C                              are black - TJP.
C Version 2.0  - 1990 Sep 10 - parameterize dimensions; correct
C                              bounding box; add color support (from
C                              D. Meier's CPdriver) - TJP.
C Version 2.1  - 1991 Nov 29 - update Document Structuring Conventions
C                              to version 3.0.
C Version 3.0  - 1992 Sep 22 - add marker support; add CPS and VCPS
C                              modes - TJP.
C Version 3.1  - 1992 Nov 12 - up to 256 colors.
C Version 3.2  - 1993 May 26 - correct error in marker support.
C Version 4.0  - 1993 Sep 20 - trap Fortran I/O errors.
C Version 4.1  - 1994 Aug  4 - make marker support optional.
C Version 5.0  - 1994 Aug 30 - support for images.
C Version 5.1  - 1994 Sep  7 - support for PGQCR.
C Version 5.2  - 1994 Oct 12 - add IDENT option.
C Version 5.3  - 1995 May  8 - recognise '-' as standard output; keep
C                              track of bounding box; use upper case
C                              for all defined commands; move
C                              showpage outside save/restore.
C Version 5.4  - 1995 Aug 19 - correct usage of PS_BBOX.
C Version 6.0  - 1995 Dec 28 - reject concurrent access.
C Version 6.1  - 1996 Apr 29 - decode environment variables using GRCTOI.
C Version 6.2  - 1996 Oct  7 - correct bounding-box error (K-G Adams);
C                              correct error in use of GCTOI (G Gonczi);
C                              suppress <0 0 C> commands (R Scharroo);
C                              allow arbitrary page size.
C
C Supported device: 
C   Any printer that accepts the PostScript page description language, 
C   eg, the LaserWriter (Apple Computer, Inc.).
C   PostScript is a trademark of Adobe Systems Incorporated.
C
C Device type code: 
C   /PS (monochrome landscape mode, long edge of paper horizontal).
C   /CPS (color landscape mode, long edge of paper horizontal).
C   /VPS (monochrome portrait mode, short edge of paper horizontal).
C   /VCPS (color portrait mode, short edge of paper horizontal).
C
C Default file name:
C   pgplot.ps
C
C Default view surface dimensions:
C   10.5 inches horizontal x  7.8 inches vertical (landscape mode),
C    7.8 inches horizontal x 10.5 inches vertical (portrait mode).
C   These dimensions can be changed with environment variables.
C
C Resolution:
C   The driver uses coordinate increments of 0.001 inch, giving an
C   ``apparent'' resolution of 1000 pixels/inch. The true resolution is
C   device-dependent; eg, on an Apple LaserWriter it is 300 pixels/inch
C   (in both dimensions). 
C
C Color capability (monochrome mode): 
C   Color indices 0-255 are supported. Color index 0 is white (erase
C   or background color), indices 1-13 are black, 14 is light grey,
C   and 15 is dark grey.
C
C Color capability (color mode):
C   Color indices 0-255 are supported. Color index 0 is white (erase
C   or background color), index 1 is black, and indices 2-15 have the
C   standard PGPLOT color assignments.
C
C Input capability: none.
C
C File format: the file contains variable length records (maximum 132
C characters) containing PostScript commands. The commands use only
C printable ASCII characters, and the file can be examined or modified 
C with a text editor. 
C
C Obtaining hardcopy: use the operating system print or copy command to
C send the file to a suitable device.
C
C Environment variables:
C
C  PGPLOT_PS_WIDTH      default  7800
C  PGPLOT_PS_HEIGHT     default 10500
C  PGPLOT_PS_HOFFSET    default   350
C  PGPLOT_PS_VOFFSET    default   250
C These variables tell PGPLOT how big an image to produce. The defaults
C are appropriate for 8.5 x 11-inch paper. The maximum dimensions of
C a PGPLOT image are WIDTH by HEIGHT, with the lower left corner offset
C by HOFFSET horizontally and VOFFSET vertically from the lower left
C corner of the paper. The units are milli-inches. The "top" of the
C paper is the edge that comes out of the printer first.
C
C  PGPLOT_IDENT
C If this variable is set, the user name, date and time are written
C in the bottom right corner of each page.
C
C  PGPLOT_PS_BBOX
C If this variable has value MAX, PGPLOT puts standard (full-page)
C bounding-box information in the header of the PostScript file. If
C the variable is unset or has some other value, PGPLOT puts the
C correct (smallest) bounding box information in the trailer of the
C PostScript file.
C
C  PGPLOT_PS_EOF
C Normally the output file does not contain special end-of-file
C characters. But if environment variable PGPLOT_PS_EOF is defined
C (with any value) PGPLOT writes a control-D job-separator character at 
C the beginning and at the end of the file. This is appropriate for
C Apple LaserWriters using the serial interface, but it may not be 
C appropriate for other PostScript devices.
C
C  PGPLOT_PS_MARKERS
C Specify "NO" to suppress use of a PostScript font for the graph
C markers; markers are then emulated by line-drawing. 
C
C Document Structuring Conventions:
C
C  The PostScript files conform to Version 3.0 of the Adobe Document 
C  Structuring Conventions (see ref.3) and to version 3.0 of the
C  encapsulated PostScript file (EPSF) format. This should allow
C  the files to be read by other programs that accept the EPSF format.
C  Note, though, that multi-page plots are not valid EPSF files. The
C  files do not contain a screen preview section.
C
C References:
C
C (1) Adobe Systems, Inc.: PostScript Language Reference Manual.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (2) Adobe Systems, Inc.: PostScript Language Tutorial and Cookbook.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (3) Adobe Systems, Inc.: PostScript Language Reference Manual, Second 
C Edition. Addison-Wesley, Reading, Massachusetts, 1990.
C-----------------------------------------------------------------------
      INTEGER DWD, DHT, DOFFW, DOFFH
      CHARACTER*(*) PTYPE, LTYPE, CPTYPE, CLTYPE, DEFNAM
      PARAMETER (
     : PTYPE= 'VPS   (PostScript file, portrait orientation)',
     : LTYPE= 'PS    (PostScript file, landscape orientation)',
     : CPTYPE='VCPS  (Colour PostScript file, portrait orientation)',
     : CLTYPE='CPS   (Colour PostScript file, landscape orientation)')
C     PARAMETER (PTYPE='VPS', LTYPE='PS', CPTYPE='VCPS', CLTYPE='CPS')
      PARAMETER (DEFNAM='pgplot.ps')
C -- printable paper area: in milli-inches; (WIDTH, HEIGHT) are
C    the dimensions of the printable area; OFFW, OFFH the offset from
C    the lower left corner of the paper
      PARAMETER (DWD=7800, DHT=10500, DOFFW=350, DOFFH=250)
C
      INTEGER WIDTH, HEIGHT, OFFW, OFFH
      SAVE    WIDTH, HEIGHT, OFFW, OFFH
      INTEGER  IER, I0, J0, I1, J1, L, LL, LASTI, LASTJ, UNIT, LOBUF
      SAVE                                 LASTI, LASTJ, UNIT, LOBUF
      INTEGER  CI, LW, NPTS, NPAGE, IOERR, LFNAME
      SAVE         LW, NPTS, NPAGE, IOERR, LFNAME
      INTEGER  STATE
      SAVE     STATE
      INTEGER  NXP, NYP, XORG, YORG, XLEN, YLEN, N, RGB(3)
      INTEGER  HIGH, LOW, I, K, KMAX, POSN, LD, LU
      INTEGER  BBOX(4), BB1, BB2, BB3, BB4
      SAVE     BBOX
      INTEGER  GROPTX, GRCTOI
      LOGICAL  START, LANDSC, COLOR, STDOUT
      SAVE     START,         COLOR, STDOUT
      REAL     BBXMIN, BBXMAX, BBYMIN, BBYMAX
      SAVE     BBXMIN, BBXMAX, BBYMIN, BBYMAX
      REAL     RVALUE(0:255), GVALUE(0:255), BVALUE(0:255)
      SAVE     RVALUE,        GVALUE,        BVALUE
      CHARACTER*20  SUSER, SDATE
      CHARACTER*120 INSTR, MSG
      CHARACTER*132 OBUF
      SAVE          OBUF
      CHARACTER*255 FNAME
      SAVE          FNAME
      INTEGER       MARKER(0:31), NSYM, RAD(0:31)
      SAVE          MARKER, RAD
      REAL          MFAC
      SAVE          MFAC
      REAL          SHADE(0:15), RINIT(0:15), GINIT(0:15), BINIT(0:15)
      SAVE          SHADE,       RINIT,       GINIT,       BINIT
      CHARACTER*1   HEXDIG(0:15)
      DATA HEXDIG/'0','1','2','3','4','5','6','7',
     1            '8','9','A','B','C','D','E','F'/
      DATA SHADE /1.00, 13*0.00, 0.33, 0.67/
      DATA RINIT 
     1     / 1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00, 1.00,
     2       1.00, 0.50, 0.00, 0.00, 0.50, 1.00, 0.33, 0.67/
      DATA GINIT
     1     / 1.00, 0.00, 0.00, 1.00, 0.00, 1.00, 0.00, 1.00,
     2       0.50, 1.00, 1.00, 0.50, 0.00, 0.00, 0.33, 0.67/
      DATA BINIT
     1     / 1.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 0.00,
     2       0.00, 0.00, 0.50, 1.00, 1.00, 0.50, 0.33, 0.67/
      DATA RAD/ 6,  1,  7,  6, 7, 5, 6, 8,
     :          7,  7,  9, 10, 9, 8, 6, 8,
     :          4,  5,  9, 12, 2, 4, 5, 7,
     :         11, 17, 22, 41, 9, 9, 9, 9/
      DATA STATE/0/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,900,900,260,900,280,290), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 IF (MODE.EQ.1) THEN
C         -- landscape, monochrome
          CHR = LTYPE
          LCHR = LEN(LTYPE)
      ELSE IF (MODE.EQ.2) THEN
C         -- portrait, monochrome
          CHR = PTYPE
          LCHR = LEN(PTYPE)
      ELSE IF (MODE.EQ.3) THEN
C         -- landscape, color
          CHR = CLTYPE
          LCHR = LEN(CLTYPE)
      ELSE
C         -- portrait, color
          CHR = CPTYPE
          LCHR = LEN(CPTYPE)
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = -1
      RBUF(3) = 0
      RBUF(4) = -1
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
      RBUF(3) = 5
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, Area fill, 
C    Thick lines, QCR, Markers [optional])
C
   40 CONTINUE
      CHR = 'HNNATNQNYM'
C     -- Marker support suppressed?
      CALL GRGENV('PS_MARKERS', INSTR, L)
      IF (L.GE.2) THEN
         IF (INSTR(1:L).EQ.'NO' .OR. INSTR(1:L).EQ.'no') THEN
            CHR(10:10) = 'N'
         END IF
      END IF
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(3) = 0
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          RBUF(2) = HEIGHT-1
          RBUF(4) = WIDTH-1
      ELSE
          RBUF(2) = WIDTH-1
          RBUF(4) = HEIGHT-1
      END IF
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 8
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
C     -- check for concurrent access
      IF (STATE.EQ.1) THEN
         CALL GRWARN('a PGPLOT PostScript file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
C     -- Color mode?
      CALL GRGENV('PS_COLOR', INSTR, L)
      COLOR = L.GT.0 .OR. MODE.EQ.3 .OR. MODE.EQ.4
      IF (COLOR) THEN
         DO 91 CI=0,15
            RVALUE(CI) = RINIT(CI)
            GVALUE(CI) = GINIT(CI)
            BVALUE(CI) = BINIT(CI)
 91      CONTINUE
      ELSE
         DO 92 CI=0,15
            RVALUE(CI) = SHADE(CI)
            GVALUE(CI) = SHADE(CI)
            BVALUE(CI) = SHADE(CI)
 92      CONTINUE
      END IF
      DO 93 CI=16,255
         RVALUE(CI) = 0.0
         GVALUE(CI) = 0.0
         BVALUE(CI) = 0.0
 93   CONTINUE
C     -- Device dimensions
      WIDTH = DWD
      HEIGHT = DHT
      OFFW = DOFFW
      OFFH = DOFFH
      CALL GRGENV('PS_WIDTH', INSTR, L)
      LL = 1
      IF (L.GT.0) WIDTH = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_HEIGHT', INSTR, L)
      LL = 1
      IF (L.GT.0) HEIGHT = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_HOFFSET', INSTR, L)
      LL = 1
      IF (L.GT.0) OFFW = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_VOFFSET', INSTR, L)
      LL = 1
      IF (L.GT.0) OFFH = GRCTOI(INSTR(:L),LL)
      STDOUT =CHR(1:LCHR).EQ.'-'
      IF (STDOUT) THEN
         UNIT = 6
C        -- machine-dependent!
      ELSE
         CALL GRGLUN(UNIT)
      END IF
      NBUF = 2
      RBUF(1) = UNIT
      IF (.NOT.STDOUT) THEN
         IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
         IF (IER.NE.0) THEN
            MSG = 'Cannot open output file for PostScript plot: '//
     1           CHR(:LCHR)
            CALL GRWARN(MSG)
            RBUF(2) = 0
            CALL GRFLUN(UNIT)
            RETURN
         ELSE
            INQUIRE (UNIT=UNIT, NAME=CHR)
            LCHR = LEN(CHR)
 94         IF (CHR(LCHR:LCHR).EQ.' ') THEN
               LCHR = LCHR-1
               GOTO 94
            END IF
            RBUF(2) = 1
            FNAME = CHR(:LCHR)
            LFNAME = LCHR
         END IF
      ELSE
         RBUF(2) = 1
         FNAME = '-'
         LFNAME= 1
      END IF
      STATE = 1
      IOERR = 0
      LOBUF = 0
      LASTI = -1
      LASTJ = -1
      LW = 1
      NPTS = 0
      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, CHAR(4))
      CALL GRPS02(IOERR, UNIT, '%!PS-Adobe-3.0 EPSF-3.0')
      CALL GRUSER(INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, '%%For: '//INSTR(1:L))
      CALL GRPS02(IOERR, UNIT, '%%Title: PGPLOT PostScript plot')
      CALL GRPS02(IOERR, UNIT, '%%Creator: PGPLOT')
      CALL GRDATE(INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT,
     :    '%%CreationDate: '//INSTR(1:L))
      CALL GRGENV('PS_BBOX', INSTR, L)
      CALL GRTOUP(INSTR(1:3), INSTR(1:3))
      IF (INSTR(1:3).EQ.'MAX') THEN
C        -- bounding box is based on maximum plot dimensions, not
C           actual dimensions
         CALL GRFAO('%%BoundingBox: # # # #', L, INSTR,
     :        NINT(OFFW*0.072), NINT(OFFH*0.072),
     :        NINT((WIDTH+OFFW)*0.072), NINT((HEIGHT+OFFH)*0.072))
         CALL GRPS02(IOERR, UNIT, INSTR(:L))
      ELSE
         CALL GRPS02(IOERR, UNIT, '%%BoundingBox: (atend)')
      END IF
      CALL GRPS02(IOERR, UNIT, '%%DocumentFonts: (atend)')
      CALL GRPS02(IOERR, UNIT, '%%LanguageLevel: 1')
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          CALL GRPS02(IOERR, UNIT, '%%Orientation: Landscape')
      ELSE
          CALL GRPS02(IOERR, UNIT, '%%Orientation: Portrait')
      END IF
      CALL GRPS02(IOERR, UNIT, '%%Pages: (atend)')
      CALL GRPS02(IOERR, UNIT, '%%EndComments')
      CALL GRPS02(IOERR, UNIT, '%%BeginProlog')
      CALL GRPS02(IOERR, UNIT, 
     1  '/L {moveto rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(IOERR, UNIT, 
     1  '/C {rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(IOERR, UNIT, 
     1  '/D {moveto 0 0 rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(IOERR, UNIT, '/SLW {5 mul setlinewidth} bind def')
      CALL GRPS02(IOERR, UNIT, '/SCF /pop load def')
      CALL GRPS02(IOERR, UNIT, '/BP {newpath moveto} bind def')
      CALL GRPS02(IOERR, UNIT, '/LP /rlineto load def')
      CALL GRPS02(IOERR, UNIT, 
     1  '/EP {rlineto closepath eofill} bind def')
      CALL GRPS02(IOERR, UNIT, '/MB {gsave translate MFAC dup scale '//
     1 '1 setlinewidth 2 setlinecap 0 setlinejoin newpath} bind def')
      CALL GRPS02(IOERR, UNIT, '/ME /grestore load def')
      CALL GRPS02(IOERR, UNIT, '/CC {0 360 arc stroke} bind def')
      CALL GRPS02(IOERR, UNIT, '/FC {0 360 arc fill} bind def')
      CALL GRGENV('IDENT', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRPS02(IOERR, UNIT,
     :        '/RS{findfont exch scalefont setfont moveto dup'//
     :        ' stringwidth neg exch neg exch rmoveto show} bind def')
      END IF
      CALL GRPS02(IOERR, UNIT, '%%EndProlog')
      NPAGE = 0
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      CALL GRPS02(IOERR, UNIT, ' ')
      CALL GRPS02(IOERR, UNIT, '%%Trailer')
      CALL GRGENV('PS_BBOX', INSTR, L)
      CALL GRTOUP(INSTR(1:3), INSTR(1:3))
      IF (INSTR(1:3).NE.'MAX') THEN
         CALL GRFAO('%%BoundingBox: # # # #', L, INSTR,
     :        BBOX(1), BBOX(2), BBOX(3), BBOX(4))
         CALL GRPS02(IOERR, UNIT, INSTR(:L))
      END IF
      CALL GRPS02(IOERR, UNIT, '%%DocumentFonts: ')
      CALL GRFAO('%%Pages: #', L, INSTR, NPAGE, 0, 0, 0)
      CALL GRPS02(IOERR, UNIT, INSTR(:L))
      CALL GRPS02(IOERR, UNIT, '%%EOF')
      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, CHAR(4))
      IF (IOERR.NE.0) THEN
          CALL GRWARN('++WARNING++ Error '//
     1       'writing PostScript file: file is incomplete')
          CALL GRWARN('Check for device full or quota exceeded')
          CALL GRWARN('Filename: '//FNAME(:LFNAME))
      END IF
      IF (.NOT.STDOUT) THEN
         CLOSE (UNIT, IOSTAT=IOERR)
         IF (IOERR.NE.0) THEN
           CALL GRWARN('Error closing PostScript file '//FNAME(:LFNAME))
         END IF
         CALL GRFLUN(UNIT)
      END IF
      STATE = 0
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
         HEIGHT = RBUF(1)
         WIDTH = RBUF(2)
      ELSE
         WIDTH = RBUF(1)
         HEIGHT = RBUF(2)
      END IF
      NPAGE = NPAGE+1
      CALL GRPS02(IOERR, UNIT, ' ')
      CALL GRFAO('%%Page: # #', L, INSTR, NPAGE, NPAGE, 0, 0)
      CALL GRPS02(IOERR, UNIT, INSTR(:L))
      CALL GRPS02(IOERR, UNIT, '%%BeginPageSetup')
      CALL GRPS02(IOERR, UNIT, '/PGPLOT save def')
      CALL GRPS02(IOERR, UNIT, '0.072 0.072 scale')
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          CALL GRFAO('# # translate 90 rotate', L, INSTR, WIDTH+OFFW, 
     1               OFFH, 0, 0)
      ELSE
          CALL GRFAO('# # translate', L, INSTR, OFFW, OFFH, 0, 0)
      END IF
      CALL GRPS02(IOERR, UNIT, INSTR(:L))
      CALL GRPS02(IOERR, UNIT, '1 setlinejoin 1 setlinecap 1 SLW 1 SCF')
      CALL GRPS02(IOERR, UNIT, '%%EndPageSetup')
      CALL GRPS02(IOERR, UNIT, '%%PageBoundingBox: (atend)')
      DO 111 NSYM=0,31
          MARKER(NSYM) = 0
  111 CONTINUE
      MFAC = 0.0
      BBXMIN = WIDTH
      BBYMIN = HEIGHT
      BBXMAX = 0.0
      BBYMAX = 0.0
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      IF (I0.EQ.LASTI .AND. J0.EQ.LASTJ) THEN
C        -- suppress zero-length continuation segment
         IF (I0.EQ.I1 .AND. J0.EQ.J1) RETURN
         CALL GRFAO('# # C', L, INSTR, (I1-I0), (J1-J0), 0, 0)
      ELSE
         CALL GRFAO('# # # # L', L, INSTR, (I1-I0), (J1-J0), I0, J0)
      END IF
      LASTI = I1
      LASTJ = J1
      BBXMIN = MIN(BBXMIN, I0-LW*5.0, I1-LW*5.0)
      BBXMAX = MAX(BBXMAX, I0+LW*5.0, I1+LW*5.0)
      BBYMIN = MIN(BBYMIN, J0-LW*5.0, J1-LW*5.0)
      BBYMAX = MAX(BBYMAX, J0+LW*5.0, J1+LW*5.0)
      GOTO 800
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      CALL GRFAO('# # D', L, INSTR, I1, J1, 0, 0)
      LASTI = I1
      LASTJ = J1
      BBXMIN = MIN(BBXMIN, I1-LW*5.0)
      BBXMAX = MAX(BBXMAX, I1+LW*5.0)
      BBYMIN = MIN(BBYMIN, J1-LW*5.0)
      BBYMAX = MAX(BBYMAX, J1+LW*5.0)
      GOTO 800
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
C     -- optionally write identification
      CALL GRGENV('IDENT', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRUSER(SUSER, LU)
         CALL GRDATE(SDATE, LD)
         POSN = WIDTH - 1
         IF (LANDSC) POSN = HEIGHT - 1
         CALL GRFAO('('//SUSER(:LU)//' '//SDATE(:LD)//
     :        ' [#]) # # 100 /Helvetica RS',
     :        L, INSTR, NPAGE, POSN, 50, 0)
         CALL GRPS02(IOERR, UNIT, '0.0 setgray')
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
C     -- optionally draw bounding box
      CALL GRGENV('PS_DRAW_BBOX', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRFAO('0.0 setgray 0 SLW newpath # # moveto', L, INSTR,
     :              NINT(BBXMIN), NINT(BBYMIN), 0, 0)
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
         CALL GRFAO('# # lineto # # lineto', L, INSTR,
     :        NINT(BBXMIN), NINT(BBYMAX), NINT(BBXMAX), NINT(BBYMAX))
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
         CALL GRFAO('# # lineto closepath stroke', L,INSTR,
     :              NINT(BBXMAX), NINT(BBYMIN), 0, 0)
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
      CALL GRPS02(IOERR, UNIT, 'PGPLOT restore showpage')
      CALL GRPS02(IOERR, UNIT, '%%PageTrailer')
      IF (LANDSC) THEN
         BB1 = INT((WIDTH-BBYMAX+OFFW)*0.072)
         BB2 = INT((BBXMIN+OFFH)*0.072)
         BB3 = 1+INT((WIDTH-BBYMIN+OFFW)*0.072)
         BB4 = 1+INT((BBXMAX+OFFH)*0.072)
      ELSE
         BB1 = INT((BBXMIN+OFFW)*0.072)
         BB2 = INT((BBYMIN+OFFH)*0.072)
         BB3 = 1+INT((BBXMAX+OFFW)*0.072)
         BB4 = 1+INT((BBYMAX+OFFH)*0.072)
      END IF
      CALL GRFAO('%%PageBoundingBox: # # # #', L, INSTR,
     :           BB1, BB2, BB3, BB4)
      CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      IF (NPAGE.EQ.1) THEN
         BBOX(1) = BB1
         BBOX(2) = BB2
         BBOX(3) = BB3
         BBOX(4) = BB4
      ELSE
         BBOX(1) = MIN(BBOX(1),BB1)
         BBOX(2) = MIN(BBOX(2),BB2)
         BBOX(3) = MAX(BBOX(3),BB3)
         BBOX(4) = MAX(BBOX(4),BB4)
      END IF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      IF (COLOR) THEN
          WRITE(INSTR,'(3(F5.3,1X),''setrgbcolor'')')
     1          RVALUE(CI), GVALUE(CI), BVALUE(CI)
          L = 29
      ELSE
          WRITE(INSTR,'(F5.3,1X,''setgray'')') RVALUE(CI)
          L = 13
      END IF
      LASTI = -1
      GOTO 800
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
  190 GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (NPTS.EQ.0) THEN
          NPTS = RBUF(1)
          START = .TRUE.
          RETURN
      ELSE
          NPTS = NPTS-1
          I0 = NINT(RBUF(1))
          J0 = NINT(RBUF(2))
          IF (START) THEN
              CALL GRFAO('# # BP', L, INSTR, I0, J0, 0, 0)
              START = .FALSE.
              LASTI = I0
              LASTJ = J0
          ELSE IF (NPTS.EQ.0) THEN
              CALL GRFAO('# # EP', L, INSTR, (I0-LASTI), 
     1                     (J0-LASTJ), 0, 0)
              LASTI = -1
              LASTJ = -1
          ELSE
              CALL GRFAO('# # LP', L, INSTR, (I0-LASTI), 
     1                     (J0-LASTJ), 0, 0)
              LASTI = I0
              LASTJ = J0
          END IF
          BBXMIN = MIN(BBXMIN, I0-LW*5.0)
          BBXMAX = MAX(BBXMAX, I0+LW*5.0)
          BBYMIN = MIN(BBYMIN, J0-LW*5.0)
          BBYMAX = MAX(BBYMAX, J0+LW*5.0)
          GOTO 800
      END IF
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (COLOR) THEN
          CI = RBUF(1)
          RVALUE(CI) = RBUF(2)
          GVALUE(CI) = RBUF(3)
          BVALUE(CI) = RBUF(4)
      ELSE
          CI = RBUF(1)
          RVALUE(CI) = 0.30*RBUF(2) + 0.59*RBUF(3) + 0.11*RBUF(4)
          GVALUE(CI) = RVALUE(CI)
          BVALUE(CI) = RVALUE(CI)
      END IF
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      LW = NINT(RBUF(1))
      CALL GRFAO('# SLW', L, INSTR, LW, 0, 0, 0)
      LASTI = -1
      GOTO 800
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      IF (LOBUF.NE.0) THEN
C         -- flush buffer first
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      CALL GRPS02(IOERR, UNIT, CHR(:LCHR))
      LASTI = -1
      RETURN
C
C--- IFUNC=26, Image.---------------------------------------------------
C
  260 CONTINUE
      N = RBUF(1)
      IF (N.EQ.0) THEN
C         -- First: setup for image
C         -- Set clipping region (RBUF(2...5))
          NXP = RBUF(2)
          NYP = RBUF(3)
          XORG = RBUF(4)
          XLEN = RBUF(5) - RBUF(4)
          YORG = RBUF(6) 
          YLEN = RBUF(7) - RBUF(6)
          BBXMIN = MIN(BBXMIN, RBUF(4), RBUF(5))
          BBXMAX = MAX(BBXMAX, RBUF(4), RBUF(5))
          BBYMIN = MIN(BBYMIN, RBUF(6), RBUF(7))
          BBYMAX = MAX(BBYMAX, RBUF(6), RBUF(7))
C      
          CALL GRPS02(IOERR, UNIT, 'gsave newpath')
          CALL GRFAO('# # moveto # 0 rlineto 0 # rlineto', L, INSTR,
     :               XORG, YORG, XLEN, YLEN)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          CALL GRFAO('# 0 rlineto closepath clip', L, INSTR, -XLEN,
     :                0, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
C         -- 
          CALL GRFAO('/picstr # string def', L, INSTR, NXP, 0, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          CALL GRFAO('# # 8 [', L, INSTR, NXP, NYP, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          WRITE (INSTR, '(6(1PE10.3, 1X), '']'')') (RBUF(I),I=8,13)
          CALL GRPS02(IOERR, UNIT, INSTR(:67))
          IF (COLOR) THEN
              CALL GRPS02(IOERR, UNIT, 
     :      '{currentfile picstr readhexstring pop} false 3 colorimage')
          ELSE
              CALL GRPS02(IOERR, UNIT, 
     :      '{currentfile picstr readhexstring pop} image')
          END IF
      ELSE IF (N.EQ.-1) THEN
C         -- Last: terminate image
          CALL GRPS02(IOERR, UNIT, 'grestore')
      ELSE 
C         -- Middle: write N image pixels; each pixel uses 6 chars
C            in INSTR, so N must be <= 20.
          L = 0
          KMAX = 1
          IF (COLOR) KMAX = 3
          DO 262 I=1,N
              CI = RBUF(I+1)
              RGB(1) = NINT(255.0*RVALUE(CI))
              RGB(2) = NINT(255.0*GVALUE(CI))
              RGB(3) = NINT(255.0*BVALUE(CI))
              DO 261 K=1,KMAX
                  HIGH = RGB(K)/16
                  LOW  = RGB(K)-16*HIGH
                  L = L+1
                  INSTR(L:L) = HEXDIG(HIGH)
                  L = L+1
                  INSTR(L:L) = HEXDIG(LOW)
 261          CONTINUE
 262      CONTINUE
          CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
      RETURN
C
C--- IFUNC=28, Marker.--------------------------------------------------
C
  280 CONTINUE
      NSYM = NINT(RBUF(1))
C     -- Output code for this marker if necessary
      IF (MARKER(NSYM).EQ.0) THEN
          IF (LOBUF.GT.0) CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
          CALL GRPS03(IOERR, NSYM, UNIT)
          MARKER(NSYM) = 1
      END IF
C     -- Output scale factor
      IF (RBUF(4).NE.MFAC) THEN
          IF (LOBUF.GT.0) CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
          MFAC = RBUF(4)
          WRITE (INSTR, '(''/MFAC '',F10.3,'' def'')') MFAC
          CALL GRPS02(IOERR, UNIT, INSTR(1:24))
      END IF
C     -- Output an instruction to draw one marker
      I1 = NINT(RBUF(2))
      J1 = NINT(RBUF(3))
      CALL GRFAO('# # M#', L, INSTR, I1, J1, NSYM, 0)
      LASTI = -1
      BBXMIN = MIN(BBXMIN, I1-MFAC*RAD(NSYM))
      BBXMAX = MAX(BBXMAX, I1+MFAC*RAD(NSYM))
      BBYMIN = MIN(BBYMIN, J1-MFAC*RAD(NSYM))
      BBYMAX = MAX(BBYMAX, J1+MFAC*RAD(NSYM))
      GOTO 800
C
C--- IFUNC=29, Query color representation.------------------------------
C
 290  CONTINUE
      CI = NINT(RBUF(1))
      NBUF = 4
      RBUF(2) = RVALUE(CI)
      RBUF(3) = GVALUE(CI)
      RBUF(4) = BVALUE(CI)
      RETURN
C
C-----------------------------------------------------------------------
C Buffer output if possible.
C
  800 IF ( (LOBUF+L+1). GT. 132) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          OBUF(1:L) = INSTR(1:L)
          LOBUF = L
      ELSE
          IF (LOBUF.GT.1) THEN
              LOBUF = LOBUF+1
              OBUF(LOBUF:LOBUF) = ' '
          END IF
          OBUF(LOBUF+1:LOBUF+L) = INSTR(1:L)
          LOBUF = LOBUF+L
      END IF
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,
     1  '(''Unimplemented function in PS device driver: '',I10)') IFUNC
      CALL GRWARN(MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END

C*GRPS03 -- PGPLOT PostScript driver, marker support
C+
      SUBROUTINE GRPS03(IOERR, NSYM, UNIT)
      INTEGER IOERR, NSYM, UNIT
C
C Write PostScript instructions for drawing graph marker number NSYM
C on Fortran unit UNIT.
C-----------------------------------------------------------------------
      CHARACTER*80 T(6)
      INTEGER I, N
C
      IF (NSYM.LT.0 .OR. NSYM.GT.31) RETURN
      GOTO (100, 101, 102, 103, 104, 105, 106, 107, 108,
     1      109, 110, 111, 112, 113, 114, 115, 116, 117,
     2      118, 119, 120, 121, 122, 123, 124, 125, 126,
     3      127, 128, 129, 130, 131) NSYM+1
C
  100 T(1)='/M0 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
      T(2)='0 -12 rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  101 T(1)='/M1 {MB 0 0 1 FC ME} bind def'
      N=1
      GOTO 200
  102 T(1)='/M2 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto'
      T(2)='14 0 rlineto stroke ME} bind def'
      N=2
      GOTO 200
  103 T(1)='/M3 {MB 0 6 moveto 0 -6 lineto -5 3 moveto 5 -3 lineto'
      T(2)='5 3 moveto -5 -3 lineto stroke ME} bind def'
      N=2
      GOTO 200
  104 T(1)='/M4 {MB 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  105 T(1)='/M5 {MB -5 -5 moveto 10 10 rlineto -5 5 moveto'
      T(2)='10 -10 rlineto stroke ME} bind def'
      N=2
      GOTO 200
  106 T(1)='/M6 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
      T(2)='0 -12 rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  107 T(1)='/M7 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='stroke ME} bind def'
      N=2
      GOTO 200 
  108 T(1)='/M8 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto 14 0 rlineto'
      T(2)='stroke 0 0 7 CC ME} bind def'
      N=2
      GOTO 200
  109 T(1)='/M9 {MB 0 0 1 FC 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  110 T(1)='/M10 {MB -9 9 moveto -8 7 lineto -7 3 lineto -7 -3 lineto'
      T(2)='-8 -7 lineto -9 -9 lineto -7 -8 lineto -3 -7 lineto'
      T(3)='3 -7 lineto 7 -8 lineto 9 -9 lineto 8 -7 lineto'
      T(4)='7 -3 lineto 7 3 lineto 8 7 lineto 9 9 lineto 7 8 lineto'
      T(5)='3 7 lineto -3 7 lineto  -7 8 lineto closepath stroke'
      T(6)='ME} bind def'
      N=6
      GOTO 200
  111 T(1)='/M11 {MB 0 10 moveto -6 0 lineto 0 -10 lineto 6 0 lineto'
      T(2)='closepath stroke ME} bind def'
      N=2
      GOTO 200
  112 T(1)='/M12 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
      T(2)='-5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
      T(3)='lineto 2 3 lineto closepath stroke ME} bind def'
      N=3
      GOTO 200
  113 T(1)='/M13 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='fill ME} bind def'
      N=2
      GOTO 200
  114 T(1)='/M14 {MB -2 6 moveto -2 2 lineto -6 2 lineto -6 -2 lineto'
      T(2)='-2 -2 lineto -2 -6 lineto 2 -6 lineto 2 -2 lineto'
      T(3)='6 -2 lineto 6 2 lineto 2 2 lineto 2 6 lineto closepath'
      T(4)='stroke ME} bind def'
      N=4
      GOTO 200
  115 T(1)='/M15 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='0 -8 moveto 7 4 lineto -7 4 lineto closepath stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  116 T(1)='/M16 {MB -4 -4 moveto 0 8 rlineto 8 0 rlineto 0 -8'
      T(2)='rlineto closepath fill ME} bind def'
      N=2
      GOTO 200
  117 T(1)='/M17 {MB 0 0 4.5 FC ME} bind def'
      N=1
      GOTO 200
  118 T(1)='/M18 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
      T(2)=' -5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
      T(3)='lineto 2 3 lineto closepath fill ME} bind def'
      N=3
      GOTO 200
  119 T(1)='/M19 {MB -12 -12 moveto 0 24 rlineto 24 0 rlineto 0 -24'
      T(2)='rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  120 T(1)='/M20 {MB 0 0 2 CC ME} bind def'
      N=1
      GOTO 200
  121 T(1)='/M21 {MB 0 0 4 CC ME} bind def'
      N=1
      GOTO 200
  122 T(1)='/M22 {MB 0 0 5 CC ME} bind def'
      N=1
      GOTO 200
  123 T(1)='/M23 {MB 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  124 T(1)='/M24 {MB 0 0 11 CC ME} bind def'
      N=1
      GOTO 200
  125 T(1)='/M25 {MB 0 0 17 CC ME} bind def'
      N=1
      GOTO 200
  126 T(1)='/M26 {MB 0 0 22 CC ME} bind def'
      N=1
      GOTO 200
  127 T(1)='/M27 {MB 0 0 41 CC ME} bind def'
      N=1
      GOTO 200
  128 T(1)='/M28 {MB -6 2 moveto -9 0 lineto -6 -2 lineto -3 5'
      T(2)='moveto -8 0 lineto -3 -5 lineto -8 0 moveto 9 0 lineto'
      T(3)='stroke ME} bind def'
      N=3
      GOTO 200
  129 T(1)='/M29 {MB 6 2 moveto 9 0 lineto 6 -2 lineto 3 5 moveto'
      T(2)='8 0 lineto 3 -5 lineto 8 0 moveto -9 0 lineto stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  130 T(1)='/M30 {MB 2 6 moveto 0 9 lineto -2 6 lineto 5 3 moveto'
      T(2)='0 8 lineto -5 3 lineto 0 8 moveto 0 -9 lineto stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  131 T(1)='/M31 {MB 2 -6 moveto 0 -9 lineto -2 -6 lineto 5 -3'
      T(2)='moveto 0 -8 lineto -5 -3 lineto 0 -8 moveto 0 9 lineto'
      T(3)='stroke ME} bind def'
      N=3
      GOTO 200
C
  200 DO 210 I=1,N
          CALL GRPS02(IOERR, UNIT, T(I))
  210 CONTINUE
C
      END

C*GRPS02 -- PGPLOT PostScript driver, copy buffer to file
C+
      SUBROUTINE GRPS02 (IER, UNIT, S)
C
C Support routine for PSdriver: write character string S on
C specified Fortran unit.
C
C Error handling: if IER is not 0 on input, the routine returns
C immediately. Otherwise IER receives the I/O status from the Fortran
C write (0 => success).
C-----------------------------------------------------------------------
      INTEGER IER, UNIT
      CHARACTER*(*) S
C
      IF (IER.EQ.0) THEN
          WRITE (UNIT, '(A)', IOSTAT=IER) S
          IF (IER.NE.0) CALL 
     1        GRWARN('++WARNING++ Error writing PostScript file')
      END IF
C-----------------------------------------------------------------------
      END
C*GREXEC -- PGPLOT device handler dispatch routine
C 12/93 C. T. Dum: version for MS F32 Power Station
C  20-Apr-1996: "W9DRIV", use CASE structure [PAS]
C+
      SUBROUTINE GREXEC(IDEV, IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IDEV, IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C---
      INTEGER NDEV
      PARAMETER (NDEV=11)
      CHARACTER*10 MSG
C---
      SELECT CASE (IDEV)
      CASE (0)
         RBUF(1) = NDEV
         NBUF = 1
         CHR = ' '
         LCHR = 0
      CASE (1)
      CASE (2)
      CASE (3)
      CASE (4)
      CASE (5)
      CASE (6)
         CALL NUDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      CASE (7)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,1)
      CASE (8)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,2)
      CASE (9)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,3)
      CASE (10)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,4)
      CASE (11)
         CALL LXDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      CASE DEFAULT
         WRITE (MSG,'(I10)') IDEV
         CALL GRQUIT('Unknown device code in GREXEC: '//MSG)
      END SELECT
      RETURN
      END
