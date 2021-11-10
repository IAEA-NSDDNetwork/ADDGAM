*  ENSDFLIB.FOR                                                         01 00010
*                                                                       01 00020
*  VERSION 1  AS OF  8-JUL-92.  COMBINED F77STR V4(10), NSDCNV V2(3),   01 00030
*                               AND NSDMTH V1(0).                       01 00040
*          1.1      23-FEB-93. 1. Finished typing all variables         01 00050
*                              2. Delinted                              01 00060
*                              3. Avoided possible compiler problems    01 00070
*                                 with DO loop variables in BREAK,      01 00080
*                                 SPAN, and INTSCN                      01 00090
*                              4. Corrected TYPSTR:                     01 00100
*                                 Was not allowing leading blanks as    01 00110
*                                   per documentation for FORTRAN       01 00120
*                                   numbers                             01 00130
*                                 Worked around AIX XL FORTRAN          01 00140
*                                   "enhancement" which ignored error   01 00150
*                                   branch in test for FORTRAN numbers  01 00160
*                               5. Corrected RLSCN and DRLSCN: Not      01 00170
*                                  allowing for a real number not       01 00180
*                                  followed by an exponent              01 00190
*          1.2      01-Apr-93.  Corrected RLSCN and DRLSCN - Attempt to 01 00200
*                                  divide by zero when period but no    01 00210
*                                  decimal fraction                     01 00220
*          1.3      31-Aug-94.  Corrected NUMSTR - No check on integer  01 00230
*                                  being larger than string             01 00240
*          1.4      09-Feb-95.  1. Corrected CNVU2S                     01 00250
*                                  Format 2 was not working as designed.01 00260
*                                    Corrected this. Also, LENDX now    01 00270
*                                    to specify number of significant   01 00280
*                                    digits.                            01 00290
*                                  Integer overflow errors for extremely01 00300
*                                    precise numbers - added check for  01 00310
*                                    this.                              01 00320
*                               2. Added Logical Function IOVRFLW to    01 00330
*                                  check for possible integer overflow. 01 00340
*                               3. Corrected KNVIX. Increased TEMP from 01 00350
*                                  12 to 20 characters to avoid         01 00360
*                                  truncation of final result.          01 00370
*                               4. Corrected SCALX. Roundoff problem due01 00380
*                                  to mixture of REAL*4 and REAL*8 in   01 00390
*                                  line 20.                             01 00400
*          1.4a     10-Feb-95.  Corrected line 11 of IOVRFLW for        01 00410
*                                 compiler-dependent problem and lines  01 00420
*                                 10 and 11 for FORTRAN-dependent       01 00430
*                                 double precision problems             01 00440
*          1.4b     13-Feb-95.  Corrected line 11 of IOVRFLW for        01 00450
*                                 compiler-dependent problem            01 00460
*          1.4c     04-Apr-95.  Corrected line 11 of IOVRFLW for typo   01 00470
*                                 error                                 01 00480
*          1.4d     03-Nov-95.  Corrected error in RLSCN and DRLSCN when01 00490
*                                 there was a trailing period.          01 00500
*          1.4e     01-Mar-96   Incorrect results returned in RLSCN and 01 00510
*                                 DRLSCN when a period followed by one  01 00520
*                                 number - Fixed                        01 00530
*          1.5      11-Aug-98   Modified ZSYM and IZEL to handle IUPAC  01 00540
*                                 chemical symbols between Z=104 and 10901 00550
*                                 inclusive. Extended allowed range of  01 00560
*                                 Z's from 112 to 199.                  01 00570
*          1.5a     16-Sep-98   Corrected error in IZEL when mixed      01 00580
*                                 alphanumeric symbol (e.g. "1S") passed01 00590
*                                 to it.                                01 00600
*          1.5b     08-Oct-98   Changed IOVRFLW to IVRFLW for ANSI      01 00610
*                                 standard considerations               01 00620
*          1.5c     14-Apr-99   Modified ZSYM/IZEL for change in the    01 00630
*                               neutron chemical symbol from "N " to    01 00640
*                               "NN"                                    01 00650
*          1.5d     28-Jun-99   Corrected fatal error in CNVS2U when    01 00660
*                                 the single character "E" was passed   01 00670
*                                                                       01 00680
*                                                                       01 00690
************************************************************************01 00700
*                                                                      *01 00710
*  SUBPROGRAM LIBRARY F77STR.                                          *01 00720
*     F77 VERSION OF STRING ROUTINES.                                  *01 00730
*                                                                      *01 00740
*  VERSION 1(00) AS OF  5-AUG-83. REWRITE FOR FORTRAN 77.              *01 00750
*  VERSION 1(01) AS OF 12-AUG-83. IBM VS FORTRAN WON'T ALLOW FUNCTION  *01 00760
*                                 NAME TO BE ASSIGNED ITS VALUE AS DO  *01 00770
*                                 LOOP VARIABLE OR AS ACTUAL FUNCTION  *01 00780
*                                 ARGUMENT USED WITHIN THE FUNCTION.   *01 00790
*  VERSION 1(02) AS OF 16-AUG-83. ADDSTR DIDN'T TAKE INTO CONSIDERATION*01 00800
*                                 THE CASE WHERE THE INSERTION IS TO   *01 00810
*                                 THE END OF THE STRING.               *01 00820
*  VERSION 1(03) AS OF 16-AUG-83. DELSTR DIDN'T TAKE INTO CONSIDERATION*01 00830
*                                 THE CASE WHERE THE DELETION IS AT THE*01 00840
*                                 END OF THE STRING.                   *01 00850
*  VERSION 2(04) AS OF 20-JUL-84. ADD NEW SUBROUTINES RPLSTR (REPLACE  *01 00860
*                                 STRING) AND REDSTR (REDUCE STRING).  *01 00870
*  VERSION 2(05) AS OF 25-JUL-84. FIX REDSTR SUCH THAT IF TO = CHAR(0) *01 00880
*                                 THEN FROM IS SIMPLY DELETED.         *01 00890
*  VERSION 2(06) AS OF 10-SEP-84. RENAME RPLSTR TO REPCHR AND REDSTR TO*01 00900
*                                 REPSTR.                              *01 00910
*  VERSION 2(07) AS OF 10-SEP-84. RLSCN AND INTSCN SHOULD IGNORE LEAD- *01 00920
*                                 ING BLANKS.                          *01 00930
*  VERSION 2(10) AS OF 10-SEP-84. REMOVE UNNEEDED ICHAR CALLS.         *01 00940
*  VERSION 2(11) AS OF 10-SEP-84. IN RLSCN ACCEPT LOWER CASE E ALSO.   *01 00950
*  VERSION 3(12) AS OF 11-SEP-84. ADD FUNCTION INDEXF.                 *01 00960
*  VERSION 3(13) AS OF 13-SEP-84. LET INDEXF CHECK POS OUT OF BOUNDS.  *01 00970
*  VERSION 3(14) AS OF  3-OCT-84. 2(7) NEEDS MORE WORK.                *01 00980
*  VERSION 4     AS OF    JUN-86. Add PADLFT and LBSUB.  Modify VALSTR *01 00990
*                                 and RLSCN to allow double precision  *01 01000
*  VERSION 4(1)  AS OF    DEC-86. DRLSCN made to be a separate sub.    *01 01010
*                                 RLSCN back to 3(14) version.         *01 01020
*  VERSION 4(2)  AS OF  7-JAN-87. PADLFT DO LOOP ADDED FOR IPC         *01 01030
*  VERSION 4(3)  AS OF 13-APR-88. TYPSTR MOD(FORTRAN NUM RETURN -2)    *01 01040
*  VERSION 4(4)  AS OF 25-JUL-88. RLSCN,DRLSCN MODIFIED                *01 01050
*  VERSION 4(5)  AS OF  3-FEB-89. INTSCN - WHEN STRING HAS SIGN ALONE  *01 01060
*  VERSION 4(6)  AS OF 15-AUG-89. ADDSTR and DELSTR eliminate work     *01 01070
*  VERSION 4(7)  AS OF 29-SEP-89. LENSTR treats null as blank          *01 01080
*  VERSION 4(8)  AS OF 02-MAR-90. REPSTR-correct problem when to string*01 01090
*                                 contains from string                 *01 01100
*  VERSION 4(9)  AS OF 26-APR-90. TYPSTR FORTRAN NO. bug fix           *01 01110
*  VERSION 4(10) AS OF 17-MAY-90. ADDSTR and DELSTR modified so that   *01 01120
*                                 they work with compilers that don't  *01 01130
*                                 handle STR(2:)=STR(1:) as the VAX    *01 01140
*                                 does.                                *01 01150
*  VERSION 4(11) AS OF 23-FEB-93. 1. Finished typing all variables     *01 01160
*                                 2. Delinted                          *01 01170
*                                 3. Avoided possible compiler problems*01 01180
*                                    with DO loop variables in BREAK,  *01 01190
*                                    SPAN, and INTSCN                  *01 01200
*                                 4. Corrected TYPSTR:                 *01 01210
*                                    Was not allowing leading blanks as*01 01220
*                                      per documentation for FORTRAN   *01 01230
*                                      numbers                         *01 01240
*                                    Worked around AIX XL FORTRAN      *01 01250
*                                      "enhancement" which ignored     *01 01260
*                                      error branch in test for FORTRAN*01 01270
*                                      numbers                         *01 01280
*                                 5. Corrected RLSCN and DRLSCN:       *01 01290
*                                    Not allowing for a real number not*01 01300
*                                      followed by an exponent         *01 01310
*  VERSION 4(12) as of 01-Apr-93. Corrected RLSCN and DRLSCN - Attempt *01 01320
*                                   to divide by zero when period but  *01 01330
*                                   no decimal fraction                *01 01340
*          4(13)       31-Aug-94. Corrected NUMSTR - No check on        01 01350
*                                   integer being larger than string    01 01360
*          4(14)       03-Nov-95. Corrected error in RLSCN and DRLSCN   01 01370
*                                   when there was a trailing period.   01 01380
*          4(15)       01-Mar-96. Incorrect results returned in RLSCN   01 01390
*                                   and DRLSCN when a period followed   01 01400
*                                   by one number - Fixed               01 01410
*                                                                      *01 01420
*     REFER ALL COMMENTS AND INQUIRIES TO                              *01 01430
*     NATIONAL NUCLEAR DATA CENTER                                     *01 01440
*     BUILDING 197D                                                    *01 01450
*     BROOKHAVEN NATIONAL LABORATORY                                   *01 01460
*     UPTON, NEW YORK 11973                                            *01 01470
*     TELEPHONE 516-282-2901 COMM                                      *01 01480
*                   666-2901 FTS                                       *01 01490
*                                                                      *01 01500
************************************************************************01 01510
                                                                        01 01520
      INTEGER FUNCTION LENSTR (STRING)                                  01 01530
                                                                        01 01540
************************************************************************01 01550
*                                                                      *01 01560
*  COMPUTE LENGTH OF TEXT WITHIN STRING EXCLUSIVE OF TRAILING BLANK    *01 01570
*  OR NULL CHARACTERS (I.E., VARIABLE LENGTH STRINGS WHICH EXIST       *01 01580
*  WITHIN A FIXED LENGTH STRING AREA).                                 *01 01590
*                                                                      *01 01600
*                                                                      *01 01610
************************************************************************01 01620
                                                                        01 01630
************************************************************************01 01640
*                                                                      *01 01650
*                            DATA SECTION.                             *01 01660
*                                                                      *01 01670
************************************************************************01 01680
                                                                        01 01690
*  FUNCTION ARGUMENTS.                                                  01 01700
                                                                        01 01710
      CHARACTER*(*)     STRING                                          01 01720
                                                                        01 01730
*  LOCAL VARIABLES.                                                     01 01740
                                                                        01 01750
      INTEGER           L                                               01 01760
                                                                        01 01770
************************************************************************01 01780
*                                                                      *01 01790
*                          PROCEDURE SECTION.                          *01 01800
*                                                                      *01 01810
************************************************************************01 01820
                                                                        01 01830
      DO 10 L = LEN(STRING), 1, -1                                      01 01840
         IF (STRING(L:L) .NE. ' ' .AND.                                 01 01850
     1       ICHAR(STRING(L:L)).NE.0) THEN                              01 01860
            LENSTR = L                                                  01 01870
            RETURN                                                      01 01880
         ENDIF                                                          01 01890
   10 CONTINUE                                                          01 01900
      LENSTR = 0                                                        01 01910
      END                                                               01 01920
                                                                        02 00010
      SUBROUTINE REPCHR (STR, FROM, TO)                                 02 00020
                                                                        02 00030
************************************************************************02 00040
*                                                                      *02 00050
*  SCAN STR FOR AN OCCURRENCE OF ANY CHARACTER IN STRING FROM. REPLACE *02 00060
*  THAT CHARACTER WITH THE CORRESPONDING CHARACTER FROM STRING TO.     *02 00070
*                                                                      *02 00080
************************************************************************02 00090
                                                                        02 00100
************************************************************************02 00110
*                                                                      *02 00120
*                             DATA SECTION.                            *02 00130
*                                                                      *02 00140
************************************************************************02 00150
                                                                        02 00160
*  SUBROUTINE ARGUMENTS.                                                02 00170
                                                                        02 00180
      CHARACTER*(*)     STR, FROM, TO                                   02 00190
                                                                        02 00200
*  FUNCTION REFERENCES.                                                 02 00210
                                                                        02 00220
      INTEGER           BREAK                                           02 00230
      External          Break                                           02 00240
      Integer           INDEX                                           02 00250
      Intrinsic         INDEX                                           02 00260
                                                                        02 00270
*  LOCAL VARIABLES.                                                     02 00280
                                                                        02 00290
      INTEGER           I, J                                            02 00300
                                                                        02 00310
************************************************************************02 00320
*                                                                      *02 00330
*                          PROCEDURE SECTION.                          *02 00340
*                                                                      *02 00350
************************************************************************02 00360
                                                                        02 00370
      I = 0                                                             02 00380
   10 I = BREAK(STR, I+1, FROM)                                         02 00390
      IF (I .GT. LEN(STR)) RETURN                                       02 00400
      J = INDEX(FROM, STR(I:I))                                         02 00410
      STR(I:I) = TO(J:J)                                                02 00420
      GOTO 10                                                           02 00430
      END                                                               02 00440
                                                                        03 00010
      SUBROUTINE REPSTR (STR, FROM, TO)                                 03 00020
                                                                        03 00030
************************************************************************03 00040
*                                                                      *03 00050
*  REPLACE OCCURRENCES OF THE STRING FROM WHICH OCCUR IN THE STRING STR*03 00060
*  WITH THE STRING TO. IF TO = CHAR(0) (NULL) THEN JUST DELETE FROM.   *03 00070
*                                                                      *03 00080
*  02-mar-90.  Modified to avoid infinite loop when TO string contains *03 00090
*         FROM string. TWB                                             *03 00100
************************************************************************03 00110
                                                                        03 00120
************************************************************************03 00130
*                                                                      *03 00140
*                             DATA SECTION.                            *03 00150
*                                                                      *03 00160
************************************************************************03 00170
                                                                        03 00180
*  SUBROUTINE ARGUMENTS.                                                03 00190
                                                                        03 00200
      CHARACTER*(*)     STR, FROM, TO                                   03 00210
                                                                        03 00220
*  LOCAL VARIABLES.                                                     03 00230
                                                                        03 00240
      INTEGER           I,J                                             03 00250
                                                                        03 00260
*   INTRINSIC FUNCTIONS.                                                03 00270
                                                                        03 00280
      INTEGER LEN                                                       03 00290
      INTRINSIC LEN                                                     03 00300
                                                                        03 00310
*   EXTERNAL FUNCTIONS (From F77STR library).                           03 00320
                                                                        03 00330
      INTEGER INDEXF                                                    03 00340
      EXTERNAL INDEXF                                                   03 00350
                                                                        03 00360
************************************************************************03 00370
*                                                                      *03 00380
*                          PROCEDURE SECTION.                          *03 00390
*                                                                      *03 00400
************************************************************************03 00410
                                                                        03 00420
      J=1                                                               03 00430
  100 CONTINUE                                                          03 00440
      I = INDEXF(STR,J, FROM)                                           03 00450
      IF (I .EQ. 0) RETURN                                              03 00460
      J=I                                                               03 00470
      CALL DELSTR(STR, I, LEN(FROM))                                    03 00480
      IF (TO .NE. CHAR(0)) THEN                                         03 00490
         CALL ADDSTR(STR, I, TO)                                        03 00500
         J=I+LEN(TO)                                                    03 00510
         IF(J.GT.LEN(STR)) RETURN                                       03 00520
      ENDIF                                                             03 00530
      GOTO 100                                                          03 00540
      END                                                               03 00550
                                                                        04 00010
      SUBROUTINE ADDSTR (STRING, POS, NEW)                              04 00020
                                                                        04 00030
************************************************************************04 00040
*                                                                      *04 00050
*  ADD NEW TEXT TO STRING STARTING AT POSITION POS, EXTENDING RIGHT    *04 00060
*     HAND SEGMENT FURTHER TO THE RIGHT WITH TRUNCATION OF EXCESS      *04 00070
*     CHARACTERS.                                                      *04 00080
*                                                                      *04 00090
************************************************************************04 00100
                                                                        04 00110
************************************************************************04 00120
*                                                                      *04 00130
*                            DATA SECTION.                             *04 00140
*                                                                      *04 00150
************************************************************************04 00160
                                                                        04 00170
*  FUNCTION ARGUMENTS.                                                  04 00180
                                                                        04 00190
      CHARACTER*(*)     STRING                                          04 00200
      INTEGER           POS                                             04 00210
      CHARACTER*(*)     NEW                                             04 00220
                                                                        04 00230
*  FUNCTION REFERENCES.                                                 04 00240
                                                                        04 00250
      Integer           LEN                                             04 00260
      Intrinsic         LEN                                             04 00270
                                                                        04 00280
*  LOCAL VARIABLES.                                                     04 00290
                                                                        04 00300
      INTEGER           L                                               04 00310
*                          LENGTH OF RIGHT HAND SEGMENT OF STRING WHICH 04 00320
*                          WILL STILL APPEAR IN STRING.                 04 00330
      Integer           ln                                              04 00340
*                          Length of string to be added                 04 00350
      INTEGER           I                                               04 00360
*                          INDEX TO DO LOOP                             04 00370
                                                                        04 00380
************************************************************************04 00390
*                                                                      *04 00400
*                          PROCEDURE SECTION.                          *04 00410
*                                                                      *04 00420
************************************************************************04 00430
                                                                        04 00440
      L = LEN(STRING)                                                   04 00450
      LN=LEN(NEW)                                                       04 00460
      IF (POS .LT. 1 .OR. L .LT. POS) RETURN                            04 00470
      L = L - (POS - 1) - LN                                            04 00480
      IF (L .LE. 0) THEN                                                04 00490
         STRING(POS:) = NEW                                             04 00500
      ELSE                                                              04 00510
         DO 100 I=LEN(STRING),POS+LN,-1                                 04 00520
            STRING(I:I)=STRING(I-LN:I-LN)                               04 00530
  100 CONTINUE                                                          04 00540
         STRING(POS:POS+LN-1) = NEW                                     04 00550
      ENDIF                                                             04 00560
      END                                                               04 00570
                                                                        05 00010
      SUBROUTINE DELSTR (STRING, POS, SIZE)                             05 00020
                                                                        05 00030
************************************************************************05 00040
*                                                                      *05 00050
*  DELETE SIZE CHARACTERS FROM STRING STARTING AT POSITION POS.        *05 00060
*  FILL AT RIGHT WITH TRAILING BLANKS.                                 *05 00070
*                                                                      *05 00080
************************************************************************05 00090
                                                                        05 00100
************************************************************************05 00110
*                                                                      *05 00120
*                            DATA SECTION.                             *05 00130
*                                                                      *05 00140
************************************************************************05 00150
                                                                        05 00160
*  FUNCTION ARGUMENTS.                                                  05 00170
                                                                        05 00180
      CHARACTER*(*)     STRING                                          05 00190
      INTEGER           POS                                             05 00200
      INTEGER           SIZE                                            05 00210
                                                                        05 00220
*  COMMON BLOCKS.                                                       05 00230
                                                                        05 00240
*  FUNCTION REFERENCES.                                                 05 00250
                                                                        05 00260
      Integer           LEN                                             05 00270
      Intrinsic         LEN                                             05 00280
                                                                        05 00290
*  LOCAL VARIABLES.                                                     05 00300
                                                                        05 00310
      INTEGER           L                                               05 00320
*                          LENGTH OF RIGHT HAND SEGMENT OF STRING WHICH 05 00330
*                          WILL STILL APPEAR IN STRING.                 05 00340
      INTEGER           I                                               05 00350
*                          INDEX TO DO LOOP                             05 00360
************************************************************************05 00370
*                                                                      *05 00380
*                          PROCEDURE SECTION.                          *05 00390
*                                                                      *05 00400
************************************************************************05 00410
                                                                        05 00420
      L = LEN(STRING)                                                   05 00430
      IF (POS .LT. 1 .OR. L .LT. POS) RETURN                            05 00440
      L = L - (POS - 1) - SIZE                                          05 00450
      IF (L .LE. 0) THEN                                                05 00460
         STRING(POS:) = ' '                                             05 00470
      ELSE                                                              05 00480
         DO 100 I=POS,LEN(STRING)-SIZE                                  05 00490
            STRING(I:I)=STRING(I+SIZE:I+SIZE)                           05 00500
  100    CONTINUE                                                       05 00510
         STRING(LEN(STRING)-SIZE+1:)=' '                                05 00520
      ENDIF                                                             05 00530
      END                                                               05 00540
                                                                        06 00010
      SUBROUTINE SQZSTR (STRING, CHAR)                                  06 00020
                                                                        06 00030
************************************************************************06 00040
*                                                                      *06 00050
*  SQUEEZ OUT ALL OCCURRENCES OF CHAR FROM STRING.                     *06 00060
*                                                                      *06 00070
************************************************************************06 00080
                                                                        06 00090
************************************************************************06 00100
*                                                                      *06 00110
*                            DATA SECTION.                             *06 00120
*                                                                      *06 00130
************************************************************************06 00140
                                                                        06 00150
*  FUNCTION ARGUMENTS.                                                  06 00160
                                                                        06 00170
      CHARACTER*(*)     STRING                                          06 00180
      CHARACTER*1       CHAR                                            06 00190
                                                                        06 00200
*  FUNCTION REFERENCES.                                                 06 00210
                                                                        06 00220
      Integer           LEN                                             06 00230
      Intrinsic         LEN                                             06 00240
                                                                        06 00250
*  LOCAL VARIABLES.                                                     06 00260
                                                                        06 00270
      INTEGER           FROM                                            06 00280
      INTEGER           TO                                              06 00290
                                                                        06 00300
************************************************************************06 00310
*                                                                      *06 00320
*                          PROCEDURE SECTION.                          *06 00330
*                                                                      *06 00340
************************************************************************06 00350
                                                                        06 00360
      TO = 1                                                            06 00370
      DO 10 FROM = 1, LEN(STRING)                                       06 00380
         IF (STRING(FROM:FROM) .NE. CHAR) THEN                          06 00390
            IF (TO .NE. FROM) STRING(TO:TO) = STRING(FROM:FROM)         06 00400
            TO = TO + 1                                                 06 00410
         ENDIF                                                          06 00420
   10 CONTINUE                                                          06 00430
      IF (TO .EQ. FROM) RETURN                                          06 00440
      STRING(TO:) = ' '                                                 06 00450
      END                                                               06 00460
                                                                        07 00010
      INTEGER FUNCTION INDEXF (STRING, POS, SUB)                        07 00020
                                                                        07 00030
************************************************************************07 00040
*                                                                      *07 00050
*  SAME AS THE STANDARD FUNCTION INDEX EXCEPT THE THE SCAN STARTS AT   *07 00060
*  POSITION POS INSTEAD OF POSITION 1. STRING WILL BE SEARCHED FOR THE *07 00070
*  FIRST OCCURRANCE OF SUB AT OR AFTER POS. THE POSITION OF THE FIRST  *07 00080
*  CHARACTER OF SUB IN STRING WILL BE RETURNED, OR ELSE ZERO (0) WILL  *07 00090
*  BE RETURNED.                                                        *07 00100
*                                                                      *07 00110
************************************************************************07 00120
                                                                        07 00130
************************************************************************07 00140
*                                                                      *07 00150
*                            DATA SECTION.                             *07 00160
*                                                                      *07 00170
************************************************************************07 00180
                                                                        07 00190
*  FUNCTION ARGUMENTS.                                                  07 00200
                                                                        07 00210
      CHARACTER*(*)     STRING                                          07 00220
      INTEGER           POS                                             07 00230
      CHARACTER*(*)     SUB                                             07 00240
                                                                        07 00250
*  FUNCTION REFERENCES.                                                 07 00260
                                                                        07 00270
      Integer           INDEX                                           07 00280
      Intrinsic         INDEX                                           07 00290
                                                                        07 00300
************************************************************************07 00310
*                                                                      *07 00320
*                          PROCEDURE SECTION.                          *07 00330
*                                                                      *07 00340
************************************************************************07 00350
                                                                        07 00360
*  TEST THAT POS IS IN RANGE.                                           07 00370
                                                                        07 00380
      INDEXF = 0                                                        07 00390
      IF (POS .LT. 1 .OR. LEN(STRING) .LT. POS) RETURN                  07 00400
                                                                        07 00410
*  USE INDEX WITH SUB-STRING AND OFFSET.                                07 00420
                                                                        07 00430
      INDEXF = INDEX(STRING(POS:), SUB)                                 07 00440
      IF (INDEXF .NE. 0) INDEXF = INDEXF + POS - 1                      07 00450
      END                                                               07 00460
                                                                        08 00010
      INTEGER FUNCTION BREAK (STRING, POS, BRKSTR)                      08 00020
                                                                        08 00030
************************************************************************08 00040
*                                                                      *08 00050
*  SCANS STRING LOOKING FOR THE FIRST OCCURRENCE OF A CHARACTER (THE   *08 00060
*  BREAK CHARACTER) WHICH IS IN THE BREAK STRING.                      *08 00070
*                                                                      *08 00080
*  SCANNING BEGINS AT THE POSITION SPECIFIED BY POS AND CONTINUES TO   *08 00090
*  THE END OF THE STRING.                                              *08 00100
*                                                                      *08 00110
*  THE FUNCTION VALUE IS SET TO THE POSITION WITHIN THE STRING WHERE   *08 00120
*  THE BREAK CHARACTER IS FOUND.                                       *08 00130
*                                                                      *08 00140
*  IF THERE IS NO BREAK CHARACTER IN THE STRING, THE FUNCTION VALUE IS *08 00150
*  SET TO THE LENGTH OF THE STRING PLUS ONE.                           *08 00160
*                                                                      *08 00170
************************************************************************08 00180
                                                                        08 00190
************************************************************************08 00200
*                                                                      *08 00210
*                            DATA SECTION.                             *08 00220
*                                                                      *08 00230
************************************************************************08 00240
                                                                        08 00250
*  FUNCTION ARGUMENTS.                                                  08 00260
                                                                        08 00270
      CHARACTER*(*)     STRING                                          08 00280
      INTEGER           POS                                             08 00290
      CHARACTER*(*)     BRKSTR                                          08 00300
                                                                        08 00310
*  FUNCTION REFERENCES.                                                 08 00320
                                                                        08 00330
      Integer           LEN                                             08 00340
      Intrinsic         LEN                                             08 00350
                                                                        08 00360
*  LOCAL VARIABLES.                                                     08 00370
                                                                        08 00380
      INTEGER           B                                               08 00390
*                          WORKING VARIABLE FOR BREAK LOOP.             08 00400
      INTEGER           I                                               08 00410
*                          INDEX INTO BRKSTR.                           08 00420
      INTEGER           LBRK                                            08 00430
*                          LENGTH OF BRKSTR.                            08 00440
      INTEGER           LSTR                                            08 00450
*                          LENGTH OF STRING.                            08 00460
                                                                        08 00470
************************************************************************08 00480
*                                                                      *08 00490
*                          PROCEDURE SECTION.                          *08 00500
*                                                                      *08 00510
************************************************************************08 00520
                                                                        08 00530
*  SCAN FOR BREAK CHARACTER (IN BRKSTR).                                08 00540
                                                                        08 00550
      BREAK = POS                                                       08 00560
      LSTR = LEN(STRING)                                                08 00570
      IF (POS .LT. 1 .OR. LSTR .LT. POS) RETURN                         08 00580
      LBRK = LEN(BRKSTR)                                                08 00590
      DO 20 B = POS, LSTR                                               08 00600
         DO 10 I = 1, LBRK                                              08 00610
            IF (STRING(B:B) .EQ. BRKSTR(I:I)) THEN                      08 00620
               BREAK = B                                                08 00630
               RETURN                                                   08 00640
            ENDIF                                                       08 00650
   10    CONTINUE                                                       08 00660
   20 CONTINUE                                                          08 00670
C     Changed from BREAK = B to avoid possible compiler dependences     08 00680
C       (TWB. 930223)                                                   08 00690
      BREAK = lstr+1                                                    08 00700
      END                                                               08 00710
                                                                        09 00010
      INTEGER FUNCTION SPAN (STRING, POS, SPNSTR)                       09 00020
                                                                        09 00030
************************************************************************09 00040
*                                                                      *09 00050
*  SCANS STRING LOOKING FOR THE FIRST OCCURRENCE OF A CHARACTER (THE   *09 00060
*  BREAK CHARACTER) WHICH IS NOT IN THE SPAN STRING.                   *09 00070
*                                                                      *09 00080
*  SCANNING BEGINS AT THE POSITION SPECIFIED BY POS AND CONTINUES TO   *09 00090
*  THE END OF THE STRING.                                              *09 00100
*                                                                      *09 00110
*  THE FUNCTION VALUE IS SET TO THE POSITION WITHIN THE STRING WHERE   *09 00120
*  THE BREAK CHARACTER IS FOUND.                                       *09 00130
*                                                                      *09 00140
*  IF THERE IS NO BREAK CHARACTER IN THE STRING, THE FUNCTION VALUE IS *09 00150
*  SET TO THE LENGTH OF THE STRING PLUS ONE.                           *09 00160
*                                                                      *09 00170
************************************************************************09 00180
                                                                        09 00190
************************************************************************09 00200
*                                                                      *09 00210
*                            DATA SECTION.                             *09 00220
*                                                                      *09 00230
************************************************************************09 00240
                                                                        09 00250
*  FUNCTION ARGUMENTS.                                                  09 00260
                                                                        09 00270
      CHARACTER*(*)     STRING                                          09 00280
      INTEGER           POS                                             09 00290
      CHARACTER*(*)     SPNSTR                                          09 00300
                                                                        09 00310
*  FUNCTION REFERENCES.                                                 09 00320
                                                                        09 00330
      Integer           LEN                                             09 00340
      Intrinsic         LEN                                             09 00350
                                                                        09 00360
*  LOCAL VARIABLES.                                                     09 00370
                                                                        09 00380
      INTEGER           I                                               09 00390
*                          INDEX INTO SPNSTR.                           09 00400
      INTEGER           LSPN                                            09 00410
*                          LENGTH OF SPNSTR.                            09 00420
      INTEGER           LSTR                                            09 00430
*                          LENGTH OF STRING.                            09 00440
      INTEGER           S                                               09 00450
*                          WORKING VARIABLE FOR SPAN LOOP.              09 00460
                                                                        09 00470
************************************************************************09 00480
*                                                                      *09 00490
*                          PROCEDURE SECTION.                          *09 00500
*                                                                      *09 00510
************************************************************************09 00520
                                                                        09 00530
*  SCAN FOR BREAK CHARACTER (NOT IN SPNSTR).                            09 00540
                                                                        09 00550
      SPAN = POS                                                        09 00560
      LSTR = LEN(STRING)                                                09 00570
      IF (POS .LT. 1 .OR. LSTR .LT. POS) RETURN                         09 00580
      LSPN = LEN(SPNSTR)                                                09 00590
      DO 20 S = POS, LSTR                                               09 00600
         DO 10 I = 1, LSPN                                              09 00610
            IF (STRING(S:S) .EQ. SPNSTR(I:I)) GOTO 20                   09 00620
   10    CONTINUE                                                       09 00630
            SPAN = S                                                    09 00640
            RETURN                                                      09 00650
   20 CONTINUE                                                          09 00660
C     Changed from SPAN + S to avoid possible compiler dependences      09 00670
C       (TWB. 930223)                                                   09 00680
      SPAN = lstr+1                                                     09 00690
      END                                                               09 00700
                                                                        10 00010
      REAL FUNCTION VALSTR (STRING)                                     10 00020
                                                                        10 00030
************************************************************************10 00040
*                                                                      *10 00050
*  VALUE OF LEADING REAL NUMERIC STRING.                               *10 00060
*                                                                      *10 00070
************************************************************************10 00080
                                                                        10 00090
************************************************************************10 00100
*                                                                      *10 00110
*                            DATA SECTION.                             *10 00120
*                                                                      *10 00130
************************************************************************10 00140
                                                                        10 00150
*  FUNCTION ARGUMENTS.                                                  10 00160
                                                                        10 00170
      CHARACTER*(*)     STRING                                          10 00180
                                                                        10 00190
*  FUNCTION REFERENCES.                                                 10 00200
                                                                        10 00210
      INTEGER           RLSCN                                           10 00220
      External          Rlscn                                           10 00230
                                                                        10 00240
*  LOCAL VARIABLES.                                                     10 00250
                                                                        10 00260
      INTEGER           I                                               10 00270
      REAL              V                                               10 00280
                                                                        10 00290
************************************************************************10 00300
*                                                                      *10 00310
*                          PROCEDURE SECTION.                          *10 00320
*                                                                      *10 00330
************************************************************************10 00340
                                                                        10 00350
      I = RLSCN(STRING, 1, V)                                           10 00360
      VALSTR = V                                                        10 00370
      END                                                               10 00380
C                                                                       11 00010
      DOUBLE PRECISION FUNCTION DVALST (STRING)                         11 00020
                                                                        11 00030
************************************************************************11 00040
*                                                                      *11 00050
*  VALUE OF LEADING REAL NUMERIC STRING IN DOUBLE PRECISION.           *11 00060
*                                                                      *11 00070
************************************************************************11 00080
                                                                        11 00090
************************************************************************11 00100
*                                                                      *11 00110
*                            DATA SECTION.                             *11 00120
*                                                                      *11 00130
************************************************************************11 00140
                                                                        11 00150
*  FUNCTION ARGUMENTS.                                                  11 00160
                                                                        11 00170
      CHARACTER*(*)     STRING                                          11 00180
                                                                        11 00190
*  FUNCTION REFERENCES.                                                 11 00200
                                                                        11 00210
      INTEGER           DRLSCN                                          11 00220
      External          Drlscn                                          11 00230
                                                                        11 00240
*  LOCAL VARIABLES.                                                     11 00250
                                                                        11 00260
      INTEGER           I                                               11 00270
      DOUBLE PRECISION  V                                               11 00280
                                                                        11 00290
************************************************************************11 00300
*                                                                      *11 00310
*                          PROCEDURE SECTION.                          *11 00320
*                                                                      *11 00330
************************************************************************11 00340
                                                                        11 00350
      I = DRLSCN(STRING, 1, V)                                          11 00360
      DVALST = V                                                        11 00370
      END                                                               11 00380
                                                                        12 00010
      INTEGER FUNCTION RLSCN (STRING, POS, VALUE)                       12 00020
                                                                        12 00030
************************************************************************12 00040
*                                                                      *12 00050
*  SCANS STRING LOOKING FOR THE LEADING REAL NUMERIC STRING.           *12 00060
*                                                                      *12 00070
*  SCANNING BEGINS AT THE POSITION SPECIFIED BY POS AND CONTINUES TO   *12 00080
*  THE END OF THE STRING.                                              *12 00090
*                                                                      *12 00100
*  LEADING BLANKS ARE IGNORED.                                         *12 00110
*                                                                      *12 00120
*  THE NUMERIC STRING MUST HAVE THE FORM:                              *12 00130
*                                                                      *12 00140
*        [SIGN] D+ ['.' D*] ['E' [SIGN] D+]        OR                  *12 00150
*        [SIGN]     '.' D+  ['E' [SIGN] D+]                            *12 00160
*                                                                      *12 00170
*     WHERE SIGN IS '+' OR '-',                                        *12 00180
*        D* IS ZERO OR MORE DIGITS,                                    *12 00190
*        D+ IS ONE  OR MORE DIGITS,                                    *12 00200
*        '.' AND 'E' ARE LITERAL (ALSO ACCEPT LOWER CASE 'E'),         *12 00210
*        BRACKETS [, ] DELIMIT OPTIONAL SEQUENCES.                     *12 00220
*                                                                      *12 00230
*  VALUE IS SET TO THE NUMERIC VALUE OF THE STRING.                    *12 00240
*                                                                      *12 00250
*  THE FUNCTION VALUE IS SET TO THE POSITION WITHIN THE STRING WHERE   *12 00260
*  THE NUMERIC STRING ENDS PLUS ONE (I.E., THE BREAK CHARACTER).       *12 00270
*                                                                      *12 00280
************************************************************************12 00290
                                                                        12 00300
************************************************************************12 00310
*                                                                      *12 00320
*                            DATA SECTION.                             *12 00330
*                                                                      *12 00340
************************************************************************12 00350
                                                                        12 00360
*  FUNCTION ARGUMENTS.                                                  12 00370
                                                                        12 00380
      CHARACTER*(*)     STRING                                          12 00390
      INTEGER           POS                                             12 00400
      REAL              VALUE                                           12 00410
                                                                        12 00420
*  FUNCTION REFERENCES.                                                 12 00430
                                                                        12 00440
      INTEGER           INTSCN,Lenstr                                   12 00450
      External          Intscn,Lenstr                                   12 00460
      Integer           LEN                                             12 00470
      Intrinsic         LEN                                             12 00480
                                                                        12 00490
*  LOCAL VARIABLES.                                                     12 00500
                                                                        12 00510
      INTEGER           FRACT                                           12 00520
      INTEGER           INT                                             12 00530
      INTEGER           KFRACT                                          12 00540
      INTEGER           POWER                                           12 00550
      INTEGER           PTR                                             12 00560
      INTEGER           SIGN                                            12 00570
                                                                        12 00580
************************************************************************12 00590
*                                                                      *12 00600
*                          PROCEDURE SECTION.                          *12 00610
*                                                                      *12 00620
************************************************************************12 00630
                                                                        12 00640
*  CHECK POS.                                                           12 00650
                                                                        12 00660
      RLSCN = POS                                                       12 00670
      VALUE = 0.0                                                       12 00680
      IF (POS .LT. 1 .OR. LEN(STRING) .LT. POS) RETURN                  12 00690
                                                                        12 00700
*  SET UP WORKING VARIABLES.                                            12 00710
                                                                        12 00720
      INT = 0                                                           12 00730
      FRACT = 0                                                         12 00740
      KFRACT = 0                                                        12 00750
      POWER = 0                                                         12 00760
                                                                        12 00770
*  SKIP LEADING BLANKS.                                                 12 00780
                                                                        12 00790
   10 IF (STRING(RLSCN:RLSCN) .EQ. ' ') THEN                            12 00800
         RLSCN = RLSCN + 1                                              12 00810
         IF (RLSCN .GT. LEN(STRING)) RETURN                             12 00820
         GOTO 10                                                        12 00830
      ENDIF                                                             12 00840
                                                                        12 00850
*  LOOK FOR SIGN.                                                       12 00860
*    NOTE: SEPARATE CHECK FOR SIGN SINCE INTEGER PART MAY BE OMITTED.   12 00870
                                                                        12 00880
      SIGN = 0                                                          12 00890
      IF (STRING(RLSCN:RLSCN) .EQ. '+') THEN                            12 00900
         SIGN = +1                                                      12 00910
      ELSE IF (STRING(RLSCN:RLSCN) .EQ. '-') THEN                       12 00920
         SIGN = -1                                                      12 00930
      ENDIF                                                             12 00940
      IF (SIGN .NE. 0) RLSCN = RLSCN + 1                                12 00950
                                                                        12 00960
*  LOOK FOR INTEGER PART.                                               12 00970
                                                                        12 00980
      RLSCN = INTSCN(STRING, RLSCN, .FALSE., INT)                       12 00990
                                                                        12 01000
*  LOOK FOR FRACTION PART.                                              12 01010
                                                                        12 01020
      IF (RLSCN .GT. LEN(STRING)) GO TO 100                             12 01030
      IF (RLSCN .GT. POS + ABS(SIGN)) THEN                              12 01040
*      DETERMINE IF FIRST FORM OR SECOND FORM.                          12 01050
*       HANDLE FIRST FORM:  D+ ['.' D*]                                 12 01060
         IF (STRING(RLSCN:RLSCN) .EQ. '.') THEN                         12 01070
            RLSCN = RLSCN + 1                                           12 01080
            If(rlscn .LE. Lenstr(string))Then                           12 01090
               If(string(rlscn:rlscn) .NE. ' ')Then                     12 01100
                  PTR = INTSCN(STRING, RLSCN, .FALSE., FRACT)           12 01110
                  KFRACT = PTR - RLSCN                                  12 01120
                  RLSCN = PTR                                           12 01130
               EndIf                                                    12 01140
            EndIf                                                       12 01150
         ENDIF                                                          12 01160
      ELSE                                                              12 01170
*       HANDLE SECOND FORM:  '.' D+                                     12 01180
         IF (STRING(RLSCN:RLSCN) .NE. '.') THEN                         12 01190
*          IF '.' MISSING, THEN WE HAVE NOTHING.                        12 01200
            RLSCN = POS                                                 12 01210
            RETURN                                                      12 01220
         ELSE                                                           12 01230
            RLSCN = RLSCN + 1                                           12 01240
            PTR = INTSCN(STRING, RLSCN, .FALSE., FRACT)                 12 01250
            KFRACT = PTR - RLSCN                                        12 01260
            IF (KFRACT .EQ. 0) THEN                                     12 01270
*             IF FRACTION MISSING, THEN WE STILL HAVE NOTHING.          12 01280
               RLSCN = POS                                              12 01290
               RETURN                                                   12 01300
            ELSE                                                        12 01310
               RLSCN = PTR                                              12 01320
            ENDIF                                                       12 01330
         ENDIF                                                          12 01340
      ENDIF                                                             12 01350
                                                                        12 01360
*  LOOK FOR EXPONENT PART.                                              12 01370
                                                                        12 01380
      IF (RLSCN .GT. LEN(STRING)) GO TO 100                             12 01390
      IF (STRING(RLSCN:RLSCN) .EQ. 'E'                                  12 01400
     +      .OR. STRING(RLSCN:RLSCN) .EQ. 'e') THEN                     12 01410
         RLSCN = RLSCN + 1                                              12 01420
         PTR = INTSCN(STRING, RLSCN, .TRUE., POWER)                     12 01430
         IF (PTR-RLSCN .EQ. 0) THEN                                     12 01440
*          IF WE HAVE THE 'E' BUT NOTHING ELSE THEN WE ASSUME THAT THE  12 01450
*          'E' IS A TERMINATOR (E.G., 5.3EV) AND RETURN WHAT WE HAVE SO 12 01460
*          FAR (E.G., 5.3).                                             12 01470
            RLSCN = RLSCN - 1                                           12 01480
            VALUE = INT + FRACT / 10.0 ** KFRACT                        12 01490
            IF (SIGN .EQ. -1) VALUE = -VALUE                            12 01500
            RETURN                                                      12 01510
         ELSE                                                           12 01520
            RLSCN = PTR                                                 12 01530
         ENDIF                                                          12 01540
      ENDIF                                                             12 01550
                                                                        12 01560
*  COMPUTE REAL VALUE FROM ITS PARTS.                                   12 01570
                                                                        12 01580
  100 CONTINUE                                                          12 01590
      If(kfract .NE. 0)Then                                             12 01600
         VALUE = (INT + FRACT / 10.0 ** KFRACT) * 10.0 ** POWER         12 01610
      Else                                                              12 01620
         VALUE = INT * 10.0 ** POWER                                    12 01630
      Endif                                                             12 01640
      IF (SIGN .EQ. -1) VALUE = -VALUE                                  12 01650
      END                                                               12 01660
      INTEGER FUNCTION DRLSCN (STRING, POS, VALUE)                      13 00010
                                                                        13 00020
************************************************************************13 00030
*                                                                      *13 00040
*  SCANS STRING LOOKING FOR THE LEADING REAL NUMERIC STRING.           *13 00050
*  SAME AS RLSCN, BUT VALUE IS A DOUBLE PRECISION NUMBER.               13 00060
*                                                                      *13 00070
*  SCANNING BEGINS AT THE POSITION SPECIFIED BY POS AND CONTINUES TO   *13 00080
*  THE END OF THE STRING.                                              *13 00090
*                                                                      *13 00100
*  LEADING BLANKS ARE IGNORED.                                         *13 00110
*                                                                      *13 00120
*  THE NUMERIC STRING MUST HAVE THE FORM:                              *13 00130
*                                                                      *13 00140
*        [SIGN] D+ ['.' D*] ['E' [SIGN] D+]        OR                  *13 00150
*        [SIGN]     '.' D+  ['E' [SIGN] D+]                            *13 00160
*                                                                      *13 00170
*     WHERE SIGN IS '+' OR '-',                                        *13 00180
*        D* IS ZERO OR MORE DIGITS,                                    *13 00190
*        D+ IS ONE  OR MORE DIGITS,                                    *13 00200
*        '.' AND 'E' ARE LITERAL (ALSO ACCEPT LOWER CASE 'E'),         *13 00210
*        BRACKETS [, ] DELIMIT OPTIONAL SEQUENCES.                     *13 00220
*                                                                      *13 00230
*  VALUE IS SET TO THE NUMERIC VALUE OF THE STRING.                    *13 00240
*                                                                      *13 00250
*  THE FUNCTION VALUE IS SET TO THE POSITION WITHIN THE STRING WHERE   *13 00260
*  THE NUMERIC STRING ENDS PLUS ONE (I.E., THE BREAK CHARACTER).       *13 00270
*                                                                      *13 00280
************************************************************************13 00290
                                                                        13 00300
************************************************************************13 00310
*                                                                      *13 00320
*                            DATA SECTION.                             *13 00330
*                                                                      *13 00340
************************************************************************13 00350
                                                                        13 00360
*  FUNCTION ARGUMENTS.                                                  13 00370
                                                                        13 00380
      CHARACTER*(*)     STRING                                          13 00390
      INTEGER           POS                                             13 00400
      DOUBLE PRECISION  VALUE                                           13 00410
                                                                        13 00420
*  FUNCTION REFERENCES.                                                 13 00430
                                                                        13 00440
      INTEGER           INTSCN,Lenstr                                   13 00450
      External          Intscn,Lenstr                                   13 00460
      Integer           LEN                                             13 00470
      Intrinsic         LEN                                             13 00480
                                                                        13 00490
*  LOCAL VARIABLES.                                                     13 00500
                                                                        13 00510
      INTEGER           FRACT                                           13 00520
      INTEGER           INT                                             13 00530
      INTEGER           KFRACT                                          13 00540
      INTEGER           POWER                                           13 00550
      INTEGER           PTR                                             13 00560
      INTEGER           SIGN                                            13 00570
                                                                        13 00580
************************************************************************13 00590
*                                                                      *13 00600
*                          PROCEDURE SECTION.                          *13 00610
*                                                                      *13 00620
************************************************************************13 00630
                                                                        13 00640
*  CHECK POS.                                                           13 00650
                                                                        13 00660
      DRLSCN = POS                                                      13 00670
      VALUE = 0.0                                                       13 00680
      IF (POS .LT. 1 .OR. LEN(STRING) .LT. POS) RETURN                  13 00690
                                                                        13 00700
*  SET UP WORKING VARIABLES.                                            13 00710
                                                                        13 00720
      INT = 0                                                           13 00730
      FRACT = 0                                                         13 00740
      KFRACT = 0                                                        13 00750
      POWER = 0                                                         13 00760
                                                                        13 00770
*  SKIP LEADING BLANKS.                                                 13 00780
                                                                        13 00790
   10 IF (STRING(DRLSCN:DRLSCN) .EQ. ' ') THEN                          13 00800
         DRLSCN = DRLSCN + 1                                            13 00810
         IF (DRLSCN .GT. LEN(STRING)) RETURN                            13 00820
         GOTO 10                                                        13 00830
      ENDIF                                                             13 00840
                                                                        13 00850
*  LOOK FOR SIGN.                                                       13 00860
*    NOTE: SEPARATE CHECK FOR SIGN SINCE INTEGER PART MAY BE OMITTED.   13 00870
                                                                        13 00880
      SIGN = 0                                                          13 00890
      IF (STRING(DRLSCN:DRLSCN) .EQ. '+') THEN                          13 00900
         SIGN = +1                                                      13 00910
      ELSE IF (STRING(DRLSCN:DRLSCN) .EQ. '-') THEN                     13 00920
         SIGN = -1                                                      13 00930
      ENDIF                                                             13 00940
      IF (SIGN .NE. 0) DRLSCN = DRLSCN + 1                              13 00950
                                                                        13 00960
*  LOOK FOR INTEGER PART.                                               13 00970
                                                                        13 00980
      DRLSCN = INTSCN(STRING, DRLSCN, .FALSE., INT)                     13 00990
                                                                        13 01000
*  LOOK FOR FRACTION PART.                                              13 01010
                                                                        13 01020
      IF (DRLSCN .GT. LEN(STRING)) GO TO 100                            13 01030
      IF (DRLSCN .GT. POS + ABS(SIGN)) THEN                             13 01040
*      DETERMINE IF FIRST FORM OR SECOND FORM.                          13 01050
*       HANDLE FIRST FORM:  D+ ['.' D*]                                 13 01060
         IF (STRING(DRLSCN:DRLSCN) .EQ. '.') THEN                       13 01070
            DRLSCN = DRLSCN + 1                                         13 01080
            If(drlscn .LE. Lenstr(string))Then                          13 01090
               If(string(drlscn:drlscn) .NE. ' ')Then                   13 01100
                  PTR = INTSCN(STRING, DRLSCN, .FALSE., FRACT)          13 01110
                  KFRACT = PTR - DRLSCN                                 13 01120
                  DRLSCN = PTR                                          13 01130
               EndIf                                                    13 01140
            EndIf                                                       13 01150
         ENDIF                                                          13 01160
      ELSE                                                              13 01170
*       HANDLE SECOND FORM:  '.' D+                                     13 01180
         IF (STRING(DRLSCN:DRLSCN) .NE. '.') THEN                       13 01190
*          IF '.' MISSING, THEN WE HAVE NOTHING.                        13 01200
            DRLSCN = POS                                                13 01210
            RETURN                                                      13 01220
         ELSE                                                           13 01230
            DRLSCN = DRLSCN + 1                                         13 01240
            PTR = INTSCN(STRING, DRLSCN, .FALSE., FRACT)                13 01250
            KFRACT = PTR - DRLSCN                                       13 01260
            IF (KFRACT .EQ. 0) THEN                                     13 01270
*             IF FRACTION MISSING, THEN WE STILL HAVE NOTHING.          13 01280
               DRLSCN = POS                                             13 01290
               RETURN                                                   13 01300
            ELSE                                                        13 01310
               DRLSCN = PTR                                             13 01320
            ENDIF                                                       13 01330
         ENDIF                                                          13 01340
      ENDIF                                                             13 01350
                                                                        13 01360
*  LOOK FOR EXPONENT PART.                                              13 01370
                                                                        13 01380
      IF (DRLSCN .GT. LEN(STRING)) GO TO 100                            13 01390
      IF (STRING(DRLSCN:DRLSCN) .EQ. 'E'                                13 01400
     +      .OR. STRING(DRLSCN:DRLSCN) .EQ. 'e') THEN                   13 01410
         DRLSCN = DRLSCN + 1                                            13 01420
         PTR = INTSCN(STRING, DRLSCN, .TRUE., POWER)                    13 01430
         IF (PTR-DRLSCN .EQ. 0) THEN                                    13 01440
*          IF WE HAVE THE 'E' BUT NOTHING ELSE THEN WE ASSUME THAT THE  13 01450
*          'E' IS A TERMINATOR (E.G., 5.3EV) AND RETURN WHAT WE HAVE SO 13 01460
*          FAR (E.G., 5.3).                                             13 01470
            DRLSCN = DRLSCN - 1                                         13 01480
            VALUE = INT + FRACT / 10.0 ** KFRACT                        13 01490
            IF (SIGN .EQ. -1) VALUE = -VALUE                            13 01500
            RETURN                                                      13 01510
         ELSE                                                           13 01520
            DRLSCN = PTR                                                13 01530
         ENDIF                                                          13 01540
      ENDIF                                                             13 01550
                                                                        13 01560
*  COMPUTE REAL VALUE FROM ITS PARTS.                                   13 01570
                                                                        13 01580
  100 CONTINUE                                                          13 01590
      If(kfract .NE. 0)Then                                             13 01600
         VALUE = (INT + FRACT / 10.0 ** KFRACT) * 10.0 ** POWER         13 01610
      Else                                                              13 01620
         VALUE = INT * 10.0 ** POWER                                    13 01630
      Endif                                                             13 01640
      IF (SIGN .EQ. -1) VALUE = -VALUE                                  13 01650
      END                                                               13 01660
                                                                        14 00010
      INTEGER FUNCTION IVLSTR (STRING)                                  14 00020
                                                                        14 00030
************************************************************************14 00040
*                                                                      *14 00050
*  VALUE OF LEADING INTEGER STRING.                                    *14 00060
*                                                                      *14 00070
************************************************************************14 00080
                                                                        14 00090
************************************************************************14 00100
*                                                                      *14 00110
*                            DATA SECTION.                             *14 00120
*                                                                      *14 00130
************************************************************************14 00140
                                                                        14 00150
*  FUNCTION ARGUMENTS.                                                  14 00160
                                                                        14 00170
      CHARACTER*(*)     STRING                                          14 00180
                                                                        14 00190
*  FUNCTION REFERENCES.                                                 14 00200
                                                                        14 00210
      INTEGER           INTSCN                                          14 00220
      External          Intscn                                          14 00230
                                                                        14 00240
*  LOCAL VARIABLES.                                                     14 00250
                                                                        14 00260
      INTEGER           I                                               14 00270
      INTEGER           IV                                              14 00280
                                                                        14 00290
************************************************************************14 00300
*                                                                      *14 00310
*                          PROCEDURE SECTION.                          *14 00320
*                                                                      *14 00330
************************************************************************14 00340
                                                                        14 00350
      I = INTSCN(STRING, 1, .TRUE., IV)                                 14 00360
      IVLSTR = IV                                                       14 00370
      END                                                               14 00380
                                                                        15 00010
      INTEGER FUNCTION INTSCN (STRING, POS, SIGNED, VALUE)              15 00020
                                                                        15 00030
************************************************************************15 00040
*                                                                      *15 00050
*  SCANS STRING LOOKING FOR THE LEADING INTEGER STRING.                *15 00060
*                                                                      *15 00070
*  SCANNING BEGINS AT THE POSITION SPECIFIED BY POS AND CONTINUES TO   *15 00080
*  THE END OF THE STRING.                                              *15 00090
*                                                                      *15 00100
*  LEADING BLANKS ARE IGNORED.                                         *15 00110
*                                                                      *15 00120
*  THE SEARCH MAY BE FOR A SIGNED (SIGNED = .TRUE.) OR UNSIGNED (SIGNED*15 00130
*  = .FALSE.) INTEGER VALUE.  IF SIGNED, LEADING PLUS (+) OR MINUS (-) *15 00140
*  IS ALLOWED.  IF UNSIGNED, THEY WILL TERMINATE THE SCAN AS THEY ARE  *15 00150
*  INVALID FOR AN UNSIGNED INTEGER.                                    *15 00160
*                                                                      *15 00170
*  VALUE IS SET TO THE NUMERIC VALUE OF THE INTEGER STRING.            *15 00180
*                                                                      *15 00190
*  THE FUNCTION VALUE IS SET TO THE POSITION WITHIN THE STRING WHERE   *15 00200
*  THE INTEGER STRING ENDS PLUS ONE (I.E., THE BREAK CHARACTER).       *15 00210
*                                                                      *15 00220
************************************************************************15 00230
                                                                        15 00240
************************************************************************15 00250
*                                                                      *15 00260
*                            DATA SECTION.                             *15 00270
*                                                                      *15 00280
************************************************************************15 00290
                                                                        15 00300
*  FUNCTION ARGUMENTS.                                                  15 00310
                                                                        15 00320
      CHARACTER*(*)     STRING                                          15 00330
      INTEGER           POS                                             15 00340
      LOGICAL           SIGNED                                          15 00350
      INTEGER           VALUE                                           15 00360
                                                                        15 00370
                                                                        15 00380
*  Function references.                                                 15 00390
                                                                        15 00400
      Integer           Lenstr                                          15 00410
      External          Lenstr                                          15 00420
      Integer           LEN                                             15 00430
      Intrinsic         LEN                                             15 00440
                                                                        15 00450
*  LOCAL VARIABLES.                                                     15 00460
                                                                        15 00470
      INTEGER           DIGIT                                           15 00480
*                          STORES CURRENT DIGIT VALUE FOR SCAN.         15 00490
      INTEGER           SIGN                                            15 00500
*                          SIGN (+1 OR -1) IF SIGNED = .TRUE..          15 00510
                                                                        15 00520
************************************************************************15 00530
*                                                                      *15 00540
*                          PROCEDURE SECTION.                          *15 00550
*                                                                      *15 00560
************************************************************************15 00570
                                                                        15 00580
*  CHECK POS.                                                           15 00590
                                                                        15 00600
      INTSCN = POS                                                      15 00610
      VALUE = 0                                                         15 00620
      IF (POS .LT. 1 .OR. LEN(STRING) .LT. POS) RETURN                  15 00630
                                                                        15 00640
*  SKIP LEADING BLANKS.                                                 15 00650
                                                                        15 00660
    5 IF (STRING(INTSCN:INTSCN) .EQ. ' ') THEN                          15 00670
         INTSCN = INTSCN + 1                                            15 00680
         IF (INTSCN .GT. LEN(STRING)) RETURN                            15 00690
         GOTO 5                                                         15 00700
      ENDIF                                                             15 00710
                                                                        15 00720
*  IF SIGNED, CHECK FOR SIGN.                                           15 00730
                                                                        15 00740
      SIGN = 0                                                          15 00750
      IF (SIGNED) THEN                                                  15 00760
         IF (STRING(INTSCN:INTSCN) .EQ. '+') THEN                       15 00770
            SIGN = +1                                                   15 00780
         ELSE IF (STRING(INTSCN:INTSCN) .EQ. '-') THEN                  15 00790
            SIGN = -1                                                   15 00800
         ENDIF                                                          15 00810
         IF (SIGN .NE. 0) INTSCN = INTSCN + 1                           15 00820
C                                                                       15 00830
C   IF sign is the last char in the field (with no integer following it)15 00840
c   INTSCN value is left as POS or at the end of leading blanks.        15 00850
c                                                                       15 00860
         IF (INTSCN .GT. LENSTR(STRING)) THEN                           15 00870
            INTSCN=INTSCN-1                                             15 00880
            RETURN                                                      15 00890
         ENDIF                                                          15 00900
      ENDIF                                                             15 00910
                                                                        15 00920
*  PROCESS DIGIT STRING.                                                15 00930
                                                                        15 00940
      DO 10 INTSCN = INTSCN, LEN(STRING)                                15 00950
         DIGIT = ICHAR(STRING(INTSCN:INTSCN)) - ICHAR('0')              15 00960
         IF (DIGIT .LT. 0 .OR. 9 .LT. DIGIT) GOTO 20                    15 00970
         VALUE = VALUE * 10 + DIGIT                                     15 00980
   10 CONTINUE                                                          15 00990
C     Explicitly defined intscn to avoid possible compiler dependences  15 01000
C       (TWB. 930223)                                                   15 01010
      intscn=LEN(string)+1                                              15 01020
                                                                        15 01030
*  ADJUST SIGN.                                                         15 01040
                                                                        15 01050
   20 IF (SIGNED .AND. SIGN .EQ. -1) VALUE = -VALUE                     15 01060
      END                                                               15 01070
                                                                        16 00010
      SUBROUTINE NUMSTR (NUM, STR)                                      16 00020
                                                                        16 00030
************************************************************************16 00040
*                                                                      *16 00050
*  CONVERT THE INTEGER NUM INTO CHARACTER FORMAT (INTO STR).           *16 00060
*                                                                      *16 00070
************************************************************************16 00080
                                                                        16 00090
************************************************************************16 00100
*                                                                      *16 00110
*                            DATA SECTION.                             *16 00120
*                                                                      *16 00130
************************************************************************16 00140
                                                                        16 00150
*  SUBROUTINE ARGUMENTS.                                                16 00160
                                                                        16 00170
      INTEGER           NUM                                             16 00180
      CHARACTER*(*)     STR                                             16 00190
                                                                        16 00200
*  FUNCTION REFERENCES.                                                 16 00210
                                                                        16 00220
      Integer Lenstr                                                    16 00230
      External Lenstr                                                   16 00240
C                                                                       16 00250
      Integer LEN                                                       16 00260
      Intrinsic LEN                                                     16 00270
                                                                        16 00280
*  LOCAL VARIABLES.                                                     16 00290
                                                                        16 00300
      CHARACTER*5       FMT                                             16 00310
      Character*11 wrkstr,stars                                         16 00320
C                                                                       16 00330
      Data stars/'***********'/                                         16 00340
                                                                        16 00350
*  FORMAT STATEMENTS.                                                   16 00360
                                                                        16 00370
    1 FORMAT('(I', I2.2, ')')                                           16 00380
                                                                        16 00390
************************************************************************16 00400
*                                                                      *16 00410
*                          PROCEDURE SECTION.                          *16 00420
*                                                                      *16 00430
************************************************************************16 00440
                                                                        16 00450
      WRITE (UNIT=FMT,FMT=1) LEN(wrkstr)                                16 00460
      WRITE (UNIT=wrkstr,FMT=FMT,ERR=100) NUM                           16 00470
      Call Lbsup(wrkstr)                                                16 00480
      If(LEN(str) .GE. Lenstr(wrkstr))Then                              16 00490
         str=wrkstr                                                     16 00500
         Call Padlft(str,LEN(str))                                      16 00510
         Return                                                         16 00520
      EndIf                                                             16 00530
100   Continue                                                          16 00540
      str=stars                                                         16 00550
      Return                                                            16 00560
      END                                                               16 00570
      SUBROUTINE LBSUP(STR)                                             17 00010
C                                                                       17 00020
C     TAKES AWAY LEADING BLANKS.                                        17 00030
C     DUMMY ARGUMENT:                                                   17 00040
C     STR   THE STRING (ASSIGNED)                                       17 00050
C                                                                       17 00060
      Integer Lenstr                                                    17 00070
      External Lenstr                                                   17 00080
C                                                                       17 00090
      CHARACTER STR*(*)                                                 17 00100
      Integer i,lc                                                      17 00110
                                                                        17 00120
      LC=LENSTR(STR)                                                    17 00130
      DO 10 I=1,LC                                                      17 00140
         IF(STR(1:1).NE.' ') GO TO 90                                   17 00150
         STR=STR(2:)                                                    17 00160
10       CONTINUE                                                       17 00170
90    RETURN                                                            17 00180
      END                                                               17 00190
      SUBROUTINE PADLFT(STR,L)                                          18 00010
C                                                                       18 00020
C     MAKE STR L CHARACTERS LONG BY EITHER TAKING AWAY BLANKS OR        18 00030
C     FILLING WITH BLANKS TO THE LEFT.                                  18 00040
C     DUMMY ARGUMENTS:                                                  18 00050
C     STR   THE STRING (ASSIGNED)                                       18 00060
C     L     WANTED CURRENT LENGTH OF STR                                18 00070
C                                                                       18 00080
      CHARACTER*(*) STR                                                 18 00090
      Integer l                                                         18 00100
C                                                                       18 00110
      Integer Lenstr                                                    18 00120
      External Lenstr                                                   18 00130
C                                                                       18 00140
      CHARACTER*1   TEMP                                                18 00150
      Integer i,lc                                                      18 00160
C                                                                       18 00170
      IF(L.GT.LEN(STR)) RETURN                                          18 00180
      LC=LENSTR(STR)                                                    18 00190
      IF(LC.GE.L) RETURN                                                18 00200
      DO 100 I=1,LC                                                     18 00210
         TEMP=STR(LC-I+1:LC-I+1)                                        18 00220
         STR(L-I+1:L-I+1)=TEMP                                          18 00230
  100 CONTINUE                                                          18 00240
      STR(1:L-LC)=' '                                                   18 00250
      RETURN                                                            18 00260
      END                                                               18 00270
                                                                        19 00010
      INTEGER FUNCTION TYPSTR (STRING)                                  19 00020
                                                                        19 00030
************************************************************************19 00040
*                                                                      *19 00050
*  DETERMINE THE TYPE OF THE STRING:                                   *19 00060
*     0 = BLANK.                                                       *19 00070
*     1 = NUMERIC (0 - 9 ONLY).                                        *19 00080
*     2 = ALPHA (A - Z (UPPER CASE) ONLY).                             *19 00090
*    -1 = MIXED OR OTHER.                                              *19 00100
*    -2 = FORTRAN NUMBER                                               *19 00110
*                                                                      *19 00120
*    Trailing blanks are ignored but beginning blanks blanks count as  *19 00130
*    non-numeric, non-alpha character, except that for fortran number  *19 00140
*    beginning blanks are also allowed.                                *19 00150
*                                                                      *19 00160
************************************************************************19 00170
                                                                        19 00180
************************************************************************19 00190
*                                                                      *19 00200
*                            DATA SECTION.                             *19 00210
*                                                                      *19 00220
************************************************************************19 00230
                                                                        19 00240
*  FUNCTION ARGUMENTS.                                                  19 00250
                                                                        19 00260
      CHARACTER*(*)     STRING                                          19 00270
                                                                        19 00280
*  FUNCTION REFERENCES.                                                 19 00290
                                                                        19 00300
      Integer Break,Indexf,Intscn,Lenstr,Rlscn                          19 00310
      External Break,Indexf,Intscn,Lenstr,Rlscn                         19 00320
      Integer INDEX                                                     19 00330
      Logical LLE,LLT                                                   19 00340
      Intrinsic INDEX                                                   19 00350
                                                                        19 00360
*  LOCAL VARIABLES.                                                     19 00370
                                                                        19 00380
      INTEGER           I,istart,ix,lstr                                19 00390
      CHARACTER         CHR                                             19 00400
C      CHARACTER*20      TFMT                                           19 00410
C      CHARACTER*5       TEMP                                           19 00420
      Real              X                                               19 00430
                                                                        19 00440
************************************************************************19 00450
*                                                                      *19 00460
*                          PROCEDURE SECTION.                          *19 00470
*                                                                      *19 00480
************************************************************************19 00490
                                                                        19 00500
*  FIND TYPE OF FIRST CHARACTER, THEN VERIFY REST OF STRING.            19 00510
                                                                        19 00520
      CHR = STRING(1:1)                                                 19 00530
      LSTR=LENSTR(STRING)                                               19 00540
                                                                        19 00550
*  BLANK.                                                               19 00560
                                                                        19 00570
      IF (CHR .EQ. ' ') THEN                                            19 00580
         DO 10 I = 2, LEN(STRING)                                       19 00590
            IF (STRING(I:I) .NE. ' ') THEN                              19 00600
               GO TO 100                                                19 00610
            ENDIF                                                       19 00620
   10    CONTINUE                                                       19 00630
         TYPSTR = 0                                                     19 00640
                                                                        19 00650
*  NUMERIC.                                                             19 00660
                                                                        19 00670
      ELSE IF ('0' .LE. CHR .AND. CHR .LE. '9') THEN                    19 00680
         DO 20 I = 2, LSTR                                              19 00690
            CHR = STRING(I:I)                                           19 00700
            IF (CHR .LT. '0' .OR. '9' .LT. CHR) THEN                    19 00710
               GO TO 100                                                19 00720
            ENDIF                                                       19 00730
   20    CONTINUE                                                       19 00740
         TYPSTR = 1                                                     19 00750
                                                                        19 00760
*  ALPHABETIC.                                                          19 00770
                                                                        19 00780
      ELSE IF (LLE('A', STRING(1:1)) .AND. LLE(STRING(1:1), 'Z')) THEN  19 00790
         DO 30 I = 2, LSTR                                              19 00800
            IF (LLT(STRING(I:I), 'A') .OR. LLT('Z', STRING(I:I))) THEN  19 00810
               GO TO 100                                                19 00820
            ENDIF                                                       19 00830
   30    CONTINUE                                                       19 00840
         TYPSTR = 2                                                     19 00850
                                                                        19 00860
*  OTHER.                                                               19 00870
                                                                        19 00880
      ELSE                                                              19 00890
         GO TO 100                                                      19 00900
      ENDIF                                                             19 00910
      RETURN                                                            19 00920
C                                                                       19 00930
C   alpha, number, etc are mixed.                                       19 00940
C   check if it is a fortran readable number                            19 00950
C                                                                       19 00960
100   Continue                                                          19 00970
C      NCHAR=LSTR/10+1                                                  19 00980
C      WRITE(TEMP,'(I5)') LSTR                                          19 00990
C                                                                       19 01000
C   see if it is a real no                                              19 01010
C                                                                       19 01020
C     CHECK FOR IMBEDDED "," or " " (fortran delimiter)                 19 01030
      IF(INDEX(STRING,',') .GT.0) GO TO 140                             19 01040
      istart=index(string(1:lstr),' ')                                  19 01050
      If(istart .GT. 1)Goto 140                                         19 01060
C     Not allowing leading blanks although it should for a FORTRAN numbe19 01070
C       (TWB. 930222)                                                   19 01080
      If(istart .GT. 0)Then                                             19 01090
         Do 130 istart=istart+1,lstr                                    19 01100
            If(string(istart:istart) .NE. ' ')GoTo 135                  19 01110
130      Continue                                                       19 01120
135      Continue                                                       19 01130
      Else                                                              19 01140
         istart=1                                                       19 01150
      Endif                                                             19 01160
C      IF(INDEX(STRING(1:LSTR),' ').GT.0) GO TO 140                     19 01170
C     AIX XL FORTRAN compiler treats non-FORTRAN number characters as   19 01180
C       zero and issues a warning instead of implementing the ERR branch19 01190
C       (TWB. 930222)                                                   19 01200
      If(INDEX(string(istart:lstr),'E') .GT. 0                          19 01210
     2  .OR. INDEX(string(istart:lstr),'.') .GT. 0)Then                 19 01220
         i=rlscn(string(1:lstr),istart,X)                               19 01230
         If(i .GT. lstr)Then                                            19 01240
            typstr=-2                                                   19 01250
            RETURN                                                      19 01260
         Endif                                                          19 01270
      Else                                                              19 01280
         i=INDEX(string(istart:lstr),'+')+INDEX(string(istart:lstr),'-')19 01290
         If(i .GT. 1)GoTo 140                                           19 01300
         If(i .EQ. 1)Then                                               19 01310
            i=Intscn(string(1:lstr),istart,.TRUE.,ix)                   19 01320
         Else                                                           19 01330
            i=Intscn(string(1:lstr),istart,.FALSE.,ix)                  19 01340
         Endif                                                          19 01350
         If(i .GT. lstr)Then                                            19 01360
            typstr=-2                                                   19 01370
            RETURN                                                      19 01380
         Endif                                                          19 01390
      Endif                                                             19 01400
C      TFMT='(F'//TEMP(5-NCHAR+1:)//'.0)'                               19 01410
C      READ(STRING,TFMT,ERR=140) X                                      19 01420
C      TYPSTR=-2                                                        19 01430
C      RETURN                                                           19 01440
C                                                                       19 01450
C   not fortran acceptable number                                       19 01460
c                                                                       19 01470
  140 Continue                                                          19 01480
      TYPSTR=-1                                                         19 01490
      RETURN                                                            19 01500
      END                                                               19 01510
************************************************************************20 00010
*                                                                      *20 00020
*  SUBPROGRAM LIBRARY NSDCNV.                                          *20 00030
*     F77 VERSION OF CONVERSION ROUTINES.                              *20 00040
*                                                                      *20 00050
*  VERSION 1(00).                 SPLIT FROM NSDLIB.                   *20 00060
*  VERSION 2                      ADDED CNVU2S.                        *20 00070
*  VERSION 2(01)                  Added Double precision ENTRY's       *20 00080
*                                 DCNVSU, DCNVUS.                      *20 00090
*                                 SCALDX and SCALX became doubple prec *20 00100
*                                 CNVS2U-SDX need not be a pure integer*20 00110
*  VERSION 2(02)                  CNVU2S ABS to DABS etc.              *20 00120
*  VERSION 2(03)                  Add SUPEMB subroutine.               *20 00130
*  Version 2(04) as of 23-Feb-93. Completed typing all variables       *20 00140
*          2(05)       09-Feb-95. 1. Corrected CNVU2S                   20 00150
*                                   Format 2 was not working as         20 00160
*                                     designed. Corrected this. Also,   20 00170
*                                     LENDX now to specify number of    20 00180
*                                     significant digits.               20 00190
*                                   Integer overflow errors for         20 00200
*                                     extremely precise numbers - added 20 00210
*                                     check for this.                   20 00220
*                                 2. Added Logical Function IOVRFLW to  20 00230
*                                   check for possible integer overflow.20 00240
*                                 3. Corrected KNVIX. Increased TEMP    20 00250
*                                   from 12 to 20 characters to avoid   20 00260
*                                   truncation of final result.         20 00270
*                                 4. Corrected SCALX. Roundoff problem  20 00280
*                                   due to mixture of REAL*4 and REAL*8 20 00290
*                                   in line 20.                         20 00300
*          2(06)       10-Feb-95. Corrected line 11 of IOVRFLW for      20 00310
*                                   compiler-dependent problem and lines20 00320
*                                   10 and 11 for FORTRAN-dependent     20 00330
*                                   double precision problems           20 00340
*          2(07)       13-Feb-95. Corrected line 11 of IOVRFLW for      20 00350
*                                   compiler-dependent problem          20 00360
*          2(08)       04-Apr-95. Corrected line 11 of IOVRFLW for typo 20 00370
*                                   error                               20 00380
*                                                                      *20 00390
*     REFER ALL COMMENTS AND INQUIRIES TO                              *20 00400
*     NATIONAL NUCLEAR DATA CENTER                                     *20 00410
*     BUILDING 197D                                                    *20 00420
*     BROOKHAVEN NATIONAL LABORATORY                                   *20 00430
*     UPTON, NEW YORK 11973                                            *20 00440
*     TELEPHONE 516-282-2901 COMM                                      *20 00450
*                   666-2901 FTS                                       *20 00460
*                                                                      *20 00470
************************************************************************20 00480
                                                                        20 00490
      SUBROUTINE CNVS2U (SX, SDX, Y, DY)                                20 00500
                                                                        20 00510
************************************************************************20 00520
*                                                                      *20 00530
*  CONVERT SX AND SDX INTO TWO REAL NUMBERS X AND DX. (CNVS2U)         *20 00540
*    "              "          DOUBLE PREC REAL NUMBERS.  (DCNVSU)     *20 00550
*                                                                      *20 00560
************************************************************************20 00570
                                                                        20 00580
*  SUBROUTINE ARGUMENTS.                                                20 00590
                                                                        20 00600
      CHARACTER*(*)     SX, SDX                                         20 00610
      DOUBLE PRECISION  X, DX                                           20 00620
      REAL              Y, DY                                           20 00630
                                                                        20 00640
*  FUNCTION REFERENCES.                                                 20 00650
                                                                        20 00660
      INTEGER           IVLSTR                                          20 00670
      INTEGER           LENSTR                                          20 00680
      DOUBLE PRECISION  DVALST                                          20 00690
      External          Ivlstr,Lenstr,Dvalst                            20 00700
                                                                        20 00710
*  LOCAL VARIABLES.                                                     20 00720
                                                                        20 00730
      INTEGER           EXP                                             20 00740
      INTEGER           IDOT                                            20 00750
      INTEGER           IEXP                                            20 00760
      DOUBLE PRECISION  POWER                                           20 00770
      INTEGER           R                                               20 00780
      CHARACTER*24      TX                                              20 00790
      CHARACTER*24      TDX                                             20 00800
      DOUBLE PRECISION  Z,DZ                                            20 00810
      LOGICAL           LSINGL                                          20 00820
C                                                                       20 00830
C   ENTRY point for CNVS2U                                              20 00840
C                                                                       20 00850
      LSINGL=.TRUE.                                                     20 00860
      Y=0.                                                              20 00870
      DY=0.                                                             20 00880
      GO TO 100                                                         20 00890
C                                                                       20 00900
C   ENTRY point for DCNVSU   double precision                           20 00910
C                                                                       20 00920
      ENTRY DCNVSU(SX,SDX,X,DX)                                         20 00930
C                                                                       20 00940
      LSINGL=.FALSE.                                                    20 00950
      X=0.                                                              20 00960
      DX=0.                                                             20 00970
                                                                        20 00980
*  INITIALIZE                                                           20 00990
                                                                        20 01000
  100 Z=0.                                                              20 01010
      DZ=0.                                                             20 01020
                                                                        20 01030
*  COPY INPUT STRINGS TO TEMP STRINGS.                                  20 01040
                                                                        20 01050
      TX = SX                                                           20 01060
      TDX = SDX                                                         20 01070
                                                                        20 01080
*  SQUEEZE OUT ALL EXTRANEOUS CHARACTERS AND BLANKS FROM TX.            20 01090
                                                                        20 01100
      CALL SUPALF(TX)                                                   20 01110
      CALL SUPEMB(TX)                                                   20 01120
      CALL SUPALF(TDX)                                                  20 01130
      CALL SQZSTR(TX, ' ')                                              20 01140
      CALL SQZSTR(TDX,' ')                                              20 01150
      R = LENSTR(TX)                                                    20 01160
      IF (R .EQ. 0) RETURN                                              20 01170
*     Look to see if its a single non-numeric character                 20 01180
*       and return                                                      20 01190
      If(r.EQ.1 .AND. (tx(1:1).LT.'0' .OR. tx(1:1).GT.'9'))Return       20 01200
                                                                        20 01210
*  LOOK FOR 'E' IN TX AND SET EXPONENT VALUE.                           20 01220
                                                                        20 01230
      IEXP = INDEX(TX(:R), 'E')                                         20 01240
      IF (IEXP .EQ. 0) THEN                                             20 01250
         EXP = 0                                                        20 01260
      ELSE                                                              20 01270
         EXP = IVLSTR(TX(IEXP+1:R))                                     20 01280
         R = IEXP - 1                                                   20 01290
      ENDIF                                                             20 01300
                                                                        20 01310
*  LOOK FOR '.' IN TX AND ADJUST EXPONENT VALUE.                        20 01320
                                                                        20 01330
      IDOT = INDEX(TX(:R), '.')                                         20 01340
      IF (IDOT .GT. 0) THEN                                             20 01350
         EXP = EXP - R + IDOT                                           20 01360
         CALL DELSTR(TX(IDOT:R), 1, 1)                                  20 01370
         R = R - 1                                                      20 01380
      ENDIF                                                             20 01390
                                                                        20 01400
*  CONVERT TX, TDX FROM STRING TO NUMBER AND MULTIPLY BY EXPONENT.      20 01410
                                                                        20 01420
      POWER = 10.0 ** EXP                                               20 01430
      Z = DVALST(TX(:R)) * POWER                                        20 01440
      CALL SQZSTR(TDX, ' ')                                             20 01450
      R = LENSTR(TDX)                                                   20 01460
      IF (R .GT. 0) THEN                                                20 01470
C         IF (TYPSTR(TDX(:R)) .EQ. 1) THEN                              20 01480
            DZ=IVLSTR(TDX(:R))                                          20 01490
            IF(DZ.LT.0) DZ=DZ*(-1)                                      20 01500
            DZ = DZ * POWER                                             20 01510
C         ENDIF                                                         20 01520
      ENDIF                                                             20 01530
C                                                                       20 01540
      IF(LSINGL) THEN                                                   20 01550
         Y=Z                                                            20 01560
         DY=DZ                                                          20 01570
      ELSE                                                              20 01580
         X=Z                                                            20 01590
         DX=DZ                                                          20 01600
      ENDIF                                                             20 01610
      END                                                               20 01620
      SUBROUTINE SUPALF (STR)                                           21 00010
                                                                        21 00020
************************************************************************21 00030
*                                                                      *21 00040
*  SUBROUTINE SUPALF WILL CONVERT ALL NON-NUMERIC CHARACTERS IN STRING *21 00050
*  STR TO BLANKS (EXCEPT  . ,  E ,  +  AND  - ).                       *21 00060
*                                                                      *21 00070
************************************************************************21 00080
                                                                        21 00090
*  SUBROUTINE ARGUMENTS.                                                21 00100
                                                                        21 00110
      CHARACTER*(*)     STR                                             21 00120
                                                                        21 00130
*  Function References.                                                 21 00140
                                                                        21 00150
      Integer           ICHAR                                           21 00160
      Intrinsic         ICHAR                                           21 00170
                                                                        21 00180
*  LOCAL VARIABLES.                                                     21 00190
                                                                        21 00200
      INTEGER           I                                               21 00210
      CHARACTER*1       CHR                                             21 00220
      INTEGER           ICHR                                            21 00230
                                                                        21 00240
*  SCAN STRING AND REPLACE ALL INVALID CHARACTERS.                      21 00250
                                                                        21 00260
      DO 10 I = 1, LEN(STR)                                             21 00270
         CHR = STR(I:I)                                                 21 00280
         ICHR = ICHAR(CHR)                                              21 00290
         IF (ICHR .LT. ICHAR('0') .OR. ICHAR('9') .LT. ICHR) THEN       21 00300
            IF   (CHR .EQ. '.') THEN                                    21 00310
            ELSE IF (CHR .EQ. 'E') THEN                                 21 00320
            ELSE IF (CHR .EQ. '+') THEN                                 21 00330
            ELSE IF (CHR .EQ. '-') THEN                                 21 00340
            ELSE                                                        21 00350
               STR(I:I) = ' '                                           21 00360
            ENDIF                                                       21 00370
         ENDIF                                                          21 00380
   10 CONTINUE                                                          21 00390
      END                                                               21 00400
      SUBROUTINE ZSYM (EL, SYM)                                         22 00010
***   SUBROUTINE IZEL (SYM, EL)                                         22 00020
                                                                        22 00030
************************************************************************22 00040
*                                                                      *22 00050
*  ZSYM: TRANSLATE ELEMENT NUMBER (Z) INTO SYMBOL TEXT.                *22 00060
*                                                                      *22 00070
*  IZEL: TRANSLATE SYMBOL TEXT INTO ELEMENT NUMBER (Z).                *22 00080
*                                                                      *22 00090
************************************************************************22 00100
                                                                        22 00110
*  FUNCTION ARGUMENTS.                                                  22 00120
                                                                        22 00130
      INTEGER           EL                                              22 00140
      CHARACTER*2       SYM                                             22 00150
                                                                        22 00160
*  Commons                                                              22 00170
      Character*80 izlmsg                                               22 00180
      Common/IZLCOM/izlmsg                                              22 00190
*  LOCAL VARIABLES.                                                     22 00200
                                                                        22 00210
      INTEGER           NSYM                                            22 00220
      Parameter (nsym=109)                                              22 00230
      Integer   isym                                                    22 00240
      CHARACTER*2       SYMTBL(0:NSYM)                                  22 00250
                                                                        22 00260
*  DATA INITIALIZATIONS.                                                22 00270
                                                                        22 00280
      DATA              SYMTBL   /'NN',                                 22 00290
     1    'H ', 'HE', 'LI', 'BE', 'B ', 'C ', 'N ', 'O ', 'F ', 'NE',   22 00300
     2    'NA', 'MG', 'AL', 'SI', 'P ', 'S ', 'CL', 'AR', 'K ', 'CA',   22 00310
     3    'SC', 'TI', 'V ', 'CR', 'MN', 'FE', 'CO', 'NI', 'CU', 'ZN',   22 00320
     4    'GA', 'GE', 'AS', 'SE', 'BR', 'KR', 'RB', 'SR', 'Y ', 'ZR',   22 00330
     5    'NB', 'MO', 'TC', 'RU', 'RH', 'PD', 'AG', 'CD', 'IN', 'SN',   22 00340
     6    'SB', 'TE', 'I ', 'XE', 'CS', 'BA', 'LA', 'CE', 'PR', 'ND',   22 00350
     7    'PM', 'SM', 'EU', 'GD', 'TB', 'DY', 'HO', 'ER', 'TM', 'YB',   22 00360
     8    'LU', 'HF', 'TA', 'W ', 'RE', 'OS', 'IR', 'PT', 'AU', 'HG',   22 00370
     9    'TL', 'PB', 'BI', 'PO', 'AT', 'RN', 'FR', 'RA', 'AC', 'TH',   22 00380
     A    'PA', 'U ', 'NP', 'PU', 'AM', 'CM', 'BK', 'CF', 'ES', 'FM',   22 00390
     B    'MD', 'NO', 'LR', 'RF', 'DB', 'SG', 'BH', 'HS', 'MT'/         22 00400
                                                                        22 00410
***   ENTRY ZSYM (EL, SYM)                                              22 00420
                                                                        22 00430
      SYM = '  '                                                        22 00440
      If(0 .LE. el .AND. el .LE. nsym)Then                              22 00450
         sym=symtbl(el)                                                 22 00460
      Else                                                              22 00470
         If(el .GT. nsym)Then                                           22 00480
            isym=el-100                                                 22 00490
            If(isym .LT. 100)Write(sym,FMT='(I2)')isym                  22 00500
         EndIf                                                          22 00510
      EndIf                                                             22 00520
      RETURN                                                            22 00530
                                                                        22 00540
      ENTRY IZEL (SYM, EL)                                              22 00550
                                                                        22 00560
      izlmsg=' '                                                        22 00570
      If(sym(1:1) .GE. '0' .AND. sym(1:1) .LE. '9')Then                 22 00580
         If(sym(2:2).LT.'0' .OR. sym(2:2).GT.'9')Then                   22 00590
            el=-1                                                       22 00600
            Return                                                      22 00610
         EndIf                                                          22 00620
         Read(sym,FMT='(I2)')isym                                       22 00630
         isym=isym+100                                                  22 00640
         If(isym .GE. 104 .AND. isym .LE. nsym)                         22 00650
     2     izlmsg='Obsolete formalism. Use '//symtbl(isym)              22 00660
         If(isym .LE. 103)Then                                          22 00670
            el=-1                                                       22 00680
         Else                                                           22 00690
            el=isym                                                     22 00700
         EndIf                                                          22 00710
         Return                                                         22 00720
      EndIf                                                             22 00730
      DO 10 EL = 0, NSYM                                                22 00740
         IF (SYM .EQ. SYMTBL(EL)) RETURN                                22 00750
   10 CONTINUE                                                          22 00760
      EL = -1                                                           22 00770
      END                                                               22 00780
      SUBROUTINE CNVU2S (Y, DY, SX, LENX, SDX, LENDX)                   23 00010
C                                                                       23 00020
C        CONVERTS THE REAL NUMBER Y(OR X FOR DOUBLE PREC), WITH         23 00030
C        OPTIONAL UNCERTAINTY DY(OR DX FOR DOUBLE REC),                 23 00040
C        INTO STRING FORMAT.  ONE OF FOUR FORMATS IS SELECTED BASED     23 00050
C        ON THE VALUES OF DY(OR DX) AND LENDX.                          23 00060
C                                                                       23 00070
C        Y     IS THE INPUT REAL NUMBER TO BE CONVERTED.                23 00080
C        DY    IS THE INPUT REAL UNCERTAINTY IN Y.                      23 00090
C        X     IS THE DOUBLE PRECISION NUMBER TO BE CONVERTED.          23 00100
C        DX    IS THE DOUBLE PRECISION UNCERTAINTY IN X.                23 00110
C        SX    IS THE OUTPUT STRING FOR X (AND IN FORMAT 2 ALSO DX).    23 00120
C        LENX  IS THE INPUT LENGTH SPECIFIER FOR SX.                    23 00130
C        SDX   IS THE OUTPUT STRING FOR DX (FORMATS 1 AND 3 ONLY).      23 00140
C        LENDX IS THE INPUT LENGTH SPECIFIER FOR SDX (FORMATS 1 AND 3). 23 00150
C              OR A FORMAT FLAG (FORMAT 2 AND 4).                       23 00160
C                                                                       23 00170
C        FORMAT 1:  DX > 0.0, LENDX > 0.                                23 00180
C           SX AND SDX ARE SET.                                         23 00190
C           SDX WILL BE IN THE RANGE 1 TO 25.                           23 00200
C           SX WILL BE SET AS APPROPRIATE FOR THE SPECIFIED UNCERTAINTY.23 00210
C                                                                       23 00220
C        FORMAT 2:  DX > 0.0, LENDX <= 0.                               23 00230
C           SX ONLY IS SET, SDX IS NOT MODIFIED.                        23 00240
C           X AND DX ARE FORMATTED INTO SX.  THE UNCERTAINTY IS NOT     23 00250
C           CONSTRAINED TO THE RANGE 1 TO 25 IF DX > 25.0.              23 00260
C           If LENDX=0, results will be set to the "natural" number of  23 00270
C             significant digits.                                       23 00280
C           If LENDX>0, results will be set to -LENDX significant       23 00290
C             digits.                                                   23 00300
C                                                                       23 00310
C        FORMAT 3:  DX = 0.0, LENDX >= 0.                               23 00320
C           SX AND SDX ARE SET.                                         23 00330
C           SX WILL BE SET USING 4 SIGNIFICANT DIGITS.                  23 00340
C           SDX WILL BE BLANKED OUT TO A LENGTH OF LENDX.               23 00350
C                                                                       23 00360
C        FORMAT 4:  DX = 0.0, LENDX < 0.                                23 00370
C           SX ONLY IS SET, SDX IS NOT MODIFIED.                        23 00380
C           SX WILL BE SET USING -LENDX SIGNIFICANT DIGITS.             23 00390
C                                                                       23 00400
C--         ARGUMENTS PASSED TO THIS ROUTINE.                           23 00410
C                                                                       23 00420
      DOUBLE PRECISION  X, DX, Z, DZ                                    23 00430
      REAL      Y, DY                                                   23 00440
      INTEGER LENX, LENDX                                               23 00450
      CHARACTER*(*) SX, SDX                                             23 00460
C                                                                       23 00470
C--         SUBROUTINES REFERRED                                        23 00480
C                                                                       23 00490
      Integer Lenstr                                                    23 00500
      Logical Ivrflw                                                    23 00510
      External Ivrflw,Lenstr                                            23 00520
      Double Precision DABS,DBLE,DLOG10,DSIGN                           23 00530
      Intrinsic DABS,DBLE,DLOG10,DSIGN                                  23 00540
C     KNVIX, SCALDX, SCALX, SCAL10, KNVI2S                              23 00550
C     SQZSTR(F77STR), ADDSTR(F77STR)                                    23 00560
C                                                                       23 00570
C--         LOCAL VARIABLES.                                            23 00580
C                                                                       23 00590
      INTEGER IX, IDX, IPWR, ISIG, I, IBLK, LENXT                       23 00600
      CHARACTER*10  TEMP                                                23 00610
      DOUBLE PRECISION T                                                23 00620
C                                                                       23 00630
C   ENTRY FOR CNVU2S (SINGLE PRECISION)                                 23 00640
C                                                                       23 00650
      X=Y                                                               23 00660
      DX=DY                                                             23 00670
      GO TO 10                                                          23 00680
C                                                                       23 00690
C   ENTRY FOR DCNVUS (DOUBLE PRECISION)                                 23 00700
C                                                                       23 00710
      ENTRY DCNVUS(Z,DZ,SX,LENX,SDX,LENDX)                              23 00720
      X=Z                                                               23 00730
      DX=DZ                                                             23 00740
C                                                                       23 00750
C--         DETERMINE FORMATS BASED ON VALUES OF DX AND LENDX.          23 00760
C                                                                       23 00770
   10 IF (DX .GT. 0.0) THEN                                             23 00780
         IF (LENDX .GT. 0) THEN                                         23 00790
C                                                                       23 00800
C--         FORMAT 1:  SX, SDX (1, 25).                                 23 00810
C                                                                       23 00820
            CALL SCALDX(DX, IDX, IPWR)                                  23 00830
C           Check if there will be an integer overflow if SCALX is      23 00840
C             called                                                    23 00850
            If(Ivrflw(x,ipwr))Then                                      23 00860
               sx='*************************************'               23 00870
               sdx='*************************************'              23 00880
               Return                                                   23 00890
            EndIf                                                       23 00900
            CALL SCALX(X, IX, IPWR)                                     23 00910
C    when IX and IDX are multiple of 10, reduce them by 10--skip this(ys23 00920
C            CALL SCAL10(IX, IDX, IPWR)                                 23 00930
            CALL KNVIX(IX, IPWR, SX, LENX)                              23 00940
            CALL KNVI2S(IDX,SDX,LENDX)                                  23 00950
         ELSE                                                           23 00960
C                                                                       23 00970
C--         FORMAT 2:  SX ONLY (SDX INCLUDED).                          23 00980
C     Following logic was not delivering correct results as defined     23 00990
C       for the format (Error noted by RRK)                             23 01000
C                                                                       23 01010
CRRK            CALL SCALDX(DX, IDX, IPWR)                              23 01020
CRRK            IF (DX .GT. 25.0) THEN                                  23 01030
CRRK               IDX = DX + 0.9                                       23 01040
CRRK               IX = X + DSIGN(0.5D0, X)                             23 01050
CRRK               IPWR = 0                                             23 01060
CRRK            ELSE                                                    23 01070
CRRK               CALL SCALX(X, IX, IPWR)                              23 01080
CRRK            ENDIF                                                   23 01090
            ipwr=0                                                      23 01100
            idx=dx                                                      23 01110
            i=1                                                         23 01120
13          Continue                                                    23 01130
            If(DABS((dx-FLOAT(idx))/dx) .LE. 1.D-4)GoTo 14              23 01140
            ipwr=ipwr-1                                                 23 01150
            dx=dx*1.D1                                                  23 01160
            idx=INT(dx+0.9)                                             23 01170
            i=i+1                                                       23 01180
C           Not converging - abort nicely                               23 01190
            If(i .GT. 100)Then                                          23 01200
               sx='*************************************'               23 01210
               Return                                                   23 01220
            EndIf                                                       23 01230
            GoTo 13                                                     23 01240
14          Continue                                                    23 01250
            Call Knvi2s(idx,temp,0)                                     23 01260
C           lendx less than zero indicates number of significant digits 23 01270
C             to retain. If lendx=0, than default to "natural" number   23 01280
C             of significant digits                                     23 01290
15          Continue                                                    23 01300
            If(lendx.LT.0 .AND. Lenstr(temp).NE.-lendx)Then             23 01310
               If(Lenstr(temp) .LT. -lendx)Then                         23 01320
                  ipwr=ipwr-1                                           23 01330
                  dx=dx*10.                                             23 01340
                  idx=INT(dx+0.9)                                       23 01350
               EndIf                                                    23 01360
               If(Lenstr(temp) .GT. -lendx)Then                         23 01370
                  ipwr=ipwr+1                                           23 01380
                  dx=dx/10.                                             23 01390
                  idx=INT(dx+0.9)                                       23 01400
               EndIf                                                    23 01410
               Call Knvi2s(idx,temp,0)                                  23 01420
               GoTo 15                                                  23 01430
            EndIf                                                       23 01440
C           Check if there will be an integer overflow if SCALX is      23 01450
C             called                                                    23 01460
            If(Ivrflw(x,ipwr))Then                                      23 01470
               sx='*************************************'               23 01480
               Return                                                   23 01490
            EndIf                                                       23 01500
            Call Scalx(x,ix,ipwr)                                       23 01510
            CALL KNVIX(IX, IPWR, SX, LENX)                              23 01520
            IF (SX(1:1) .EQ. '*') GOTO 99                               23 01530
            CALL SQZSTR(SX,' ')                                         23 01540
            CALL ADDSTR(TEMP,1,' ')                                     23 01550
            LENXT=LENSTR(SX)+LENSTR(TEMP)                               23 01560
            IF (LENXT .LE. LENX) THEN                                   23 01570
               IBLK=LENX-LENXT                                          23 01580
               SX(LENSTR(SX)+1:LENX)=TEMP                               23 01590
               DO 16 I=1,IBLK                                           23 01600
               CALL ADDSTR(SX,1,' ')                                    23 01610
   16          CONTINUE                                                 23 01620
            ELSE                                                        23 01630
               SX='*************************************'               23 01640
            ENDIF                                                       23 01650
         ENDIF                                                          23 01660
      ELSE                                                              23 01670
C                                                                       23 01680
C--         FORMAT 3:  SX HAS 4 SIG. DIGITS, SDX BLANKED.               23 01690
C--         FORMAT 4:  SX HAS -LENDX SIG. DIGITS, SDX UNTOUCHED.        23 01700
C                                                                       23 01710
         ISIG = 4                                                       23 01720
         IF (LENDX .LT. 0) ISIG = -LENDX                                23 01730
C        ...FIND PROPER IPWR.                                           23 01740
         T = 0.0D0                                                      23 01750
         IF (DABS(X).GT. 1.D-35) T = DLOG10(DABS(X))                    23 01760
         IF (T .LT. 0.0D0) T = T - 1.0D0                                23 01770
         IPWR = INT(T) - ISIG + 1                                       23 01780
C        Check if there will be an integer overflow if SCALX is         23 01790
C          called                                                       23 01800
         If(Ivrflw(x,ipwr))Then                                         23 01810
            sx='*************************************'                  23 01820
            If(lendx .EQ. 0)sdx=' '                                     23 01830
            Return                                                      23 01840
         EndIf                                                          23 01850
         CALL SCALX(X, IX, IPWR)                                        23 01860
         CALL KNVIX(IX, IPWR, SX, LENX)                                 23 01870
         IF (LENDX .LT. 0) GOTO 99                                      23 01880
            SDX = ' '                                                   23 01890
      ENDIF                                                             23 01900
C                                                                       23 01910
C--         RETURN TO CALLING ROUTINE.                                  23 01920
C                                                                       23 01930
   99 RETURN                                                            23 01940
      END                                                               23 01950
      Logical Function Ivrflw(x,ipwr)                                   24 00010
C     Check on possiblity of integer overflow                           24 00020
C                                                                       24 00030
      Double Precision x                                                24 00040
      Integer ipwr                                                      24 00050
C                                                                       24 00060
      Double Precision xx                                               24 00070
C                                                                       24 00080
      Ivrflw=.FALSE.                                                    24 00090
      xx=x*(10.0D+0**(-ipwr))                                           24 00100
      If(.NOT.(xx.GE.-(2.D+0**31) .AND. xx.LE.((2.D+0**31)-1.D0)))      24 00110
     2  Ivrflw=.TRUE.                                                   24 00120
      Return                                                            24 00130
      End                                                               24 00140
      SUBROUTINE KNVIX (IX, IPWR, SX, LENX)                             25 00010
C                                                                       25 00020
C        CONVERT IX WITH SCALE FACTOR IPWR TO A STRING SX OF LENGTH     25 00030
C        LENX.  IF THE STRING SPACE IS TOO SMALL, RETURN STARS (*).     25 00040
C        IF IPWR > 0 USE EXPONENTIAL FORMAT.                            25 00050
C        IF IX * 10 ** IPWR < 1E-4 USE EXPONENTIAL FORMAT.              25 00060
C                                                                       25 00070
C--         ARGUEMENTS PASSED TO THIS ROUTINE.                          25 00080
C                                                                       25 00090
      INTEGER IX, IPWR, LENX                                            25 00100
      CHARACTER*(*) SX                                                  25 00110
C                                                                       25 00120
C--         OTHER SUBROUTINE REFERENCES.                                25 00130
      Integer Lenstr                                                    25 00140
      External Lenstr                                                   25 00150
C                                                                       25 00160
C     KNVI2S                                                            25 00170
C     ADDSTR(F77STR)                                                    25 00180
C                                                                       25 00190
C--         LOCAL VARIABLES.                                            25 00200
C                                                                       25 00210
      INTEGER  IPTR, JPWR, I, NEG, IBLK, LENTE2, LENTEM                 25 00220
      Character*12 TEMP2                                                25 00230
      CHARACTER*20 TEMP                                                 25 00240
C                                                                       25 00250
C--         CONVERT IX TO STRING MODE.                                  25 00260
C                                                                       25 00270
      CALL KNVI2S(IX,TEMP,0)                                            25 00280
C                                                                       25 00290
C--         NEG IS CONTROL FOR NEGATIVE NUMBER (LEAVE SPACE FOR SIGN).  25 00300
C                                                                       25 00310
      NEG = 0                                                           25 00320
      IF (IX .LT. 0) NEG = 1                                            25 00330
C                                                                       25 00340
C--         SPECIAL FORMATTING BASED ON IPWR.                           25 00350
C                                                                       25 00360
      IF (IPWR) 30, 60, 20                                              25 00370
C                                                                       25 00380
C--         IPWR > 0, RETURN DIG . FRACT E EXP.                         25 00390
C                                                                       25 00400
C     ...FIND LOCATION FOR EXPONENT.                                    25 00410
   20 LENTEM=LENSTR(TEMP)                                               25 00420
      IPTR = LENTEM + 1                                                 25 00430
C     ...COMPUTE EXPONENT VALUE.                                        25 00440
      JPWR = LENTEM - NEG + IPWR - 1                                    25 00450
C     ...ADD EXPONENT TO END OF STRING.                                 25 00460
      CALL KNVI2S(JPWR,TEMP2,0)                                         25 00470
      LENTE2=LENSTR(TEMP2)                                              25 00480
      TEMP(IPTR:IPTR+LENTE2-1)=TEMP2(1:LENTE2)                          25 00490
C     ...COMPUTE LENGTH OF NEW STRING.                                  25 00500
      LENTEM=LENTEM+LENTE2+1                                            25 00510
C     ...REPLACE EXPONENT LENGTH WITH 'E'.                              25 00520
      CALL ADDSTR(TEMP,IPTR,'E')                                        25 00530
C     ...INSERT DECIMAL POINT AFTER FIRST (REQUIRED) DIGIT.             25 00540
      CALL ADDSTR(TEMP,2+NEG,'.')                                       25 00550
C     ...CHECK FIT AND GENERATE SX.                                     25 00560
      GOTO 60                                                           25 00570
C                                                                       25 00580
C--         IPWR < 0, RETURN ONE OF:                                    25 00590
C--            DIG . FRACT :: TEMP(1) > -IPWR.                          25 00600
C--            0 . 0'S FRACT :: TEMP(1) <= -IPWR.                       25 00610
C--            DIG . FRACT E - EXP :: TEMP(1) + 4 <= -IPWR              25 00620
C                                                                       25 00630
   30 LENTEM=LENSTR(TEMP)                                               25 00640
      IF (LENTEM-NEG .GT. -IPWR) THEN                                   25 00650
C        ...FIND WHERE DECIMAL POINT BELONGS AND INSERT IT.             25 00660
         IPTR = LENTEM + 1 + IPWR                                       25 00670
         CALL ADDSTR(TEMP,IPTR,'.')                                     25 00680
      ELSE                                                              25 00690
C                                                                       25 00700
         IF (LENTEM-NEG+4 .LE. -IPWR) GOTO 20                           25 00710
C        ...NOTE E FORMAT CODE THE SAME FOR E+ AND E-.                  25 00720
C        ...FIND NUMBER OF LEADING ZEROS.                               25 00730
         IPTR = -IPWR - LENTEM + NEG                                    25 00740
C        ...INSERT LEADING ZEROS.                                       25 00750
         IF (IPTR .GE. 1) THEN                                          25 00760
            DO 45 I = 1, IPTR                                           25 00770
               CALL ADDSTR(TEMP, I+NEG, '0')                            25 00780
   45       CONTINUE                                                    25 00790
         ENDIF                                                          25 00800
C        ...INSERT DECIMAL POINT AND FIRST DIGIT.                       25 00810
         CALL ADDSTR(TEMP, 1+NEG, '0.')                                 25 00820
      ENDIF                                                             25 00830
C                                                                       25 00840
C--         RETURN TO CALLING ROUTINE.                                  25 00850
C                                                                       25 00860
C                                                                       25 00870
C--         IPWR = 0, RETURN INTEGER FORMAT.                            25 00880
C                                                                       25 00890
   60 LENTEM=LENSTR(TEMP)                                               25 00900
      IF (LENTEM .LE. LENX) THEN                                        25 00910
         SX=' '                                                         25 00920
         IBLK=LENX-LENTEM                                               25 00930
         SX(IBLK+1:LENX)=TEMP(1:LENTEM)                                 25 00940
      ELSE                                                              25 00950
         SX='***************************'                               25 00960
      ENDIF                                                             25 00970
C                                                                       25 00980
      RETURN                                                            25 00990
      END                                                               25 01000
      SUBROUTINE SCALDX (DX, IDX, IPWR)                                 26 00010
C                                                                       26 00020
C        COMPUTE IDX IN RANGE 3 TO 25.                                  26 00030
C        IPWR IS POWER OF 10 TO GET BACK TO ORIGINAL.                   26 00040
C                                                                       26 00050
C--         ARGUMENTS PASSED TO THIS ROUTINE.                           26 00060
C                                                                       26 00070
      DOUBLE PRECISION  DX                                              26 00080
      INTEGER IDX, IPWR                                                 26 00090
C                                                                       26 00100
C--         LOCAL VARIABLES.                                            26 00110
C                                                                       26 00120
      DOUBLE PRECISION  D                                               26 00130
C                                                                       26 00140
C--         SET WORKING VARIABLES.                                      26 00150
C                                                                       26 00160
      D = DX                                                            26 00170
      IPWR = 0                                                          26 00180
C                                                                       26 00190
C--         D < 3.0, MULTIPLY BY 10.0.                                  26 00200
C                                                                       26 00210
   10 IF (D .GE. 3.0) GOTO 20                                           26 00220
         D = D * 10.0                                                   26 00230
         IPWR = IPWR - 1                                                26 00240
         GOTO 10                                                        26 00250
C     ENDIF                                                             26 00260
C                                                                       26 00270
C--         D > 25.0, DIVIDE BY 10.0.                                   26 00280
C                                                                       26 00290
   20 IF (D .LE. 25.0) GOTO 30                                          26 00300
         D = D / 10.0                                                   26 00310
         IPWR = IPWR + 1                                                26 00320
         GOTO 20                                                        26 00330
C     ENDIF                                                             26 00340
C                                                                       26 00350
C--         D IN RANGE 3 TO 25, ROUND AND FIX.                          26 00360
C                                                                       26 00370
   30 IDX = INT(D + 0.9)                                                26 00380
C                                                                       26 00390
C--         RETURN TO CALLING ROUTINE.                                  26 00400
C                                                                       26 00410
      RETURN                                                            26 00420
      END                                                               26 00430
      SUBROUTINE SCALX (X, IX, IPWR)                                    27 00010
C                                                                       27 00020
C        COMPUTE IX BASED ON X AND IPWR.                                27 00030
C                                                                       27 00040
C--         ARGUMENTS PASSED TO THIS ROUTINE.                           27 00050
C                                                                       27 00060
      DOUBLE PRECISION  X                                               27 00070
      INTEGER IX, IPWR                                                  27 00080
C                                                                       27 00090
C--         Function References                                         27 00100
      Double Precision DSIGN                                            27 00110
      Intrinsic DSIGN                                                   27 00120
C                                                                       27 00130
C--         LOCAL VARIABLES.                                            27 00140
C                                                                       27 00150
      DOUBLE PRECISION  XX                                              27 00160
C                                                                       27 00170
C--         SCALE AND FIX X.                                            27 00180
C                                                                       27 00190
      XX = X * (10.0D0 ** (-IPWR))                                      27 00200
      IX = INT(XX + DSIGN(0.5D0, XX))                                   27 00210
C                                                                       27 00220
C--         RETURN TO CALLING ROUTINE.                                  27 00230
C                                                                       27 00240
      RETURN                                                            27 00250
      END                                                               27 00260
      SUBROUTINE SCAL10 (IX, IDX, IPWR)                                 28 00010
C                                                                       28 00020
C        IF IDX = 10 OR 20 AND IX A MULTIPLE OF 10,                     28 00030
C        REDUCE IX, IDX BY 10 AND CHANGE IPWR.                          28 00040
C                                                                       28 00050
C--         ARGUMENTS PASSED TO THIS ROUTINE.                           28 00060
C                                                                       28 00070
      INTEGER IX, IDX, IPWR                                             28 00080
C                                                                       28 00090
C--         Function References.                                        28 00100
      Integer MOD                                                       28 00110
      Intrinsic MOD                                                     28 00120
C                                                                       28 00130
C--         PROCEDURE.                                                  28 00140
C                                                                       28 00150
      IF ((MOD(IX, 10) .NE. 0) .OR. (MOD(IDX, 10) .NE. 0)) GOTO 99      28 00160
         IX = IX / 10                                                   28 00170
         IDX = IDX / 10                                                 28 00180
         IPWR = IPWR + 1                                                28 00190
C     ENDIF                                                             28 00200
C                                                                       28 00210
C--         RETURN TO CALLING ROUTINE.                                  28 00220
C                                                                       28 00230
   99 RETURN                                                            28 00240
      END                                                               28 00250
      SUBROUTINE KNVI2S(N,STR,LEN)                                      29 00010
C                                                                       29 00020
C   CONVERTS THE INTEGER N INTO A RIGHT JUSTIFIED STRING, STR,          29 00030
C   WITH STRING LENGTH LEN.                                             29 00040
C   IF LEN EQUALS 0, THE RETURNED STRING IS LEFTJUSTIFIED.              29 00050
C   IF N IS TOO LARGE FOR LEN CHARACTERS, STARS FILL STR.               29 00060
C   LONGEST STRING CONSIDERED IS 11 CHARACTERS ACCORDING TO             29 00070
C   LARGEST 4 BYTE INTEGER SIZE.                                        29 00080
C                                                                       29 00090
      INTEGER        N,LEN,IBLK,LENST                                   29 00100
      CHARACTER*(*)  STR                                                29 00110
      CHARACTER*(11) TEMP                                               29 00120
C                                                                       29 00130
C   SUBROUTINES REFERENCES                                              29 00140
C                                                                       29 00150
      Integer Lenstr                                                    29 00160
      External Lenstr                                                   29 00170
C    SQZSTR(F77STR)                                                     29 00180
C                                                                       29 00190
      STR=' '                                                           29 00200
      WRITE(TEMP,'(I11)') N                                             29 00210
C                                                                       29 00220
C   LEFT JUSTIFY STR                                                    29 00230
C                                                                       29 00240
      CALL SQZSTR(TEMP,' ')                                             29 00250
C                                                                       29 00260
      IF(LEN.GT.0) THEN                                                 29 00270
C                                                                       29 00280
C   LEN > 0, SO RIGHT JUSTIFY TO LENTH POSITION IF FITS                 29 00290
C                                                                       29 00300
        LENST=LENSTR(TEMP)                                              29 00310
        IF(LEN.GE.LENST) THEN                                           29 00320
          IBLK=LEN-LENST                                                29 00330
          STR(IBLK+1:LEN)=TEMP(1:LENST)                                 29 00340
        ELSE                                                            29 00350
C                                                                       29 00360
C   FILS STARS SINCE LEN IS NOT BIG ENOUGH                              29 00370
C                                                                       29 00380
          STR='*************'                                           29 00390
        ENDIF                                                           29 00400
C                                                                       29 00410
      ELSE                                                              29 00420
C                                                                       29 00430
C   LEN=0  SO LEAVE THE STRING LEFT JUSTIFIED                           29 00440
C                                                                       29 00450
        STR=TEMP                                                        29 00460
      ENDIF                                                             29 00470
      RETURN                                                            29 00480
      END                                                               29 00490
      SUBROUTINE SUPEMB(STR)                                            30 00010
C                                                                       30 00020
C   subroutine to find and eliminate unwanted embedded +'s and -'s      30 00030
C   from string STR.  Should be used in addition to SUPALF when         30 00040
C   needed.                                                             30 00050
C                                                                       30 00060
      CHARACTER*(*)  STR                                                30 00070
C                                                                       30 00080
      Integer        LEN                                                30 00090
      Intrinsic      LEN                                                30 00100
C                                                                       30 00110
      Integer        i                                                  30 00120
      LOGICAL        START,RIDOF                                        30 00130
      CHARACTER*1    CHR                                                30 00140
C                                                                       30 00150
C   + and - are allowed only at the beginning or right after E,         30 00160
c   when START is true. after bad + or - are found, rest of the         30 00170
c   string will become blank.                                           30 00180
c                                                                       30 00190
      RIDOF=.FALSE.                                                     30 00200
      START=.TRUE.                                                      30 00210
      DO 100 I=1,LEN(STR)                                               30 00220
         IF(RIDOF) THEN                                                 30 00230
            STR(I:I)=' '                                                30 00240
            GO TO 100                                                   30 00250
         ENDIF                                                          30 00260
         CHR=STR(I:I)                                                   30 00270
         IF(CHR.EQ.' ') GO TO 100                                       30 00280
         IF(START) THEN                                                 30 00290
            IF(ICHAR(CHR).LE.ICHAR('9') .AND. ICHAR(CHR).GE.ICHAR('0')) 30 00300
     1         START=.FALSE.                                            30 00310
            GO TO 100                                                   30 00320
         ENDIF                                                          30 00330
         IF(.NOT.START) THEN                                            30 00340
            IF(CHR.EQ.'E') THEN                                         30 00350
               START=.TRUE.                                             30 00360
               GO TO 100                                                30 00370
            ENDIF                                                       30 00380
            IF(CHR.EQ.'+' .OR. CHR.EQ.'-') THEN                         30 00390
               STR(I:I)=' '                                             30 00400
               RIDOF=.TRUE.                                             30 00410
            ENDIF                                                       30 00420
         ENDIF                                                          30 00430
  100 CONTINUE                                                          30 00440
      RETURN                                                            30 00450
      END                                                               30 00460
************************************************************************31 00010
*                                                                      *31 00020
*  SUBPROGRAM LIBRARY NSDMTH.                                          *31 00030
*     F77 VERSION OF MATH ROUTINES.                                    *31 00040
*                                                                      *31 00050
*  VERSION 1(00).                 SPLIT FROM NSDLIB.                   *31 00060
*  Version 1(01) as of 23-Feb-93. Finished typing all variables        *31 00070
*                                                                      *31 00080
*     REFER ALL COMMENTS AND INQUIRIES TO                              *31 00090
*     NATIONAL NUCLEAR DATA CENTER                                     *31 00100
*     BUILDING 197D                                                    *31 00110
*     BROOKHAVEN NATIONAL LABORATORY                                   *31 00120
*     UPTON, NEW YORK 11973                                            *31 00130
*     TELEPHONE 516-282-2901 COMM                                      *31 00140
*                   666-2901 FTS                                       *31 00150
*                                                                      *31 00160
************************************************************************31 00170
                                                                        31 00180
      SUBROUTINE UADD (Z, DZ, X, DX, Y, DY)                             31 00190
                                                                        31 00200
************************************************************************31 00210
*                                                                      *31 00220
*  COMPUTE THE SUM OF TWO NUMBERS AND THE UNCERTAINTY OF THE SUM.      *31 00230
*                                                                      *31 00240
************************************************************************31 00250
                                                                        31 00260
*  SUBROUTINE ARGUMENTS.                                                31 00270
                                                                        31 00280
      REAL              X, DX                                           31 00290
      REAL              Y, DY                                           31 00300
      REAL              Z, DZ                                           31 00310
                                                                        31 00320
*  Function References.                                                 31 00330
                                                                        31 00340
      Real              SQRT                                            31 00350
      Intrinsic         SQRT                                            31 00360
                                                                        31 00370
*  ADD VALUES; UNCERT IS SQUARE ROOT OF SUM OF SQUARES.                 31 00380
                                                                        31 00390
      Z = X + Y                                                         31 00400
      DZ = SQRT(DX*DX + DY*DY)                                          31 00410
      RETURN                                                            31 00420
      END                                                               31 00430
      SUBROUTINE USUB(Z, DZ, X, DX, Y, DY)                              32 00010
                                                                        32 00020
************************************************************************32 00030
*                                                                      *32 00040
*  COMPUTE THE DIFFERENCE OF TWO NUMBERS AND THE UNCERTAINTY OF THE    *32 00050
*     DIFFERENCE.                                                      *32 00060
*                                                                      *32 00070
************************************************************************32 00080
                                                                        32 00090
*  SUBROUTINE ARGUMENTS.                                                32 00100
                                                                        32 00110
      REAL              X, DX                                           32 00120
      REAL              Y, DY                                           32 00130
      REAL              Z, DZ                                           32 00140
                                                                        32 00150
*  Function References.                                                 32 00160
                                                                        32 00170
      Real              SQRT                                            32 00180
      Intrinsic         SQRT                                            32 00190
                                                                        32 00200
*  SUBTRACT VALUES; UNCERT IS SQUARE ROOT OF SUM OF SQUARES.            32 00210
                                                                        32 00220
      Z = X - Y                                                         32 00230
      DZ = SQRT(DX*DX + DY*DY)                                          32 00240
      RETURN                                                            32 00250
      END                                                               32 00260
      SUBROUTINE UMULT(Z, DZ, X, DX, Y, DY)                             33 00010
                                                                        33 00020
************************************************************************33 00030
*                                                                      *33 00040
*  COMPUTE THE PRODUCT OF TWO NUMBERS AND THE UNCERTAINTY OF THE       *33 00050
*     PRODUCT.                                                         *33 00060
*                                                                      *33 00070
************************************************************************33 00080
                                                                        33 00090
*  SUBROUTINE ARGUMENTS.                                                33 00100
                                                                        33 00110
      REAL              X, DX                                           33 00120
      REAL              Y, DY                                           33 00130
      REAL              Z, DZ                                           33 00140
                                                                        33 00150
*  Function References.                                                 33 00160
                                                                        33 00170
      Real              SQRT                                            33 00180
      Intrinsic         SQRT                                            33 00190
                                                                        33 00200
*  MULTIPLY VALUES; UNCERT IS BY FORMULA.                               33 00210
                                                                        33 00220
      Z = X * Y                                                         33 00230
      DZ = Z * SQRT((DX/X)**2 + (DY/Y)**2)                              33 00240
      RETURN                                                            33 00250
      END                                                               33 00260
      SUBROUTINE UDIV(Z, DZ, X, DX, Y, DY)                              34 00010
                                                                        34 00020
************************************************************************34 00030
*                                                                      *34 00040
*  COMPUTE THE QUOTIENT OF TWO NUMBERS AND THE UNCERTAINTY OF THE      *34 00050
*     QUOTIENT.                                                        *34 00060
*                                                                      *34 00070
************************************************************************34 00080
                                                                        34 00090
*  SUBROUTINE ARGUMENTS.                                                34 00100
                                                                        34 00110
      REAL              X, DX                                           34 00120
      REAL              Y, DY                                           34 00130
      REAL              Z, DZ                                           34 00140
                                                                        34 00150
*  Function References.                                                 34 00160
                                                                        34 00170
      Real              SQRT                                            34 00180
      Intrinsic         SQRT                                            34 00190
                                                                        34 00200
*  DIVIDE VALUES; UNCERT IS BY FORMULA.                                 34 00210
                                                                        34 00220
      Z = X / Y                                                         34 00230
      DZ = Z * SQRT((DX/X)**2 + (DY/Y)**2)                              34 00240
      RETURN                                                            34 00250
      END                                                               34 00260
      COMPLEX FUNCTION GAMA (X)                                         35 00010
                                                                        35 00020
************************************************************************35 00030
*                                                                      *35 00040
*  Z = GAMMA(X)                                                        *35 00050
*     FOR ALL VALUES OF X (Z, X COMPLEX).                              *35 00060
*                                                                      *35 00070
************************************************************************35 00080
                                                                        35 00090
*  FUNCTION ARGUMENTS.                                                  35 00100
                                                                        35 00110
      COMPLEX           X                                               35 00120
                                                                        35 00130
*  Function References                                                  35 00140
                                                                        35 00150
      COMPLEX           GAMZ                                            35 00160
      External          GAMZ                                            35 00170
      Real              AIMAG                                           35 00180
      Complex           CONJG                                           35 00190
      Intrinsic         AIMAG,CONJG                                     35 00200
                                                                        35 00210
*  LOCAL VARIABLES.                                                     35 00220
                                                                        35 00230
      REAL              FJ                                              35 00240
      REAL              FN                                              35 00250
      INTEGER           J                                               35 00260
      INTEGER           N                                               35 00270
      COMPLEX           XTMP                                            35 00280
                                                                        35 00290
*  FOR DIFFERENT VALUES OF THE REAL AND IMAGINARY PARTS, GAMMA IS       35 00300
*     COMPUTED DIFFERENTLY.                                             35 00310
                                                                        35 00320
      IF (REAL(X) .GE. 0.0) THEN                                        35 00330
         IF (AIMAG(X) .GE. 0.0) THEN                                    35 00340
            GAMA = GAMZ(X)                                              35 00350
         ELSE                                                           35 00360
          GAMA = CONJG(GAMZ(CONJG(X)))                                  35 00370
         ENDIF                                                          35 00380
      ELSE                                                              35 00390
         N = 1.0 - REAL(X)                                              35 00400
         FN = N                                                         35 00410
         XTMP = X + FN                                                  35 00420
         J = 0                                                          35 00430
         IF (AIMAG(X) .GE. 0.0) THEN                                    35 00440
            GAMA = GAMZ(XTMP)                                           35 00450
         ELSE                                                           35 00460
            GAMA = CONJG(GAMZ(CONJG(XTMP)))                             35 00470
         ENDIF                                                          35 00480
         DO 10 J = 1, N                                                 35 00490
            FJ = J - 1                                                  35 00500
            XTMP = X + FJ                                               35 00510
            GAMA = GAMA / XTMP                                          35 00520
   10    CONTINUE                                                       35 00530
      ENDIF                                                             35 00540
      RETURN                                                            35 00550
      END                                                               35 00560
      COMPLEX FUNCTION GAMZ (X)                                         36 00010
                                                                        36 00020
************************************************************************36 00030
*                                                                      *36 00040
*  Z = GAMMA(X)                                                        *36 00050
*     FOR ALL X(REAL), X(IMAG) >= 0.                                   *36 00060
*     NOTE - GAMZ CALLS GAM1 WHICH MODIFIES ITS ARGUMENT.              *36 00070
*                                                                      *36 00080
************************************************************************36 00090
                                                                        36 00100
*  FUNCTION ARGUMENTS.                                                  36 00110
                                                                        36 00120
      COMPLEX           X                                               36 00130
                                                                        36 00140
*  Function References.                                                 36 00150
                                                                        36 00160
      COMPLEX           GAM1                                            36 00170
      External          Gam1                                            36 00180
      Real              AIMAG,ALOG                                      36 00190
      Complex           CMPLX                                           36 00200
      Intrinsic         AIMAG,ALOG,CMPLX                                36 00210
                                                                        36 00220
*  LOCAL VARIABLES.                                                     36 00230
                                                                        36 00240
      COMPLEX           C                                               36 00250
      REAL              F1N                                             36 00260
      REAL              FN                                              36 00270
      INTEGER           J                                               36 00280
      INTEGER           M                                               36 00290
      REAL              PI                                              36 00300
      REAL              S                                               36 00310
      REAL              T                                               36 00320
      COMPLEX           XT                                              36 00330
      COMPLEX           XTMP                                            36 00340
      COMPLEX           Z1                                              36 00350
      COMPLEX           Z2                                              36 00360
                                                                        36 00370
*  DATA INITIALIZATIONS.                                                36 00380
                                                                        36 00390
      DATA PI/2.50 662 827 463/                                         36 00400
                                                                        36 00410
*  X(IMAG) <= 1.0 IS SPECIAL CASE.                                      36 00420
                                                                        36 00430
      IF (AIMAG(X) .LE. 1.0) THEN                                       36 00440
          XT = X                                                        36 00450
          GAMZ = GAM1(XT)                                               36 00460
      ELSE                                                              36 00470
         M = AIMAG(X)                                                   36 00480
         FN = M + 1                                                     36 00490
         XTMP = X / FN                                                  36 00500
         XT = XTMP                                                      36 00510
         Z1 = GAM1(XT)                                                  36 00520
         F1N = 1.0 / FN                                                 36 00530
         J = 1                                                          36 00540
         DO 10 J = 1, M                                                 36 00550
            XTMP = XTMP + F1N                                           36 00560
            XT = XTMP                                                   36 00570
            Z2 = GAM1(XT)                                               36 00580
            Z1 = Z1 * Z2                                                36 00590
   10    CONTINUE                                                       36 00600
         S = (FN ** (REAL(X) - 0.5)) / (PI ** M)                        36 00610
         T = AIMAG(X) * ALOG(FN)                                        36 00620
         C = S * CMPLX(COS(T), SIN(T))                                  36 00630
         GAMZ = C * Z1                                                  36 00640
      ENDIF                                                             36 00650
      RETURN                                                            36 00660
      END                                                               36 00670
      COMPLEX FUNCTION GAM1 (X)                                         37 00010
                                                                        37 00020
************************************************************************37 00030
*                                                                      *37 00040
*  Z = GAMMA(X)                                                        *37 00050
*     FOR X(REAL) >= 0, 0 <= X(IMAG) <= 1                              *37 00060
*                                                                      *37 00070
************************************************************************37 00080
                                                                        37 00090
*  FUNCTION ARGUMENTS.                                                  37 00100
                                                                        37 00110
      COMPLEX           X                                               37 00120
                                                                        37 00130
*  Function References.                                                 37 00140
                                                                        37 00150
      COMPLEX           GAM2                                            37 00160
      External          Gam2                                            37 00170
      Real              REAL                                            37 00180
      Intrinsic         REAL                                            37 00190
                                                                        37 00200
*  LOCAL VAIRABLES.                                                     37 00210
                                                                        37 00220
      REAL              FJ                                              37 00230
      REAL              FN                                              37 00240
      INTEGER           J                                               37 00250
      INTEGER           N                                               37 00260
                                                                        37 00270
*  X(REAL) <= 1.0 IS SPECIAL CASE.                                      37 00280
                                                                        37 00290
      IF (REAL(X) .LE. 1.0) THEN                                        37 00300
          GAM1 = GAM2(X)                                                37 00310
      ELSE                                                              37 00320
         N = REAL(X)                                                    37 00330
         FN = N                                                         37 00340
         X = X - FN                                                     37 00350
         IF (REAL(X) .EQ. 0.0) THEN                                     37 00360
            N = N - 1                                                   37 00370
            X = X + 1.                                                  37 00380
         ENDIF                                                          37 00390
         GAM1 = GAM2(X)                                                 37 00400
         DO 10 J = 1, N                                                 37 00410
            FJ = J - 1                                                  37 00420
            GAM1 = (X + FJ) * GAM1                                      37 00430
   10    CONTINUE                                                       37 00440
      ENDIF                                                             37 00450
      RETURN                                                            37 00460
      END                                                               37 00470
      COMPLEX FUNCTION GAM2(X)                                          38 00010
                                                                        38 00020
************************************************************************38 00030
*                                                                      *38 00040
*  Z = GAMMA(X)                                                        *38 00050
*     FOR 0 <= X(REAL) <= 1, 0 <= X(IMAG) <= 1                         *38 00060
*                                                                      *38 00070
*     USING PADE-POWER APPROXIMATION OF 1 / GAMMA(X).                  *38 00080
*                                                                      *38 00090
************************************************************************38 00100
                                                                        38 00110
*  FUNCTION ARGUMENTS.                                                  38 00120
                                                                        38 00130
      COMPLEX           X                                               38 00140
                                                                        38 00150
*  LOCAL VARIABLES.                                                     38 00160
                                                                        38 00170
      REAL              A(9)                                            38 00180
      REAL              B(9)                                            38 00190
      INTEGER           I                                               38 00200
      COMPLEX           P                                               38 00210
      COMPLEX           Q                                               38 00220
                                                                        38 00230
*  DATA INITIALIZATIONS.                                                38 00240
                                                                        38 00250
      DATA A/                                                           38 00260
     1    +0.0 000 000 000 E+0, +1.0 000 000 000 E+0,                   38 00270
     2    +1.2 536 302 998 E+0, +6.2 294 126 401 E-2,                   38 00280
     3    -1.9 367 439 704 E-1, +9.5 294 089 001 E-3,                   38 00290
     4    +1.0 021 677 762 E-2, -1.7 669 280 217 E-3,                   38 00300
     5    +7.9 027 635 693 E-5/                                         38 00310
      DATA B/                                                           38 00320
     1    +1.0 000 000 000 E+0, +6.7 641 463 495 E-1,                   38 00330
     2    +3.2 773 507 466 E-1, +1.0 279 994 528 E-1,                   38 00340
     3    +2.7 018 504 538 E-2, +5.1 647 208 257 E-3,                   38 00350
     4    +8.7 521 995 448 E-4, +9.5 129 148 083 E-5,                   38 00360
     5    +9.9 862 892 410 E-6/                                         38 00370
                                                                        38 00380
*  POLYNOMIAL EVALUATIONS.                                              38 00390
                                                                        38 00400
      P = A(9)                                                          38 00410
      Q = B(9)                                                          38 00420
      DO 10 I = 8, 1, -1                                                38 00430
         P = P * X + A(I)                                               38 00440
         Q = Q * X + B(I)                                               38 00450
   10 CONTINUE                                                          38 00460
                                                                        38 00470
*  TAKE RATIO OF TWO POLYNOMIALS.                                       38 00480
                                                                        38 00490
      GAM2 = Q / P                                                      38 00500
      RETURN                                                            38 00510
      END                                                               38 00520
      COMPLEX FUNCTION HYPERG(A, B, X)                                  39 00010
                                                                        39 00020
************************************************************************39 00030
*                                                                      *39 00040
*  Z = HYPERGEOMETRIC(A, B, X).                                        *39 00050
*                                                                      *39 00060
*  ADOPTED FROM 1604 SUBROUTINE OF C.W. NESTOR.                        *39 00070
*                                                                      *39 00080
************************************************************************39 00090
                                                                        39 00100
*  FUNCTION ARGUMENTS.                                                  39 00110
                                                                        39 00120
      COMPLEX           A                                               39 00130
      COMPLEX           B                                               39 00140
      COMPLEX           X                                               39 00150
                                                                        39 00160
*  Function References.                                                 39 00170
                                                                        39 00180
      Real              CABS                                            39 00190
      Intrinsic         CABS                                            39 00200
                                                                        39 00210
*  LOCAL VARIABLES.                                                     39 00220
                                                                        39 00230
      COMPLEX           APN                                             39 00240
      COMPLEX           BPN                                             39 00250
      COMPLEX           FN                                              39 00260
      INTEGER           N                                               39 00270
      REAL              PREC                                            39 00280
      PARAMETER        (PREC = 1.0E-6)                                  39 00290
      COMPLEX           T                                               39 00300
      COMPLEX           TEST                                            39 00310
                                                                        39 00320
*  INITIALIZE VARIABLES.                                                39 00330
                                                                        39 00340
      APN = A                                                           39 00350
      BPN = B                                                           39 00360
      FN = 0.0                                                          39 00370
      T = 1.0                                                           39 00380
      HYPERG = 1.0                                                      39 00390
                                                                        39 00400
*  ITERATE UNTIL PRECISION MET.                                         39 00410
*     IF > 30 ITERATIONS => ERROR.                                      39 00420
                                                                        39 00430
      DO 10 N = 1, 30                                                   39 00440
         FN = FN + 1.0                                                  39 00450
         T = T * APN * X / FN / BPN                                     39 00460
         APN = APN + 1.0                                                39 00470
         BPN = BPN + 1.0                                                39 00480
         TEST = T / HYPERG                                              39 00490
         HYPERG = HYPERG + T                                            39 00500
         IF (CABS(TEST) .LT. PREC) RETURN                               39 00510
   10 CONTINUE                                                          39 00520
      WRITE(6, 100) A, B, X, HYPERG, N                                  39 00530
  100 FORMAT(' ERROR IN HYPERG'/                                        39 00540
     1    4(5X, '(', E20.10, ',', E20.10, ')'/), I10)                   39 00550
      HYPERG = 0.                                                       39 00560
      RETURN                                                            39 00570
      END                                                               39 00580
