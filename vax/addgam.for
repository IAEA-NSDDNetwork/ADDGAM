************************************************************************01 00010
*PROGRAM WRITTEN BY J. K. TULI, NNDC                                   *01 00020
*PROGRAM TO ADD GAMMAS IN ADOPTED LEVELS WHEN ALL G's COME FROM ONE    *01 00030
*DATA SET.  IF GAMMAS COME FROM MORE THAN ONE DATASET BUT ARE NON-     *01 00040
*OVERLAPPING, THE PROGRAM MAY BE RUN SUCCESSIVELY WITH DIFFERENT GAMMA *01 00050
*DATA SETS AS INPUT                                                    *01 00060
*INPUT: FILES ADOPL,ADOPG                                              *01 00070
*       ADOPL - DATA SET CONTAINING  ADOPTED LEVELS                    *01 00080
*       ADOPG - DATA SET CONTAINING  GAMMAS                            *01 00090
*OUTPUT:ADOPLG - MERGED DATA SET ADOPL WITH GAMMAS FROM ADOPG          *01 00100
*       WITH IG CHANGED TO BRANCHING FROM EACH LEVEL                   *01 00110
*LINK WITH NSDFLIB                                                     *01 00130
************************************************************************01 00140
*VERSION 1       9-19-86                                               *01 00150
*VERSION 1(1)    1-15-87      IbmPC MDC added.  NE changed to NEQV     *01 00160
*VERSION 1(2)    11-2-87      VAX MDC READONLY added in OPEN data file *01 00170
*VERSION 1(3)    13-Apr-93    Specifically typed all variables and     *01 00180
*                               functions                              *01 00190
*                             Delinted using FLINT 2.83                *01 00200
*                             (TWB)                                    *01 00210
*Version 1.4     7-Feb-01     Added UNX MDC code for Linux             *01 00212
************************************************************************01 00220
        PROGRAM ADDGAM                                                  01 00230
C                                                                       01 00240
        Integer i,j                                                     01 00250
        CHARACTER*80 CARD,LCARD,E*10                                    01 00260
        CHARACTER*30 ADOPL,ADOPG,ADOPLG                                 01 00270
        Real elev,delev                                                 01 00280
        LOGICAL NUME                                                    01 00290
C                                                                       01 00300
        INTEGER Lenstr,RLSCN                                            01 00310
        External Lenstr,Rlscn                                           01 00320
C                                                                       01 00330
*UNIT 5,6 ARE USER'S TERMINALS                                          01 00340
        WRITE(6,200)                                                    01 00350
        READ(5,300)ADOPL                                                01 00360
        WRITE(6,210)                                                    01 00370
        READ(5,300)ADOPG                                                01 00380
        WRITE(6,220)                                                    01 00390
        READ(5,300)ADOPLG                                               01 00400
C+++MDC+++                                                              01 00410
C...VAX, DVF                                                            01 00420
       OPEN(UNIT=21,FILE=ADOPL,STATUS='UNKNOWN',READONLY)               01 00430
       OPEN(UNIT=22,FILE=ADOPG,STATUS='UNKNOWN',READONLY)               01 00440
      OPEN(UNIT=23,FILE=ADOPLG,STATUS='UNKNOWN',                        01 00442
     +CARRIAGECONTROL='LIST')                                           01 00444
C...UNX, ANS                                                            01 00450
C/        OPEN(UNIT=21,FILE=ADOPL,STATUS='OLD')                         01 00460
C/        OPEN(UNIT=22,FILE=ADOPG,STATUS='OLD')                         01 00470
C/        OPEN(UNIT=23,FILE=ADOPLG,STATUS='UNKNOWN')                    01 00540
C---MDC---                                                              01 00550
        LCARD=' '                                                       01 00560
*READ ADOPTED LEVELS AND COPY ALL CARDS UNTIL 'L' CARD                  01 00570
1       READ(21,100,END=99)CARD                                         01 00580
        IF(CARD.EQ.' ')GOTO 2                                           01 00590
        IF(CARD(6:9).NE.'  L ') THEN                                    01 00600
           WRITE(23,100)CARD                                            01 00610
           GOTO 1                                                       01 00620
        ENDIF                                                           01 00630
*CHECK IF THIS IS THE FIRST 'L' CARD                                    01 00640
2       IF(LCARD.EQ.' ')GOTO 5                                          01 00650
*CHECK IF ENERGY ON 'L' CARD IS NON-NUMERIC                             01 00660
        E=LCARD(10:19)                                                  01 00670
        CALL SQZSTR(E,' ')                                              01 00680
        I=RLSCN(E,1,ELEV)                                               01 00690
*I IS THE POSITION AFTER THE REAL NO. IN E                              01 00700
        J=LENSTR(E)                                                     01 00710
        NUME=.TRUE.                                                     01 00720
*CHECK FOR NON-NUMERIC CHARACTERS IN ENERGY FIELD                       01 00730
        IF(J.GE.I)NUME=.FALSE.                                          01 00740
        CALL CNVS2U(E,CARD(20:21),ELEV,DELEV)                           01 00750
*COPY HERE GAMMA RECORDS FOLLOWED BY NEXT LEVEL RECORD                  01 00760
        CALL READG(E,ELEV,DELEV,NUME)                                   01 00770
5       WRITE(23,100)CARD                                               01 00780
        LCARD=CARD                                                      01 00790
        GOTO 1                                                          01 00800
99      CLOSE(UNIT=21)                                                  01 00810
        CLOSE(UNIT=22)                                                  01 00820
        CLOSE(UNIT=23)                                                  01 00830
100     FORMAT(A80)                                                     01 00840
200     FORMAT(' DATA SET WITH ADOPTED LEVELS (GIVE FILE NAME):')       01 00850
210     FORMAT(' DATA SET WITH GAMMAS (GIVE FILE NAME):')               01 00860
220     FORMAT(' NEW DATA SET TO BE CREATED (GIVE FILE NAME):')         01 00870
300     FORMAT(A30)                                                     01 00880
        END                                                             01 00890
*PROCESS DATA SET WITH G RECORDS                                        02 00010
        SUBROUTINE READG(E,ELEV,DELEV,NUME)                             02 00020
C                                                                       02 00030
        Character*10 e*10                                               02 00040
        Real elev,delev                                                 02 00050
        Logical nume                                                    02 00060
C                                                                       02 00070
        CHARACTER*80 CARD,LCARD,E1*10,GCARD(25)                         02 00080
        Real elev1,delev1,xlev                                          02 00090
        INTEGER i,j,NCALL,ng                                            02 00100
        LOGICAL NUME1,LFOUND                                            02 00110
C                                                                       02 00120
        Integer Lenstr,Rlscn                                            02 00130
        External Lenstr,Rlscn                                           02 00140
        Data ncall/0/
        Save
C                                                                       02 00150
        NG=0                                                            02 00160
*LFOUND IS TRUE WHEN THE LEVEL MATCH IS FOUND                           02 00170
        LFOUND=.FALSE.                                                  02 00180
*CHECK IF THIS IS THE FIRST CALL TO THIS SUBROUTINE                     02 00190
        IF(NCALL.NE.0)THEN                                              02 00200
          CARD=LCARD                                                    02 00210
          GOTO 5                                                        02 00220
        ENDIF                                                           02 00230
1       READ(22,100,END=999)CARD                                        02 00240
*REJECT ALL CARDS TILL 'L' CARD MATCH IS FOUND                          02 00250
        IF( .NOT. LFOUND .AND. CARD(6:9).NE.'  L ') GOTO 1              02 00260
        IF(LFOUND )THEN                                                 02 00270
*CHECK IF IT IS THE NEXT LEVEL OR END CARD (ALL G'S HAVE BEEN READ)     02 00280
             IF(CARD(6:9) .EQ. '  L '.OR.CARD.EQ.' ')THEN               02 00290
                    LCARD=CARD                                          02 00300
                    IF(NG .GT. 0)CALL WRITEG(GCARD,NG)                  02 00310
                    GOTO 99                                             02 00320
             ENDIF                                                      02 00330
*REJECT ALL CARDS OTHER THAN G CARDS                                    02 00340
             IF(CARD(6:9) .NE. '  G ')GOTO 1                            02 00350
             NG=NG+1                                                    02 00360
             IF(NG.GT.25)THEN                                           02 00370
               WRITE(6, 200)E                                           02 00380
               GOTO 99                                                  02 00390
             ENDIF                                                      02 00400
             GCARD(NG)=CARD                                             02 00410
             GOTO 1                                                     02 00420
        ENDIF                                                           02 00430
5       E1=CARD(10:19)                                                  02 00440
*REMOVE BLANKS FROM ENERGY FIELD                                        02 00450
        CALL SQZSTR(E1,' ')                                             02 00460
        CALL CNVS2U(E1,CARD(20:21),ELEV1,DELEV1)                        02 00470
        IF(ABS(ELEV-ELEV1).LE.DELEV+DELEV1)GOTO 10                      02 00480
        IF(ELEV1.GT.ELEV)GOTO 99                                        02 00490
        IF(ELEV1.LT.ELEV)GOTO 1                                         02 00500
*CHECK IF ENERGY FIELD HAS NON-NUMERIC COMPONENT                        02 00510
10      I=RLSCN(E1,1,XLEV)                                              02 00520
*I IS THE POSITION AFTER THE REAL NO. IN E1                             02 00530
        J=LENSTR(E1)                                                    02 00540
        NUME1=.TRUE.                                                    02 00550
*CHECK FOR NON-NUMERIC CHARACTERS IN ENERGY FIELD                       02 00560
        IF(J.GE.I)NUME1=.FALSE.                                         02 00570
        IF(NUME .NEQV. NUME1)GOTO 1                                     02 00580
        IF(.NOT. NUME)THEN                                              02 00590
           WRITE(6,400)E,E1                                             02 00600
        ENDIF                                                           02 00610
        LFOUND=.TRUE.                                                   02 00620
        NG=0                                                            02 00630
        GOTO 1                                                          02 00640
99      NCALL=NCALL+1                                                   02 00650
        RETURN                                                          02 00660
999     WRITE(6,300)E                                                   02 00670
100     FORMAT(A80)                                                     02 00680
200     format(' *** More than 25 gammas *** E(level) = ',A10)          02 00690
300     FORMAT(' *** No match for this level *** E(level) = ',A10)      02 00700
400     FORMAT(' **NON-NUMERIC E FIELD**',5X,A10,'  MATCHED WITH ',A10) 02 00710
        END                                                             02 00720
*                                                                       03 00010
*CALCULATE BRANCHING RATIOS                                             03 00020
        SUBROUTINE WRITEG(GCARD,NG)                                     03 00030
C                                                                       03 00040
        CHARACTER*80 GCARD(25)                                          03 00050
        Integer ng                                                      03 00060
C                                                                       03 00070
        Integer i,j                                                     03 00080
        Character*2 unc                                                 03 00090
        REAL XIG(25),DIG(25),xmax,xn                                    03 00100
C                                                                       03 00110
        INTEGER TYPSTR                                                  03 00120
        External Typstr                                                 03 00130
C                                                                       03 00140
        XMAX=0.                                                         03 00150
        DO 10 I=1,NG                                                    03 00160
        CALL CNVS2U(GCARD(I)(22:29),GCARD(I)(30:31),XIG(I),DIG(I))      03 00170
        IF(XIG(I).GT.XMAX)XMAX=XIG(I)                                   03 00180
10      CONTINUE                                                        03 00190
        IF(XMAX .EQ.0.)XN=1.                                            03 00200
        IF(XMAX .GT.0.)XN=100./XMAX                                     03 00210
        DO 20 I=1,NG                                                    03 00220
        XIG(I)=XIG(I)*XN                                                03 00230
        DIG(I)=DIG(I)*XN                                                03 00240
*IF ONLY ONE GAMMA FROM THE LEVEL THEN NO UNCERTAINTY                   03 00250
        IF(NG.EQ.1)DIG(I)=0.                                            03 00260
*CHECK FOR NON-NUMERIC UNCERTAINTY                                      03 00270
        UNC=GCARD(I)(30:31)                                             03 00280
        J=TYPSTR(UNC)                                                   03 00290
        IF (J.EQ.0.OR.J.EQ.2 )DIG(I)=0.                                 03 00300
        CALL CNVU2S(XIG(I),DIG(I),GCARD(I)(22:29),8,GCARD(I)(30:31),2)  03 00310
        IF (J.EQ.0.OR.J.EQ.2)GCARD(I)(30:31)=UNC                        03 00320
        WRITE(23,100)GCARD(I)                                           03 00330
20      CONTINUE                                                        03 00340
100     FORMAT(A80)                                                     03 00350
        RETURN                                                          03 00360
        END                                                             03 00370
