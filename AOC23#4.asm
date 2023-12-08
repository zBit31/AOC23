//AOC1LV3   JOB NOTIFY=&SYSUID
//ASM       EXEC PGM=ASMA90,PARM='NODECK,XREF(SHORT)'
//SYSLIB    DD  DSN=SYS1.MACLIB,DISP=SHR
//          DD  DSN=SYS1.MODGEN,DISP=SHR
//*         DD  DSN=SYS1.ISFMAC,DISP=SHR
//SYSUT1    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT2    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT3    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPRINT  DD SYSOUT=*
//SYSLIN    DD SPACE=(CYL,10),UNIT=VIO,DISP=(,PASS)
//SYSIN     DD *,SYMBOLS=EXECSYS
         PRINT ON,NODATA,NOGEN
PROG1    CSECT
* STANDARD LINKAGE
**********************************************************************
         BAKR  R14,0               SAVE ALL REGISTERS              @NSC
         BASR  R12,R0                 ESTABLISH
         USING *,R12                  ADDRESSABILITY
         LA    R13,SAVEAREA           POINT TO MY LOWER-LEVEL SA
**********************  BEGIN LOGIC  *********************************
         OPEN  (FILEIN,(INPUT))
         OPEN  (FILEOUT,(OUTPUT))
* Dedicated registers:
* R2 - Position on input data
* R3 - n/a
* R4 - number countdown for BC
* R5 - 
* R6 - 
* R7 - current card points
* R8 - Will have the sum of points
* R9 - 
* R10 - N/A / work
* R11 - SUBROUTINE return address
         SR    R8,R8          Clear R8

* get record
GET_LINE EQU *

         SR    R7,R7          Clear R7

         MVI   RECOUT,C' '                  Blank Output
         MVC   RECOUT+1(L'RECOUT-1),RECOUT  Blank Output

         MVI   WINMAP,X'00'
         MVC   WINMAP+1(L'WINMAP-1),WINMAP  clear map

         GET   FILEIN,RECORD         get new record

         LA    R4,WINNING#

         LA    R2,RECORD+FIRSTPOS    get r2 to 1st number

WINLOOP  EQU *


         PACK  CPACKED,0(2,R2)       pack it
         CVB   R1,CPACKED            convert to binary
         LA    R3,WINMAP             load win map
         AR    R3,R1                 add winning number
         MVI   0(R3),X'FF'              set it to winning

         LA    R2,3(R2)              get to next number

         BCT   R4,WINLOOP            * loop until done with win num

         

*         CVD   R0,CPACKED
*         UNPK  RECOUT+10(9),CPACKED(L'CPACKED+1)
*         MVC   RECOUT+10+7(2),=C'  ' Get rid of the "C" for packed dec         

         MVC   RECOUT,WINMAP         copy winmap for diag
         PUT   FILEOUT,RECOUT

* Here we have the map of all winning numbers, let's check our nums
         LA    R4,MYNUM

         LA    R2,2(R2)              get to my numbers 3+2

MYLOOP   EQU *

         PACK  CPACKED,0(2,R2)       pack it


         CVB   R1,CPACKED            convert to binary
         LA    R3,WINMAP             load win map
         AR    R3,R1                 add winning number

         CLI   0(R3),X'FF'           is it winning?
         BNE   MYLOOP_END            nope

         LTR   R7,R7
         BNZ   ALREADY_WINNER
* first winner, 1 point.
         LA    R7,1 
         B     MYLOOP_END

* already winner, double the points
ALREADY_WINNER EQU *        
         MSFI   R7,2                  double the points


MYLOOP_END EQU *
         LA    R2,3(R2)              get to next number for next loop
         BCT   R4,MYLOOP            * loop until done with win num

         AR    R8,R7

         CVD   R7,CPACKED
         UNPK  RECOUT+10(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+10+7(2),=C'  ' Get rid of the "C" for packed dec         

         CVD   R8,CPACKED
         UNPK  RECOUT+20(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+20+7(2),=C'  ' Get rid of the "C" for packed dec         

         PUT   FILEOUT,RECOUT

         J     GET_LINE

*
*
FILE#EOD EQU   *
         CLOSE FILEIN
         CLOSE FILEOUT
*********************** STARDARD EXIT ********************************
EXIT     EQU   *                      BRANCH TO HERE FOR NORMAL RETURN
         LA    R15,0                  SET RETURN CODE REG 15 = 0
         PR
*
ABEND    ABEND 1
**********************  DATA AREAS   *********************************
FILEIN   DCB   DSORG=PS,                                               X
               MACRF=(GM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEIN,                                          X
               EODAD=FILE#EOD,                                         X
               RECFM=FB,                                               X
               LRECL=L'RECORD
*
FILEOUT  DCB   DSORG=PS,                                               X
               MACRF=(PM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEOUT,                                         X
               RECFM=FB,                                               X
               LRECL=L'RECOUT
*
RECORD   DC    CL(255)' '      INPUT AREA FOR RECORDS

BLANK    DC    C' '       TO BLANK RECOUT
*
*BIN_VAL  DS    X'FFFFFFFF',X'FF'
* Record out
RECOUT   DS    CL80       OUTPUT AREA FOR RECORDS
*         ORG   *-L'OUT_VAL
*OUT_VAL  DC    CL8'XXXXXXXX',C'<'
*
*TEMPNUM  DS    CL8
CPACKED  DS    D
*EXPACK     PACK  CPACKED,0(0,R10)
*TRTEX    TRT   0(0,R6),0(R2)       Test for a number for EX
*ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS

WINMAP   DS    XL100

*FIRSTPOS EQU   8  * position of 1st number
*WINNING# EQU   5  * quantity of winning numbers
*MYNUM    EQU   8  * quantity of my numbers

FIRSTPOS EQU   10  * position of 1st number
WINNING# EQU   10  * quantity of winning numbers
MYNUM    EQU   25  * quantity of my numbers
*
         LTORG
         YREGS
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#4)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO       EXEC PGM=AOC23#4,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD  SYSOUT=*
//DATASORT DD  SYSOUT=*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  DISP=SHR,DSN=AOC1LV.LEO.INPUT(AOC23I4)
//
//
//FILEIN   DD  *
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
/*
//
//FILEIN   DD  DISP=SHR,DSN=AOC1LV.LEO.INPUT(AOC23I4)
//
//
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
.
/*
//
//
