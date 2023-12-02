//AOC1LV2   JOB NOTIFY=&SYSUID
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
*
         SR    R7,R7          Clear R7 - Will have the sum value
         SR    R8,R8          Clear R8 - Will have the current game#

GET_LINE GET   FILEIN,RECIN
         LA    R8,1(0,R8)       Add 1 to game number
         MVI   RECOUT,C' '                  Blank Output
         MVC   RECOUT+1(L'RECOUT-1),RECOUT  Blank Output

         MVC   RECOUT(10),=C'Next Game:'

         CVD   R8,CPACKED
         UNPK  RECOUT+10(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+10+7(2),=C'  ' Get rid of the "C" for packed dec
         PUT   FILEOUT,RECOUT


         LA    R5,RECIN
         LA    R6,L'RECIN(0,R5)
CHECK_RED_LOOP EQU *
*        PUT   FILEOUT,=CL80'RED LOOP'
         LA    0,C'r'              Get next "r"
         SRST  R6,R5
*  SRST possible condition codes
*   Condition       Mask
*     Code        Bit Value
*   ---------     ---------
*      0              8     - Not applicable
*      1              4     - Char found, R1 has address of char
*      2              2     - Char not found R1 unchanged
*      3              1     - CPU limit reached - shouldn't happen
*
         BC    8+2+1,CHECK_GREEN  Not found, continue
         LA    R2,1(R6)            Save start of scan, next char on R2
         CLC   =C'red',0(R6)     is it what we need to replace?
         BE    RED_FOUND
         LA    R6,L'RECIN(0,R5)         Reset End of scan
         LR    R5,R2               Reset start of scan from saved R2
         B     CHECK_RED_LOOP      Retry scan
*
RED_FOUND EQU *
         AHI   R6,-3  get back 3 characters to get the number
         MVC   RECOUT+30(6),0(R6) move to print
         PUT   FILEOUT,RECOUT  print
         CLC   0(2,R6),=C'12'   compare with 12 red cubes
         BH    GET_LINE too high, get next line
         LA    R6,L'RECIN(0,R5)         Reset End of scan
         LR    R5,R2               Reset start of scan from saved R2
         B     CHECK_RED_LOOP      Retry scan next red
CHECK_GREEN EQU *
         LA    R5,RECIN
         LA    R6,L'RECIN(0,R5)
CHECK_GREEN_LOOP EQU *
*        PUT   FILEOUT,=CL80'GREEN LOOP'
         LA    0,C'g'              Get next "g"
         SRST  R6,R5
*  SRST possible condition codes
*   Condition       Mask
*     Code        Bit Value
*   ---------     ---------
*      0              8     - Not applicable
*      1              4     - Char found, R1 has address of char
*      2              2     - Char not found R1 unchanged
*      3              1     - CPU limit reached - shouldn't happen
*
         BC    8+2+1,CHECK_BLUE  Not found, continue
         LA    R2,1(R6)            Save start of scan, next char on R2
         CLC   =C'green',0(R6)     is it what we need to replace?
         BE    GREEN_FOUND
         LA    R6,L'RECIN(0,R5)         Reset End of scan
         LR    R5,R2               Reset start of scan from saved R2
         B     CHECK_GREEN_LOOP    Retry scan
*
GREEN_FOUND EQU *
         AHI   R6,-3  get back 3 characters to get the number
         MVC   RECOUT+30(8),0(R6) move to print
         PUT   FILEOUT,RECOUT  print
         CLC   0(2,R6),=C'13'   compare with 13 green cubes
         BH    GET_LINE too high, get next line
         LA    R6,L'RECIN(0,R5)         Reset End of scan
         LR    R5,R2               Reset start of scan from saved R2
         B     CHECK_GREEN_LOOP    Retry scan next green
*
CHECK_BLUE EQU *
         LA    R5,RECIN
         LA    R6,L'RECIN(0,R5)
CHECK_BLUE_LOOP EQU *
*        PUT   FILEOUT,=CL80'BLUE LOOP'
         LA    0,C'b'              Get next "b"
         SRST  R6,R5
*  SRST possible condition codes
*   Condition       Mask
*     Code        Bit Value
*   ---------     ---------
*      0              8     - Not applicable
*      1              4     - Char found, R1 has address of char
*      2              2     - Char not found R1 unchanged
*      3              1     - CPU limit reached - shouldn't happen
*
         BC    8+2+1,CHECK_COMPLETE   Not found, continue
         LA    R2,1(R6)            Save start of scan, next char on R2
         CLC   =C'blue',0(R6)     is it what we need to replace?
         BE    BLUE_FOUND
         LA    R6,L'RECIN(0,R5)         Reset End of scan
         LR    R5,R2               Reset start of scan from saved R2
         B     CHECK_BLUE_LOOP     Retry scan
*
BLUE_FOUND EQU *
         AHI   R6,-3  get back 3 characters to get the number
         MVC   RECOUT+30(7),0(R6) move to print
         PUT   FILEOUT,RECOUT  print
         CLC   0(2,R6),=C'14'   compare with 14 BLUE cubes
         BH    GET_LINE too high, get next line
         LA    R6,L'RECIN(0,R5)         Reset End of scan
         LR    R5,R2               Reset start of scan from saved R2
         B     CHECK_BLUE_LOOP     Retry scan next BLUE
*
CHECK_COMPLETE EQU *
         AR    R7,R8  Add game number to sum
*
         CVD   R7,CPACKED
         UNPK  RECOUT+20(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+20+7(2),=C'  ' Get rid of the "C" for packed dec

         PUT   FILEOUT,RECOUT
         J     GET_LINE
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
               LRECL=255
*
FILEOUT  DCB   DSORG=PS,                                               X
               MACRF=(PM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEOUT,                                         X
               RECFM=FB,                                               X
               LRECL=80
*
RECIN    DS    CL255      INPUT AREA FOR RECORDS
BLANK    DC    C' '       TO BLANK RECOUT
*
BIN_VAL  DS    X'FFFFFFFF',X'FF'
* Record out
RECOUT   DS    CL80       OUTPUT AREA FOR RECORDS
         ORG   *-L'OUT_VAL
OUT_VAL  DC    CL8'XXXXXXXX',C'<'
*
TEMPNUM  DS    CL8
CPACKED  DS    D
PACK     PACK  CPACKED,RECIN(0)
ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS

HEXTAB   DC    C'0123456789ABCDEF'

* Define scantab
SCANTAB  DC    256X'00'         All of the characters, except for
         ORG   SCANTAB+C'0'       numbers, are set to a non-zero
         DC    C'0123456789'              value.
         ORG
*
         LTORG
         YREGS
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#2)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO        EXEC PGM=AOC23#2,COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD SYSOUT=*
//DATASORT DD SYSOUT=*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  DISP=SHR,DSN=AOC1LV.LEO.INPUT(AOC23I2)
//
//
//
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
/*
