//AOC1LV1   JOB NOTIFY=&SYSUID
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

GET_LINE GET   FILEIN,RECIN
         MVI   RECOUT,C' '                  Blank Output
         MVC   RECOUT+1(L'RECOUT-1),RECOUT  Blank Output

         MVC   RECOUT(8),RECIN  put first 8 chars as indicator

*Basically we keep the old logic below, we just add a number to the
* spelled out numbers, like two becomes 2wo, three becomes 3hree

         LA    R4,RECIN           R4 will have the line start and roll
         LA    R5,L'RECIN(0,R4)   R5 will have the line end
REPLACE_LOOP EQU *
CK1      CLC   =C'one',0(R4)
         BNE   CK2
         MVI   0(R4),C'1'
CK2      CLC   =C'two',0(R4)
         BNE   CK3
         MVI   0(R4),C'2'
CK3      CLC   =C'three',0(R4)
         BNE   CK4
         MVI   0(R4),C'3'
CK4      CLC   =C'four',0(R4)
         BNE   CK5
         MVI   0(R4),C'4'                  
CK5      CLC   =C'five',0(R4)
         BNE   CK6
         MVI   0(R4),C'5'
CK6      CLC   =C'six',0(R4)
         BNE   CK7
         MVI   0(R4),C'6'
CK7      CLC   =C'seven',0(R4)
         BNE   CK8
         MVI   0(R4),C'7'
CK8      CLC   =C'eight',0(R4)
         BNE   CK9
         MVI   0(R4),C'8'                  
CK9      CLC   =C'nine',0(R4)
         BNE   CK_END
         MVI   0(R4),C'9'

CK_END EQU * 

         LA    R4,1(0,R4) add 1 to r4
         CR    R4,R5
         BL    REPLACE_LOOP Still low, keep looking
*Old logic
         LA    R2,SCANTAB     Set R2 - Will have the function code tab
         LA    R6,RECIN       R6 will have the address of RECIN
         TRT   0(L'RECIN,R6),0(R2)       Test for a number
         JZ    GET_LINE       Didn't find a number

         STCM  R2,X'0001',RECOUT+10

         LA    R2,SCANTAB     Set R2 - Will have the function code tab
         LA    R6,RECIN       R6 will have the end address of RECIN
         AHI   R6,L'RECIN-1      - add length for end
         TRTR  0(L'RECIN,R6),0(R2)       Test for a number
         JZ    GET_LINE       Didn't find a number

         STCM  R2,X'0001',RECOUT+11

         PACK  CPACKED,RECOUT+10(2)

         CVB   R0,CPACKED          Convert file size on R0

         AR    R7,R0   Add to sum

         CVD   R7,CPACKED
         UNPK  RECOUT+20(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+20+7,C' '  Get rid of the "C" for packed decimal

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
               LRECL=80
*
FILEOUT  DCB   DSORG=PS,                                               X
               MACRF=(PM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEOUT,                                         X
               RECFM=FB,                                               X
               LRECL=80
*
RECIN    DS    CL80       INPUT AREA FOR RECORDS
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
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#1)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO        EXEC PGM=AOC23#1,COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD SYSOUT=*
//DATASORT DD SYSOUT=*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  *
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
//
