//AOC1LV6   JOB NOTIFY=&SYSUID
//ASM       EXEC PGM=ASMA90,PARM='NODECK,XREF(SHORT)'
//SYSLIB    DD  DSN=SYS1.MACLIB,DISP=SHR
//          DD  DSN=SYS1.MODGEN,DISP=SHR
//*         DD  DSN=SYS1.ISFMAC,DISP=SHR
//SYSUT1    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT2    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT3    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPRINT  DD SYSOUT=B
//SYSLIN    DD SPACE=(CYL,10),UNIT=VIO,DISP=(,PASS)
//SYSIN     DD *,SYMBOLS=EXECSYS
         PRINT ON,NODATA,NOGEN
PROG1    CSECT
* STANDARD LINKAGE
**********************************************************************
         BAKR  R14,0               SAVE ALL REGISTERS
         BASR  R12,R0                 ESTABLISH
         USING *,R12                  ADDRESSABILITY
         LA    R13,SAVEAREA           POINT TO MY LOWER-LEVEL SA
**********************  BEGIN LOGIC  *********************************
         OPEN  (FILEIN,(INPUT))
         OPEN  (FILEOUT,(OUTPUT))
* Dedicated registers:
* R2 - READ_EBCDIC_NUM_ON_R2 subroutine / work
* R3 - READ_EBCDIC_NUM_ON_R2 subroutine / work
* R4 - Offset at time
* R5 - Offset at distance
* R6 - time of challenge
* R7 - distance to beat
* R8 - current race beat count
* R9 - beat margin multiply 
* R10 -
* R11 - SUBROUTINE return address

         GET   FILEIN,TIME             get time
         GET   FILEIN,DISTANCE         get distance

         LA    R4,TIME                 initialize time
         LA    R5,DISTANCE             initialize distance

         LA    R9,1                    start with 1 multiplier

*        L     R6,=A(46000)
FINDNUM EQU *
         SR    R8,R8                   clear beat count
* get time
         LA    R3,TIME+L'TIME          R3 - end of time
         SR    R3,R4                   R3 - length to end of time
         LR    R2,R4                   load into R2 for TRTE
         LA    R1,NUM_TAB
         TRTE  R2,R0,B'0000'           get next number
         BZ    DONE                    no number? done!

         BAS   R11,READ_EBCDIC_NUM_ON_R2  Go get

         LGR   R6,R1                   time into R6

         LR    R4,R2                   save address into R4

* get distance
         LA    R3,DISTANCE+L'DISTANCE  R3 - end of distance
         SR    R3,R5                   R3 - length to end of distance
         LR    R2,R5                   load into R2 for TRTE
         LA    R1,NUM_TAB
         TRTE  R2,R0,B'0000'           get next number
         BZ    DONE                    no number? done!

         BAS   R11,READ_EBCDIC_NUM_ON_R2  Go get

         LGR   R7,R1                   distance into R7

         LR    R5,R2                   save address into R5


         CVD   R6,CPACKED              put into packed for display
         UNPK  RECOUT+0(10),CPACKED+1
         MVC   RECOUT+0+8(2),=C'  ' Get rid of the "C" for p.dec

         CVD   R7,CPACKED              put into packed for display
         UNPK  RECOUT+10(10),CPACKED+1
         MVC   RECOUT+10+8(2),=C'  ' Get rid of the "C" for p.dec

         PUT   FILEOUT,RECOUT

* now we have to iterate time to calculate distance

         SR    R2,R2  start with zero time/speed

LOOP_CALC EQU *
         AHI   R2,1  add 1 to time/speed (R2)
         LR    R1,R6  load time into R1
         SR    R1,R2  time left
         MSR   R1,R2  get distance

         CVD   R1,CPACKED              put into packed for display
         UNPK  RECOUT+30(10),CPACKED+1
         MVC   RECOUT+30+8(2),=C'  ' Get rid of the "C" for p.dec

         CR    R1,R7   does it beat the record?
         BNH   NOT_BEAT
         AHI   R8,+1   add 1 to beat count


NOT_BEAT EQU *

         CVD   R2,CPACKED              put into packed for display
         UNPK  RECOUT+40(10),CPACKED+1
         MVC   RECOUT+40+8(2),=C'  ' Get rid of the "C" for p.dec
         CVD   R6,CPACKED              put into packed for display
         UNPK  RECOUT+50(10),CPACKED+1
         MVC   RECOUT+50+8(2),=C'  ' Get rid of the "C" for p.dec

         PUT   FILEOUT,RECOUT

         CR    R2,R6
         BL    LOOP_CALC


         MSR   R9,R8    add to multiplication


         J     FINDNUM


DONE EQU *
FILE#EOD EQU *

         CVD   R9,CPACKED              put into packed for display
         UNPK  RECOUT+60(10),CPACKED+1
         MVC   RECOUT+60+8(2),=C'  ' Get rid of the "C" for p.dec

         PUT   FILEOUT,RECOUT


         CLOSE FILEIN
         CLOSE FILEOUT



*********************** STARDARD EXIT ********************************
EXIT     EQU   *                      BRANCH TO HERE FOR NORMAL RETURN
         LA    R15,0                  SET RETURN CODE REG 15 = 0
         PR
*
ABEND    ABEND 1


* Routine to read EBCDIC number on R2 and then place it on R1
* Entry - R2 is start of number
* Will use Register pair R2/R3 for TRTE / TRTRE
* Return number on R1, address of end of number on R2
* Return to address on R11
READ_EBCDIC_NUM_ON_R2 EQU *

         LR    R0,R2                   save initial address on R0

         LA    R3,15                   max number len is 15

         LA    R1,NOT#_TAB             find a non-number
         TRTE  R2,R15,B'0000'          search for number

         LR    R15,R2                  save address on R15
         SR    R2,R0              subtract start address to get len

         AHI   R2,-1                   remove 1 for the EX
         LR    R1,R0                   load R1 from saved R0
         EX    R2,EXPACK               PACK  CPACKED,0(0,R1)

         CVB   R1,CPACKED              get number on R1
         LR    R2,R15                  get end address on R2

         BR    R11  return to called from address

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
RECORD   DC    CL(80)' '      INPUT AREA FOR RECORDS

TIME     DC    CL(80)' '      INPUT AREA FOR RECORDS
DISTANCE DC    CL(80)' '      INPUT AREA FOR RECORDS

BLANK    DC    C' '       TO BLANK RECOUT
*
*BIN_VAL  DS    X'FFFFFFFF',X'FF'
* Record out
RECOUT   DS    CL80       OUTPUT AREA FOR RECORDS
*         ORG   *-L'OUT_VAL
*OUT_VAL  DC    CL8'XXXXXXXX',C'<'
*
*TEMPNUM  DS    CL8
         DS    0D     needs to be on double
CPACKED  DS    D
EXPACK   PACK  CPACKED,0(0,R1)
*TRTEX    TRT   0(0,R6),0(R2)       Test for a number for EX
*ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS

LOWEST_LOCATION DC X'0FFFFFFFFFFFFFFF' start with a high location

* Define scantabs
NUM_TAB  DC    256X'00'         All of the characters, except for
         ORG   NUM_TAB+C'0'       numbers, are set to a zero
         DC    C'0123456789'              value.
         ORG

NOT#_TAB DC    256X'FF'         All of the characters, except for
         ORG   NOT#_TAB+C'0'       numbers, are set to a non-zero
         DC    X'00000000000000000000'        value.
         ORG
*
         LTORG
         YREGS
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#6)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//GO       EXEC PGM=AOC23#6,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD  SYSOUT=*
//FILEOUT  DD  SYSOUT=B
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  *
Time:      7  15   30
Distance:  9  40  200
