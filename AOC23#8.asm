//AOC1LV8   JOB NOTIFY=&SYSUID
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
PROG1    AMODE 31
PROG1    RMODE 24
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
* R2 - 
* R3 - 
* R4 -
* R5 -
* R6 -
* R7 -   save ZZZ addr to know we are finished
* R8 -
* R9 - 
* R10 - 
* R11 - Work area

         STORAGE OBTAIN,LENGTH=WORKLEN,LOC=31,ADDR=(R11),CHECKZERO=YES

* calculate ZZZ's address, our goal
         LR    R7,R11
         AFI   R7,((C'ZZZ'-C'AAA')*8)            get offset from start
         CVD   R7,CPACKED              put into packed for display
         UNPK  RECOUT+0(16),CPACKED+1
         MVC   RECOUT+0+14(2),=C'  ' Get rid of the "C" for p.dec
         PUT   FILEOUT,RECOUT          put for debugging


GET_LOOP EQU *

         GET   FILEIN,RECORD           get record
 
         MVC   RECOUT,RECORD           put in out for debugging
 
*        PUT   FILEOUT,RECOUT          put for debugging
 
* get index
         SR    R2,R2
         ICM   R2,B'0111',RECORD       load number into R2
         AFI   R2,(-C'AAA')            get offset from start
         MSFI  R2,8                    multiply by 8

         LA    R1,0(R2,R11)         
         CVD   R1,CPACKED              put into packed for display
         UNPK  RECOUT+0(16),CPACKED+1
         MVC   RECOUT+0+14(2),=C'  ' Get rid of the "C" for p.dec

* get L
         SR    R1,R1 
         ICM   R1,B'0111',RECORD+7     load number into R1
         AFI   R1,(-C'AAA')            get offset from start
         MSFI  R1,8                    multiply by 8
         AR    R1,R11                  get actual address
         ST    R1,0(R2,R11)            put L's address

         CVD   R1,CPACKED              put into packed for display
         UNPK  RECOUT+20(16),CPACKED+1
         MVC   RECOUT+20+14(2),=C'  ' Get rid of the "C" for p.dec

* get R
         SR    R1,R1 
         ICM   R1,B'0111',RECORD+12    load number into R1
         AFI   R1,(-C'AAA')            get offset from start
         MSFI  R1,8                    multiply by 8
         AR    R1,R11                  get actual address         
         ST    R1,4(R2,R11)            put R's address

         CVD   R1,CPACKED              put into packed for display
         UNPK  RECOUT+40(16),CPACKED+1
         MVC   RECOUT+40+14(2),=C'  ' Get rid of the "C" for p.dec


         PUT   FILEOUT,RECOUT

*        LA    R3,0(0,R11)         
*        AFI   R3,(C'CSV'-C'AAA')
*        PUT   FILEOUT,0(R3)


         J     GET_LOOP


FILE#EOD EQU *

* Here the storage should have been completely mapped
         SR    R9,R9                   R9 will have step count
         LR    R2,R11                  R2 will have addr, init at off0

R_L_LIST_END EQU *
         LA    R10,DIRECTION_TAB       R10 will have next direction

KEEP_GOING EQU *
         CLI   0(R10),C'L'
         JNE   NOT_L
         L     R2,0(0,R2)            load L's address
         J     TOOK_DIRECTION 
NOT_L EQU *         
         CLI   0(R10),C'R'
         JNE   NOT_R
         L     R2,4(0,R2)            load R's address
         J     TOOK_DIRECTION 
NOT_R EQU *         
         J     R_L_LIST_END            assume we reached the end L/R
         

TOOK_DIRECTION EQU *         
         AHI   R9,+1                   step++ 
         AHI   R10,+1                  direction++    

*        LR    R4,R2 
*        L     R5,0(0,R2)              L for debug
*        L     R6,4(0,R2)              R for debug

         
         CVD   R2,CPACKED              put into packed for display
         UNPK  RECOUT+20(16),CPACKED+1
         MVC   RECOUT+20+14(2),=C'  ' Get rid of the "C" for p.dec              
         CVD   R9,CPACKED              put into packed for display
         UNPK  RECOUT+0(16),CPACKED+1
         MVC   RECOUT+0+14(2),=C'  ' Get rid of the "C" for p.dec
         PUT   FILEOUT,RECOUT

         CR    R2,R7                   are we at goal?
         JNE   KEEP_GOING              no, keep going

         CVD   R2,CPACKED              put into packed for display
         UNPK  RECOUT+20(16),CPACKED+1
         MVC   RECOUT+20+14(2),=C'  ' Get rid of the "C" for p.dec              

         CVD   R9,CPACKED              put into packed for display
         UNPK  RECOUT+0(16),CPACKED+1
         MVC   RECOUT+0+14(2),=C'  ' Get rid of the "C" for p.dec

         PUT   FILEOUT,RECOUT


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
RECORD   DC    CL(80)' '      INPUT AREA FOR RECORDS

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
*EXPACK   PACK  CPACKED,0(0,R1)
*TRTEX    TRT   0(0,R6),0(R2)       Test for a number for EX
*ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS

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
DIRECTION_TAB EQU *
*        DC    C'RL'
         DC    C'LLR'         
* DC C'LLLRRLRRRLLRRLRRLLRLRRLRRRLRRLRRLRRRLRLRRLRLRRLRRLLRRLRLLRRLL'
* DC C'LRRRLRRLRLRLRRRLRLLRRLRRRLRRLRRLRRLRLLRLLRRLRRRLRRLRLRRLRRRLR'
* DC C'RLLRLLRRLRRRLLRRRLRLRRRLLRLRRLRRLLRRLRRLLLRRRLRLRRRLRRLLRLRRL'
* DC C'RLLRRRLRLRLLRLRRRLRLRRRLRRLRLRLLRLRRRLRRLRRRLRRRLRLRRRLRRRLLL'
* DC C'LRLRLRRRLLLRLRRRLRRLRLRRLLRLLRRRR'
DIRECTION_TAB_LEN EQU *-DIRECTION_TAB
         DC    X'00' 00 to signal end of tab
         YREGS
MAXINDEX EQU   C'ZZZ'  
MININDEX EQU   C'AAA'
RANGEINDEX EQU MAXINDEX-MININDEX
WORKLEN  EQU   RANGEINDEX*8+100
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#8)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//GO       EXEC PGM=AOC23#8,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD  SYSOUT=*
//FILEOUT  DD  SYSOUT=B
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//XFILEIN   DD  *
AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
//FILEIN   DD  *
AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
