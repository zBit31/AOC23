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
* R2 - Length of the number
* R3 - Used in TRTE for MAX number length (pair of R2)
* R4 - Position on current record (Start of number)
* R5 - Length left to process (bytes to end of record)
* R6 - Position to start to check for symbols
* R7 - Used in TRTE for # of bytes for symbol search (pair of R6)
* R8 - Will have the sum
         SR    R8,R8          Clear R8


* The idea is to process the line as we read the next line
* so we will have the "next" line info



* get current 
         GET   FILEIN,REC_NEXT
* get next
GET_LINE EQU *

         MVI   RECOUT,C' '                  Blank Output
         MVC   RECOUT+1(L'RECOUT-1),RECOUT  Blank Output

         MVC   REC_PREV,REC_CUR        cur -> prev
         MVC   REC_CUR,REC_NEXT        next -> cur

         GET   FILEIN,REC_NEXT         get new next

* find a number
*        LA    R2,NUM_TAB     Set R2 - Will have the function code tab
*        LA    R6,REC_CUR       R6 will have the address of scan start
*        LA    R1,L'REC_CUR
*        EX    R1,TRTEX          TRT   0(L'REC_CUR,R6),0(R2)      
*        TRT   0(L'REC_CUR,R6),0(R2)       Test for a number
*        JZ    GET_LINE       Didn't find a number

*         SR    R2,R2
         LA    R4,REC_CUR 
         LA    R5,L'REC_CUR
*        L     R6,=A(46000)
FINDNUM EQU *
         LA    R1,NUM_TAB
         TRTE  R4,R0,B'0000'            Test for ascii chars
         JO    FINDNUM                  Still not at end (CPU lim)
*        SR    R6,R5
*        DC    X'00'
         JZ    GET_LINE                    No numbers, next line

         MVC   RECOUT(4),0(R4)
*         PUT   FILEOUT,RECOUT

* We found a number
         LA    R1,NOT#_TAB    We will find the end of the number (len)
         LR    R2,R4
         LA    R3,8      max 8 number length
         TRTE  R2,R0,B'0000'     search for next non-num

         SR    R2,R4     get length on R2

         CVD   R2,CPACKED
         UNPK  RECOUT+10(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+10+7(2),=C'  ' Get rid of the "C" for packed dec

* Let's see if it's a valid number, see if we find a symbol around it
         LR    R6,R4
         AHI   R6,(REC_PREV-REC_CUR)-1  Go to previous line and char
         LA    R7,2(0,R2)               Len to search is num len + 2

         LA    R1,SYM_TAB    try to find a symbol
         TRTE  R6,R0,B'0000'    search for symbol
         JNZ   ADD_NUMBER       symbol found, add number

* symbol not found on previous, let's check current
         LR    R6,R4
         AHI   R6,-1                   Go to previous char
         LA    R7,2(0,R2)              Len to search is num len + 2

         LA    R1,SYM_TAB    try to find a symbol
         TRTE  R6,R0,B'0000'    search for symbol
         JNZ   ADD_NUMBER       symbol found, add number

* symbol not found on current, let's check next line
         LR    R6,R4
         AHI   R6,(REC_NEXT-REC_CUR)-1 Go to next line previous char
         LA    R7,2(0,R2)              Len to search is num len + 2

         LA    R1,SYM_TAB    try to find a symbol
         TRTE  R6,R0,B'0000'    search for symbol
         JNZ   ADD_NUMBER       symbol found, add number

         J   DISP_TOTAL  

ADD_NUMBER EQU *

         MVC   RECOUT+5(1),0(R6)

         AHI   R2,-1               remove 1 for EX (0 len is 1)
         EX    R2,PACK             EX PACK CPACKED,0(0,R4)

* add to recout for display
         UNPK  RECOUT+20(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+20+7(2),=C'  ' Get rid of the "C" for packed dec
* add it to sum
         
         CVB   R0,CPACKED    convert to binary
         AR    R8,R0         add it to total

*        PUT   FILEOUT,RECOUT

DISP_TOTAL EQU *
* add total to display
         CVD   R8,CPACKED
         UNPK  RECOUT+30(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+30+7(2),=C'  ' Get rid of the "C" for packed dec

* to-do add logic to look next number on current line
* logic to look next number on current line
         LA    R4,1(R2,R4)       go past current number
         LA    R5,REC_CUR_END  load R5 with end address
         SR    R5,R4                   reduce current pos

*         CVD   R5,CPACKED
*         UNPK  RECOUT+60(9),CPACKED(L'CPACKED+1)
*         MVC   RECOUT+60+7(2),=C'  ' Get rid of the "C" for packed dec
         PUT   FILEOUT,RECOUT
*         J EXIT  
         MVI   RECOUT+5,C' '


         J     FINDNUM          find next number


*        PUT   FILEOUT,RECOUT



*        J     GET_LINE

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
               LRECL=L'REC_NEXT
*
FILEOUT  DCB   DSORG=PS,                                               X
               MACRF=(PM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEOUT,                                         X
               RECFM=FB,                                               X
               LRECL=L'RECOUT
*
         DC    C' '   boundary blank
REC_PREV DC    CL(L'REC_NEXT)' '  INPUT AREA FOR previous record
         DC    C' '   boundary blank
REC_CUR  DC    CL(L'REC_NEXT)' '  INPUT AREA FOR processed record
REC_CUR_END EQU *
         DC    C' '   boundary blank
REC_NEXT DC    CL(255)' '      INPUT AREA FOR RECORDS
         DC    C' '   boundary blank


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
PACK     PACK  CPACKED,0(0,R4) 
*TRTEX    TRT   0(0,R6),0(R2)       Test for a number for EX
*ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS

*HEXTAB   DC    C'0123456789ABCDEF'

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
SYM_TAB  DC    256X'00'         All of the characters, except for
         ORG   SYM_TAB+C'*'       symbols, are set to a non-zero
         DC    X'FF'              value.
         ORG   SYM_TAB+C'&&'      symbols, are set to a non-zero
         DC    X'FF'              value.
         ORG   SYM_TAB+C'%'       symbols, are set to a non-zero
         DC    X'FF'              value.
         ORG   SYM_TAB+C'-'      symbols, are set to a non-zero
         DC    X'FF'              value.        
         ORG   SYM_TAB+C'='       symbols, are set to a non-zero
         DC    X'FF'              value.
         ORG   SYM_TAB+C'#'      symbols, are set to a non-zero
         DC    X'FF'              value.
         ORG   SYM_TAB+C'+'       symbols, are set to a non-zero
         DC    X'FF'              value.
         ORG   SYM_TAB+C'@'      symbols, are set to a non-zero
         DC    X'FF'              value.         
         ORG   SYM_TAB+C'$'       symbols, are set to a non-zero
         DC    X'FF'              value.
         ORG   SYM_TAB+C'/'      symbols, are set to a non-zero
         DC    X'FF'              value.                   
         ORG
*
         LTORG
         YREGS
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#3)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO        EXEC PGM=AOC23#3,COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD SYSOUT=*
//DATASORT DD SYSOUT=*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  DISP=SHR,DSN=AOC1LV.LEO.INPUT(AOC23I3)
//
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
