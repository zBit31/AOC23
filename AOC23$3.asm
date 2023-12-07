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
* R2 - Used by subroutine TRTE TRTRE
* R3 - Used by subroutine TRTE TRTRE register pair
* R4 - Position of star (gear)
* R5 - length to search for start (pair of R4)
* R6 - Position to start to check for number
* R7 - Used in TRTE for # of number search (pair of R6)
* R8 - Will have the sum
* R9 - First gear number
* R10 - N/A / work
* R11 - SUBROUTINE return address
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
FINDSTAR EQU *

         SR    R9,R9 clear R9 - first gear value


STARTRTE LA    R1,STAR_TAB
         TRTE  R4,R0,B'0000'            Test for star
         JO    STARTRTE                  Still not at end (CPU lim)
*        SR    R6,R5
*        DC    X'00'
         JZ    GET_LINE                    No stars, next line

         MVC   RECOUT+0(4),0(R4)
*         PUT   FILEOUT,RECOUT

* We found a star, start lookup on top left
         LR    R6,R4

         AHI   R6,(REC_PREV-REC_CUR)-1  Go to previous line and char
         LA    R7,3                     Len to search is 3 on top of *
*         MVC   RECOUT+30(10),0(R6)
RETRY_TOP EQU *
         LA    R1,NUM_TAB               find a number
         TRTE  R6,R0,B'0000'            search for number
*         MVC   RECOUT+40(10),0(R6)

         BZ    TOP_COMPLETE
         BAS   R11,EXPAND_NUM          get the number

         LTR   R7,R7    are we done with top?

         JP    RETRY_TOP   no, retry top



TOP_COMPLETE EQU *
* now look at same line
         LR    R6,R4
         AHI   R6,-1 go back 1 line
         LA    R7,3      len to search is 3, before and after *
RETRY_MID EQU *
         LA    R1,NUM_TAB               find a number
         TRTE  R6,R0,B'0000'            search for number

         BZ    MID_COMPLETE
         BAS   R11,EXPAND_NUM          get the number

         LTR   R7,R7    are we done with mid?

         JP    RETRY_MID   no, retry mid

MID_COMPLETE EQU *

         LR    R6,R4

         AHI   R6,(REC_NEXT-REC_CUR)-1  Go to next line and prev char
         LA    R7,3                     Len to search is 3 on bot of *
*         MVC   RECOUT+30(10),0(R6)
RETRY_BOT EQU *
         LA    R1,NUM_TAB               find a number
         TRTE  R6,R0,B'0000'            search for number
*         MVC   RECOUT+40(10),0(R6)

         BZ    NEXT_CHAR_FOR_NEXT_STAR if we got here, go look next *
         BAS   R11,EXPAND_NUM          get the number

         LTR   R7,R7    are we done with bottom?

         JP    RETRY_BOT   no, retry bottom

         J     NEXT_CHAR_FOR_NEXT_STAR

*         PUT   FILEOUT,RECOUT
*         J     FILE#EOD

* Routine to expand numbers (to left/right) and then place the num
* On R9 if it's empty or calculate gear ratio if not empty
* Entry - R6 is number found
* Will use Register pair R2/R3 for TRTE / TRTRE
* Return to address on R11 after updated R6 with address after number
EXPAND_NUM EQU *
         LA    R1,NOT#_TAB search for first not number to get end of #
         LR    R2,R6  let's work with R2/R3 pair
         LA    R3,8   max num length
         TRTRE R2,R0,B'0000' go back to start of number
         LA    R2,1(0,R2) go to next char to the number
*         MVC   RECOUT+30(9),0(R2)

         LR    R10,R2    R10 to calculate the lenght of the number
         LA    R3,8      max 8 number length
         TRTE  R2,R0,B'0000'     search for next non-num

* prepare next scan
         AR    R7,R6      get end of last scan
         LR    R6,R2      load address of next non-num for next scan
         SR    R7,R6      get new length

         SR    R2,R10     get length on R2

         CVD   R2,CPACKED
*         UNPK  RECOUT+10(9),CPACKED(L'CPACKED+1)
*         MVC   RECOUT+10+7(2),=C'  ' Get rid of the "C" for packed dec

         AHI   R2,-1               remove 1 for EX (0 len is 1)
         EX    R2,EXPACK           EX PACK CPACKED,0(0,R10)

* add to recout for display
         UNPK  RECOUT+20(9),CPACKED(L'CPACKED+1)
         MVC   RECOUT+20+7(2),=C'  ' Get rid of the "C" for packed dec

         LTR   R9,R9
         BNZ   GEAR_RATIO
* first number of the gear, add to R9 and go try to find another num
         CVB   R9,CPACKED get number on R9

         PUT   FILEOUT,RECOUT

         BR    R11  return to called from address


GEAR_RATIO EQU *
* We got two number, let's calculate the gear ratio and add to the sum
         CVB   R0,CPACKED get new number on R0
         MSR   R9,R0           Multiply

         AR    R8,R9      add to total

* add total to display
         CVD   R8,CPACKED
         UNPK  RECOUT+30(12),CPACKED(L'CPACKED+1)
         MVC   RECOUT+30+10(2),=C'  ' Get rid of the "C" for packed dec

         PUT   FILEOUT,RECOUT
         MVC   RECOUT+0(4),0(R4)
         PUT   FILEOUT,RECOUT

* we are done with this gear (asterisk) get to the next
NEXT_CHAR_FOR_NEXT_STAR EQU *
         AHI   R4,+1
         AHI   R5,-1

         MVC   RECOUT+10(10),=CL10'NEXT**'
         PUT   FILEOUT,RECOUT
         MVC   RECOUT+10(10),=CL10' '

         MVC   RECOUT+0(4),0(R4)
         PUT   FILEOUT,RECOUT
         J     FINDSTAR  go get next asterisk



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
EXPACK     PACK  CPACKED,0(0,R10)
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
STAR_TAB DC    256X'00'         All of the characters, except for
         ORG   STAR_TAB+C'*'       symbols, are set to a non-zero
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
//GO       EXEC PGM=AOC23#3,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD  SYSOUT=*
//DATASORT DD  SYSOUT=*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  DISP=SHR,DSN=AOC1LV.LEO.INPUT(AOC23I3)
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
