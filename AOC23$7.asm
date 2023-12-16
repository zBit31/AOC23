//AOC1LV7   JOB NOTIFY=&SYSUID
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
* R4 -
* R5 -
* R6 -
* R7 -
* R8 -
* R9 - 
* R10 -
* R11 - SUBROUTINE return address


GET_LOOP EQU *

         GET   FILEIN,RECORD           get record

         MVC   RECOUT,RECORD           put in out for debugging


*         LA    R2,RECORD               start of card scan
*calculate hand type
*    five of a kind   x06
*    four of a kind   x05
*    full house       x04
*    three of a kind  x03
*    two pair         x02
*    one pair         x01
*    high card        x00

         MVI   RECOUT+20,X'00'         start with zero
         SR    R1,R1                   blank R1 to be used to add

         MVC   RECOUT+21(5),RECOUT     move hand to translate area
         TR    RECOUT+21(5),OFFSET_TAB translate and get offset

         MVC   COUNTER(COUNTER_LEN),ZEROCOUNT

         LA    R2,RECOUT+21            start of translated area
         LA    R3,5                    length of translated area
CALC_HAND EQU *
         SR    R1,R1

         ICM   R1,B'0001',0(R2)
         LA    R4,COUNTER   get counter base
         AR    R4,R1        add offset from translated

         ASI   0(R4),+1     add 1 to count

         AHI   R2,+1        next char

         BCT   R3,CALC_HAND

         PUT   FILEOUT,COUNTER


* now we have the counter map populated, search for hand types
*  SRST possible condition codes
*   Condition       Mask
*     Code        Bit Value
*   ---------     ---------
*      0              8     - Not applicable
*      1              4     - Char found, R1 has address of char
*      2              2     - Char not found R1 unchanged
*      3              1     - CPU limit reached - shouldn't happen

CHECK_FIVE EQU *
         LA    R5,COUNT2              start of scan
         LA    R6,COUNTER+COUNTER_LEN  end of scan

         LA    R0,5     search for a five of a kind
         SRST  R6,R5

         BC    8+2+1,CHECK_FOUR    Not found, continue
*        LA    R2,1(R6)            Save start of scan, next char on R2

         ICM   R1,B'0001',RECOUT+20 get current
         LA    R1,6(0,R1)           add 6 for 5 of a kind
         STCM  R1,B'0001',RECOUT+20 put back
*        J     TYPE_DONE             done, get bid

CHECK_FOUR EQU *
         LA    R5,COUNT2              start of scan
         LA    R6,COUNTER+COUNTER_LEN  end of scan

         LA    R0,4     search for a five of a kind
         SRST  R6,R5

         BC    8+2+1,CHECK_THREE   Not found, continue
*        LA    R2,1(R6)            Save start of scan, next char on R2
         ICM   R1,B'0001',RECOUT+20 get current
         LA    R1,5(0,R1)           add 5 for 4 of a kind
         STCM  R1,B'0001',RECOUT+20 put back

CHECK_THREE EQU *
         LA    R5,COUNT2              start of scan
         LA    R6,COUNTER+COUNTER_LEN  end of scan

         LA    R0,3     search for a three of a kind
         SRST  R6,R5

         BC    8+2+1,CHECK_TWO     Not found, continue
*        LA    R2,1(R6)            Save start of scan, next char on R2
         ICM   R1,B'0001',RECOUT+20 get current
         LA    R1,3(0,R1)           add 3 for 3 of a kind
         STCM  R1,B'0001',RECOUT+20 put back

CHECK_TWO EQU *
         LA    R5,COUNT2              start of scan
TWO_RETRY EQU *
         LA    R6,COUNTER+COUNTER_LEN  end of scan

         LA    R0,2     search for a pair
         SRST  R6,R5

         BC    8+2+1,TYPE_DONE       Not found, continue

         ICM   R1,B'0001',RECOUT+20 get current
         LA    R1,1(0,R1)           add 1 for pair
         STCM  R1,B'0001',RECOUT+20 put back

         LA    R5,1(0,R6)            Save start of scan, next char
         J     TWO_RETRY             We might have more then one pair

TYPE_DONE EQU *


* joker will convert hand type
*    five of a kind   x06
*    four of a kind   x05 - becomes x06
*    full house       x04 
*    three of a kind  x03 - becomes x05 (increased by 2)
*    two pair         x02 - becomes x03 (increased by 2)
*    one pair         x01 - becomes x03 (increased by 2)
*    high card        x00 - becomes x01


         L     R2,COUNTJ               load J count into R2
         CFI   R2,5
         BNE   JOKER_LOOP              not 5, go loop
         MVI   RECOUT+20,X'06'         5, set to x06
         J     DONE_JOKER

JOKER_LOOP EQU *
         LTR   R2,R2 
         BZ    DONE_JOKER              none, done
         CLI   RECOUT+20,X'05'         is it a 4 of a kind?
         JE    JOKER_ADD_1                      yes, add 1
         CLI   RECOUT+20,X'00'         is it a high card?
         JE    JOKER_ADD_1                      yes, add 1
* other cases we add 2
JOKER_ADD_2 EQU *
         ICM   R1,B'0001',RECOUT+20    get current
         LA    R1,1(0,R1)              add 1 
         STCM  R1,B'0001',RECOUT+20    put back         

JOKER_ADD_1 EQU *
         ICM   R1,B'0001',RECOUT+20    get current
         LA    R1,1(0,R1)              add 1 
         STCM  R1,B'0001',RECOUT+20    put back   

         BCT   R2,JOKER_LOOP           loop until zero jokers
  
DONE_JOKER EQU *


GET_BID EQU *
* get bid
         LA    R2,RECORD+6
         LA    R3,8                    max number length
         LA    R1,NUM_TAB              number tab
         TRTE  R2,R0,B'0000'           get next number
         BZ    DONE                    no number? done!

         BAS   R11,READ_EBCDIC_NUM_ON_R2  Go get

         LR    R10,R1                   bid into R10

         CVD   R10,CPACKED              put into packed for display
         UNPK  RECOUT+40(10),CPACKED+1
         MVC   RECOUT+40+8(2),=C'  ' Get rid of the "C" for p.dec

* Store bid at end of item for value
         STH   R10,RECOUT+20+6         place at the end of item

         PUT   FILEOUT,RECOUT

* place item into sorted table
* R4 will have the offset at sorted table
* R5 - 64 bits will have the temporary sort item
         LA    R4,SORTAREA
         LG    R5,RECOUT+20   load item into 64 bit R5 - temp item

SORT_LOOP EQU *
         LTG   R1,0(0,R4)     load current item into R1 and test
         BZ    END_SORT_LOOP  end of table
         CGR   R5,R1          compare temp item with current
         BH    NEXT_ITEM      temp is higher, keep looking
         STG   R5,0(0,R4)     store temp item
         LGR   R5,R1          old item is now temp
NEXT_ITEM EQU *
         AHI   R4,8           advance R4 to next item
         J     SORT_LOOP      process next item

END_SORT_LOOP EQU *
         STG   R5,0(0,R4)              store item at end of table

         PUT   FILEOUT,SORTAREA        put for debugging

         J     GET_LOOP


DONE EQU *
FILE#EOD EQU *

* now we are done, sortarea should have what we need, loop there
         LA    R4,SORTAREA
         LA    R5,1    * R5 will have the rank (counter)
         SGR   R6,R6   * R6 will have the total winnings
COUNT_LOOP EQU *
         LGH   R1,6(0,R4)     load current item's bid in R1
         LTR   R1,R1          test R1
         BZ    COUNT_DONE     zero, we are done
         MSGR  R1,R5          multiply bid with rank
         AGR   R6,R1          add to total R6
         AHI   R5,+1          add 1 to rank
         AHI   R4,+8          advance R4 to next item


         J     COUNT_LOOP

COUNT_DONE EQU *
         CVD   R6,CPACKED              put into packed for display
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

* maybe overkill but one fullword per card label
*    (A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2)
COUNTER  DC    F'0'
COUNTJ   DC    F'0'
COUNT2   DC    F'0'
COUNT3   DC    F'0'
COUNT4   DC    F'0'
COUNT5   DC    F'0'
COUNT6   DC    F'0'
COUNT7   DC    F'0'
COUNT8   DC    F'0'
COUNT9   DC    F'0'
COUNTT   DC    F'0'
COUNTQ   DC    F'0'
COUNTK   DC    F'0'
COUNTA   DC    F'0'
COUNTER_LEN EQU *-COUNTER

ZEROCOUNT DC XL(COUNTER_LEN)'00'

O_COUNT2   EQU    COUNT2-COUNTER
O_COUNT3   EQU    COUNT3-COUNTER
O_COUNT4   EQU    COUNT4-COUNTER
O_COUNT5   EQU    COUNT5-COUNTER
O_COUNT6   EQU    COUNT6-COUNTER
O_COUNT7   EQU    COUNT7-COUNTER
O_COUNT8   EQU    COUNT8-COUNTER
O_COUNT9   EQU    COUNT9-COUNTER
O_COUNTT   EQU    COUNTT-COUNTER
O_COUNTJ   EQU    COUNTJ-COUNTER
O_COUNTQ   EQU    COUNTQ-COUNTER
O_COUNTK   EQU    COUNTK-COUNTER
O_COUNTA   EQU    COUNTA-COUNTER

* translate offset table
OFFSET_TAB  DC    256X'00'         All of the characters, except for
         ORG   OFFSET_TAB+C'A'
         DC    AL1(O_COUNTA)
         ORG   OFFSET_TAB+C'K'
         DC    AL1(O_COUNTK)
         ORG   OFFSET_TAB+C'Q'
         DC    AL1(O_COUNTQ)
         ORG   OFFSET_TAB+C'J'
         DC    AL1(O_COUNTJ)
         ORG   OFFSET_TAB+C'T'
         DC    AL1(O_COUNTT)
         ORG   OFFSET_TAB+C'9'
         DC    AL1(O_COUNT9)
         ORG   OFFSET_TAB+C'8'
         DC    AL1(O_COUNT8)
         ORG   OFFSET_TAB+C'7'
         DC    AL1(O_COUNT7)
         ORG   OFFSET_TAB+C'6'
         DC    AL1(O_COUNT6)
         ORG   OFFSET_TAB+C'5'
         DC    AL1(O_COUNT5)
         ORG   OFFSET_TAB+C'4'
         DC    AL1(O_COUNT4)
         ORG   OFFSET_TAB+C'3'
         DC    AL1(O_COUNT3)
         ORG   OFFSET_TAB+C'2'
         DC    AL1(O_COUNT2)
         ORG


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
SORTAREA DC    1000XL8'00'             1000 sort items
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#7)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//GO       EXEC PGM=AOC23#7,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD  SYSOUT=*
//FILEOUT  DD  SYSOUT=B
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  *
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
