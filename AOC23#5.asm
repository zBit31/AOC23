//AOC1LV5   JOB NOTIFY=&SYSUID
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
         OPEN  (SEEDIN,(INPUT))
         OPEN  (FILEIN,(INPUT))
         OPEN  (FILEOUT,(OUTPUT))
* Dedicated registers:
* R2 - 
* R3 - n/a
* R4 - 
* R5 - map number 1 - destination
* R6 - map number 2 - source
* R7 - map number 3 - range
* R8 - will have the seed - also used by TRTE scan
* R9 - register pair of R8 - length of scan
* R10 - used by EXPACK
* R11 - SUBROUTINE return address

* outer loop, get seed record
GET_SEED_LINE EQU *

         SGR   R8,R8          Clear R7

         MVI   RECOUT,C' '                  Blank Output
         MVC   RECOUT+1(L'RECOUT-1),RECOUT  Blank Output

GETSEED  GET   SEEDIN,RECORD

         LA    R2,RECORD
         BAS   R11,READ_EBCDIC_NUM_ON_R2  Go get 

         LGR   R8,R1 Get seed in R8

*         CVDG  R8,CPACKED              put R8 into packed for display
         UNPK  RECOUT+0(16),CPACKED+1(16)
         MVC   RECOUT+0+14(2),=C'  ' Get rid of the "C" for p.dec


END EQU *
         PUT   FILEOUT,RECOUT

* inner loop, map convert
GETFILE  GET   FILEIN,RECORD         get new record
         CLI   RECORD,C'0'
         BL    GETFILE               not a number, loop

         MVC   RECOUT+0(16),=CL16' ' blank seed for better visual

* yes, number, go get it
         LA    R2,RECORD
         BAS   R11,READ_EBCDIC_NUM_ON_R2  Go get 

         LGR   R5,R1 Get 1st number on R5 - destination
     
         

* get 2nd number
         LA    R2,1(0,R2)  advance to next char
         BAS   R11,READ_EBCDIC_NUM_ON_R2  Go get 

         LGR   R6,R1 Get 2nd number on R6 - source
      

* get 3rd number
         LA    R2,1(0,R2)  advance to next char
         BAS   R11,READ_EBCDIC_NUM_ON_R2  Go get 
         LGR   R7,R1 Get 3rd number on R7 - range
 


* now we need to do the math
* R5 - destination minus source = difference
* R6 - source
* R7 - range length + source = max range
* R8 - seed 

         SGR   R5,R6                   subtract to get diff
         CVDG  R5,CPACKED              go print diff
         UNPK  RECOUT+20(16),CPACKED+1(16)
         MVC   RECOUT+20+14(2),=C'  ' Get rid of the "C" for p.dec            
         LTGR  R5,R5   test R5
         JNM   DIFF_NOT_NEGATIVE 
         MVI   RECOUT+20+14,C'-' add minus signal 
DIFF_NOT_NEGATIVE EQU *        

         CVDG  R6,CPACKED              go print source
         UNPK  RECOUT+40(16),CPACKED+1(16)
         MVC   RECOUT+40+14(2),=C'  ' Get rid of the "C" for p.dec  

         AGR   R7,R6 add source to get max range 
         CVDG  R7,CPACKED              go print max range
         UNPK  RECOUT+60(16),CPACKED+1(16)
         MVC   RECOUT+60+14(2),=C'  ' Get rid of the "C" for p.dec   

         CGR   R8,R6 compare seed with source
         JL    GETFILE     not applicable get next map record

         CGR   R8,R7 compare seed with max range
         JH    GETFILE     not applicable get next map record

         AR    R8,R5 change the seed by diff

         CVDG  R8,CPACKED              put R8 into packed for display
         UNPK  RECOUT+3(16),CPACKED+1(16)
         MVC   RECOUT+3+14(2),=C'  ' Get rid of the "C" for p.dec

         PUT   FILEOUT,RECOUT

* we converted, now we have to get to next map conversion type
NEXTTYPE GET   FILEIN,RECORD         get new record
         CLI   RECORD,C' '           is it a blank?
         BNE   NEXTTYPE              no, keep looping 


         J GETFILE 

*
*
FILE#EOD EQU   *
* we finished this seed, close and re-open map DD         
         CLOSE FILEIN
         OPEN  (FILEIN,(INPUT))   

* here, R8 will have the location, we should compare and keep lower
         LG    R1,LOWEST_LOCATION 
         CGR   R8,R1
         JNL   GET_SEED_LINE not lowest, ignore seed

         STG   R8,LOWEST_LOCATION

         J     GET_SEED_LINE

SEED#EOD EQU   *

         LG    R1,LOWEST_LOCATION 

         MVC   RECOUT(10),=CL10'Lowest:' 

         CVDG  R1,CPACKED              put R8 into packed for display
         UNPK  RECOUT+10(16),CPACKED+1(16)
         MVC   RECOUT+10+14(2),=C'  ' Get rid of the "C" for p.dec        

         PUT   FILEOUT,RECOUT

         CLOSE SEEDIN
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

         CVBG  R1,CPACKED              get number on R1
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
SEEDIN   DCB   DSORG=PS,                                               X
               MACRF=(GM),                                             X
               DEVD=DA,                                                X
               DDNAME=SEEDS,                                           X
               EODAD=SEED#EOD,                                         X
               RECFM=FB,                                               X
               LRECL=80              
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
CPACKED  DS    XL16   16 bytes for  CVDG/CVBG
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
//SYSLMOD  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD(AOC23#5)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO       EXEC PGM=AOC23#5,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AOC1LV.LEO.LOAD
//SYSPRINT DD  SYSOUT=*
//DATASORT DD  SYSOUT=*
//FILEOUT  DD  SYSOUT=B
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//SEEDS    DD  *
79
14
55
13
//FILEIN   DD  *
seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

/*
//
//FILEIN   DD  DISP=SHR,DSN=AOC1LV.LEO.INPUT(AOC23I5)
//
//
