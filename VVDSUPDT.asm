//ANDREWJC JOB  CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEP1    EXEC ASMACLG,PARM.L='MAP,LET,LIST,AC=1',
// PARM.G='CATALOG.Z12MSTA,CATALOG.Z22MSTA'
//SYSIN    DD   *
         PRINT NOGEN
*-----------------------------------------------------------------*
* This program updates VVDS to replace a catalog name with a new
* catalog name in a VVDS
* Specify the old catalog name + ',' + the new catalog name as
* To really update the vvds, add ',Y' to the end of new catalog on
* the parameter to this program
* Andrew Jan 20191127
*-----------------------------------------------------------------*
* Parm :  The new Catalog Name to replace the old one
*-----------------------------------------------------------------*
*
         PRINT OFF
         LCLA  &REG
.LOOP    ANOP  ,                           generate regs.
R&REG    EQU   &REG
&REG     SETA  &REG+1
         AIF   (&REG LE 15).LOOP
         PRINT ON
*
*------------------------------------------------*
*
VVDSUPDT CSECT
         USING *,R15
         STM   R14,R12,12(R13)      use r13 as base as well as
         LR    R2,R13               reg-save area
         B     CMNTTAIL             reg-save area
*
CMNTHEAD EQU   *
         PRINT GEN
         DC    CL8'&SYSDATE'
         DC    C' '
         DC    CL5'&SYSTIME'
         DC    C'ANDREW JAN'
         CNOP  2,4
         PRINT NOGEN
CMNTTAIL EQU   *
*
         BALR  R12,0
         BAL   R13,76(R12)
SAVREG   DS    18F
         DROP  R15
         USING SAVREG,R13
         ST    R2,4(R13)
         ST    R13,8(R2)
*
*---MAINSTREAM BELOW-----------------------------------------------*
*
        BAL    R6,READ_PARM               Get parm values
        BAL    R6,OPEN_FILES              open files
*
        B      PROCESS                    go to process
*
FINISH  EQU    *
        BAL    R6,CLOSE_FILES             close files
        B      RETURN                     return to system
*
*---MAINSTREAM ABOVE-----------------------------------------------*
*
*---------------------------------------------------------------*
READ_PARM  EQU   *
         L     R2,0(,R1)           load parm addr.
         LH    R3,0(,R2)           load parm length into R3
         LTR   R3,R3               test if no parm specified ?
         BZ    PARM_ERROR          no, take default parm
         LA    R2,2(,R2)           step over the length field

*------------------------------------------------------------*
*  Scan to see the old and new Catalog  Name specified       *
*------------------------------------------------------------*
         XR    R5,R5               clear for the count
         LA    R4,OLDCAT           locate the oldcat field
         LA    R9,45               set the maximum possible cat len.
PARM_000 EQU   *
         CLI   0(R2),C','          get the old cat name
         BE    PARM_001            all chars processed, branch
         MVC   0(1,R4),0(R2)       copy one char
         LA    R2,1(,R2)           next char
         LA    R4,1(,R4)           next char
         LA    R5,1(,R5)           increase the count
         BCT   R9,PARM_000         loop thru all chars up to 45
         B     PARM_ERROR          found no ',' parm error
PARM_001 EQU   *
         STC   R5,OLDCATL          save the old cat name's len
         LA    R2,1(,R2)           skip the ','
         LA    R4,NEWCAT           locate the oldcat field
         LR    R9,R3               save the total parm length
         SR    R9,R5               deduct those bytes for old cat
         BCTR  R9,0                also the first ','
         XR    R7,R7               clear
PARM_002 EQU   *
         CLI   0(R2),C','          get the old cat name
         BE    PARM_003            all chars processed, branch
         MVC   0(1,R4),0(R2)       copy one char
         LA    R2,1(,R2)           next char
         LA    R4,1(,R4)           next char
         LA    R7,1(,R7)           increase the count
         BCT   R9,PARM_002         loop till end
PARM_003 EQU   *
         STC   R7,NEWCATL          save the new cat name's len
         LR    R8,R7               save the new cat name's len
         AR    R8,R5               add new and old names' size
         LA    R8,1(,R8)           also the ',' between old & new names
         SR    R7,R5               name lengh difference
         STH   R7,LENDIFF          save the difference value
         SR    R3,R8               more length?
         CH    R3,=H'2'            is there more argument?
         BLR   R6                  no more argument, go back
         LA    R2,1(,R2)           skip ','
         CLI   0(R2),C'Y'          want to update vvds?
         BNER  R6                  no, go back
         OI    F_UPDATE,L'F_UPDATE set the flag to execute VVDS update
         BR    R6                  go back to main stream

*------------------------------------------------------------*
*  Default parameters                                        *
*------------------------------------------------------------*
PARM_ERROR     EQU  *
         OI    F_PARMERR,L'F_PARMERR no parms were specified
         BR    R6                    go back to main stream
*---------------------------------------------------------------*
*
*
PROCESS  EQU    *
         GET    RPL=MASRPL                get 1st record

         BAL    R6,RTN_REC_1              process 1st record

         GET    RPL=MASRPL                omit 2nd rec
*
REC_3_LOOP      EQU *
         GET    RPL=MASRPL                get  8 consecutive recs
         BAL    R6,RTN_REC_3              process these recs

         B      REC_3_LOOP                loop thru all recs
*
*--------------------------------------------------------*
*
RTN_REC_1 EQU   *
         PUT    PRINT,OL_REC              print original ######
         LA     R4,OL_REC                 start of the record
         LA     R4,24(,R4)                addr of the cat

REC_1_00 EQU   *
         CLC    0(44,R4),BLANKS
         BER    R6                        go back
         CLC    0(L'OLDCAT,R4),OLDCAT
         BNE    REC_1_01
         MVC    0(L'OLDCAT,R4),NEWCAT
         B      REC_1_02
REC_1_01 EQU   *
         LA     R4,56(,R4)
         B      REC_1_00
REC_1_02 EQU   *
         PUT    PRINT,OL_REC              print updated  ######
         TM     F_UPDATE,L'F_UPDATE       update VVDS?
         BNOR   R6                        no, go back
         PUT    RPL=MASRPL                WRITE BACK #####
         BR     R6                        go back
*
*--------------------------------------------------------*
*
RTN_REC_3 EQU   *
         LA     R4,OL_REC                 start of the record
         LH     R3,0(,R4)                 chk if this is an empty rec
         LTR    R3,R3                     empty line?
         BZR    R6                        go back

         PUT    PRINT,OL_REC              print original ######
         XR     R3,R3                     clear for both pad and len
         LH     R11,=H'4096'              set target's len
         L      R10,NW_REC_A              load address of to-buf
         MVCL   R10,R2                    set all 4096 bytes as x'00'

         L      R2,NW_REC_F               last 2-byte
         MVC    0(2,R2),=X'0FFC'          set init. availabe bytes
         L      R2,NW_REC_E               next 2-byte backwards
         MVC    0(2,R2),=X'0000'          set init. occupied bytes
         S      R2,=F'2'                  step backwards 3 byte
         ST     R2,NW_REC_P               initiate the pointer
         L      R10,NW_REC_A              load address of to-buf

RTN_3_00 EQU   *                          this is a loop
         ST     R10,NW_REC_S              save the segment start
         LH     R3,0(,R4)                 entire record length
         LTR    R3,R3                     if all entries walked thru ?
         BNZ    RTN_3_01                  go on
*
         LA     R2,OL_REC                 orginal buf
         L      R10,NW_REC_A              updated buf
         LH     R11,=H'4096'              set source's len
         LR     R3,R11                    set target's len
         MVCL   R2,R10                    copy updated back to original
         PUT    PRINT,OL_REC              print the updated
         TM     F_UPDATE,L'F_UPDATE       update VVDS?
         BNOR   R6                        no, go back
         PUT    RPL=MASRPL                write back
         BR     R6                        go back
*

RTN_3_01 EQU   *
         ST     R4,HDRADR                 save the set start addr.
         LH     R3,0(,R4)                 header field's length
         STH    R3,RECLENO                save the length

         LA     R5,2+2+1+5(,R4)           locate the 1st count
         LA     R3,2+2+1+5                length so far

         XR     R9,R9                     clear for insert
         IC     R9,0(R5)                  1st entry's length

         LA     R5,1(,R5)                 include this 1-byte count
         AR     R5,R9                     locate the 2nd count
         LA     R3,1(,R3)                 include this 1-byte count
         AR     R3,R9                     include the 1st entry's len

         XR     R9,R9                     clear for insert
         IC     R9,0(R5)                  2st entry's length

         LA     R5,1(,R5)                 include this 1-byte count
         AR     R5,R9                     locate the 3rd count
         ST     R5,CATADR                 save the cat name's addr.
         LA     R3,1(,R3)                 include this 1-byte count
         AR     R3,R9                     include the 2nd entry's len
         STH    R3,HDRLEN                 save the length

         XR     R9,R9                     clear for insert
         IC     R9,0(R5)                  3rd entry's length

         LA     R5,1(,R5)                 include this 1-byte count
         AR     R5,R9                     locate the rest data
         ST     R5,RESTADR                save the rest data's addr.
         LA     R3,1(,R3)                 include this 1-byte count
         AR     R3,R9                     include the 3rd entry's len

         LH     R9,RECLENO                total record length
         SR     R9,R3                     this is the len of rest data
         STH    R9,RESTLEN                save it

         L      R5,CATADR                 address of the cat name
         LA     R5,1(,R5)                 skip the length

*--------------------------------------------------------*
         LR     R2,R4                     address of from-addr
         XR     R3,R3                     clear
         IC     R3,OLDCATL                save the old cat name's len
         BCTR   R3,0                      minus 1 for EX
EXCLC1   CLC    0(0,R5),OLDCAT            the one to replace with?
         EX     R3,EXCLC1                 really does the comparing
         BNE    RTN_3_02                  no, branch
         LH     R3,HDRLEN                 len to copy
         LR     R11,R3                    len to copy
         MVCL   R10,R2                    do the copying
         XR     R3,R3                     clear
         IC     R3,NEWCATL                save the new cat name's len
EXMVC1   MVC    0(0,R10),NEWCATL          new name inclu. 1-byte len
         EX     R3,EXMVC1                 really does the moving
         AR     R10,R3                    new address of blank
         LA     R10,1(,R10)               don't forget the len. byte
         LH     R3,RESTLEN                values after cat name
         LR     R11,R3                    same length
         L      R2,RESTADR                start address
         MVCL   R10,R2                    do the copying
         LH     R3,RECLENO                original rec length
         AH     R3,LENDIFF                new rec length
         STH    R3,RECLENN                save the new rec length
         L      R2,NW_REC_S               start of the to-buf
         STH    R3,0(,R2)                 save back
         B      RTN_3_03                  branch

RTN_3_02 EQU    *
         LH     R3,RECLENO                bytes to copy
         STH    R3,RECLENN                new len = old len
         LR     R11,R3                    bytes to copy
         MVCL   R10,R2                    copy to to-addr

RTN_3_03 EQU    *
         L      R2,NW_REC_E               accumulated occupied bytes
         LH     R3,0(,R2)                 current count
         AH     R3,RECLENN                plus this rec length
         STH    R3,0(,R2)                 save back
         L      R2,NW_REC_F               spare bytes remained
         LH     R3,0(,R2)                 current count
         SH     R3,RECLENN                dedcut the rec length
         SH     R3,=H'3'                  also the 3-byte len at end
         BH     RTN_3_04                  space is enough, go on
*
         MVC   WK_WARN,BLANKS             clear
         MVC   WK_WARN(L'SPC_ERR),SPC_ERR ind. error msg
         PUT   WARN,WK_WARN               print it
         B     FINISH                     stop executing
*
RTN_3_04 EQU    *
         STH    R3,0(,R2)                 save back
         L      R2,NW_REC_P               the current pointer
         LH     R3,RECLENN                this rec's new length
         STH    R3,0(,R2)                 save it
         S      R2,=F'3'                  move 3-byte backwards
         ST     R2,NW_REC_P               save back as new pointer
*--------------------------------------------------------*

RTN_3_05 EQU    *
         LH     R3,RECLENO                length of this entry
         AR     R4,R3                     locate the start of next ent
         B      RTN_3_00                  loop thru all of old buf
*
*--------------------------------------------------------*
*
*
*--------------------------------------------------------*
*
OPEN_FILES EQU  *
         OPEN  (PRINT,OUTPUT,WARN,OUTPUT)         open outputs
*
         TM    F_PARMERR,L'F_PARMERR no parms were specified ?
         BNO   OPEN_VSAM             having parms, go on
*
         MVC   WK_WARN,BLANKS                     clear
         MVC   WK_WARN(L'PARM_ERR),PARM_ERR       ind. error msg
         PUT   WARN,WK_WARN                       print it
         B     FINISH                             stop executing
*
OPEN_VSAM EQU  *
         TM    F_UPDATE,L'F_UPDATE                do the real update ?
         BNO   OPEN_VSAM_01                       no, branch
         MVC   WK_WARN,BLANKS                     clear
         MVC   WK_WARN(L'UPDT_MSG),UPDT_MSG       send update msg
         PUT   WARN,WK_WARN                       print it
*
OPEN_VSAM_01 EQU  *
         OPEN  MASACB                             open vsam
         LTR   R15,R15                            test for good rslt
         BZR   R6                                 good, go on
*                                                 error handling
         MVC   WK_WARN,BLANKS                     clear
         MVC   WK_WARN(L'OPEN_ERR),OPEN_ERR       send error msg
         PUT   WARN,WK_WARN                       print it
         B     FINISH                             stop
*--------------------------------------------------------*
*
*
*--------------------------------------------------------*
CLOSE_FILES EQU  *
         CLOSE (MASACB,,PRINT,,WARN)     close files
         BR    R6                        go back
*--------------------------------------------------------*
*
*
*--------------------------------------------------------*
RETURN   EQU   *
         L     R13,4(R13)
         RETURN (14,12),RC=0              back to caller
*--------------------------------------------------------*
*
*
*--------------------------------------------------------*
RTN_LER  EQU   *
         MVC   WK_WARN,BLANKS             clear
         MVC   WK_WARN(L'LOG_ERR),LOG_ERR logical errors
         PUT   WARN,WK_WARN               print warning msg
         B     FINISH                     back to caller
*
*--------------------------------------------------------*
*
SPC_LER  EQU   *
         MVC   WK_WARN,BLANKS             clear
         MVC   WK_WARN(L'SPC_ERR),SPC_ERR logical errors
         PUT   WARN,WK_WARN               print warning msg
         B     FINISH                     back to caller
*
*--------------------------------------------------------*
*
RTN_SYN  EQU   *
         MVC   WK_WARN,BLANKS             clear
         MVC   WK_WARN(L'PHY_ERR),PHY_ERR physical errors
         PUT   WARN,WK_WARN               print warning msg
         B     FINISH                     back to caller
*
*--------------------------------------------------------*
*
*
*--------------------------------------------------------*
RTN_EOD  EQU   *                          end of file encountered
         B     FINISH                     back to caller
*
*--------------------------------------------------------*
*
*
         LTORG ,                          hear comes literal table
*
*
*------------------------------------------------------------------*
MASACB   ACB   DDNAME=MASDS,AM=VSAM,                                   X
               MACRF=(CNV,OUT),             ci process/update          X
               EXLST=EXITS
*
MASRPL   RPL   ACB=MASACB,AM=VSAM,                                     X
               OPTCD=(CNV,UPD),             ci process/update          X
               AREA=OL_REC,                                            X
               AREALEN=4096
*
EXITS    EXLST LERAD=RTN_LER,               logical errors             X
               SYNAD=RTN_SYN,               physical errors            X
               EODAD=RTN_EOD                end of file
*------------------------------------------------------------------*
*
*
*------------------------------------------------------------------*
PRINT    DCB   DSORG=PS,DDNAME=PRINT,MACRF=PM
WARN     DCB   DSORG=PS,DDNAME=WARN,MACRF=PM
*------------------------------------------------------------------*
*
NW_REC_A DC    A(NW_REC)                 keep the address of NW_REC
NW_REC_E DC    A(NW_REC+X'FFC')          keep the end addr of NW_REC
NW_REC_F DC    A(NW_REC+X'FFE')          keep the end addr of NW_REC
NW_REC_P DS    A                         addr of each set move backward
NW_REC_S DS    A                         start addr of each set
WK_R15   DS    A                         for return code
HDRADR   DS    A                         catalog's start address
CATADR   DS    A                         catalog's start address
RESTADR  DS    A                         addr of 1st byte following cat
RECLENO  DS    H                         len of record in old buf
HDRLEN   DS    H                         len of hearder
RESTLEN  DS    H                         len. of bytes to rellocate
LENDIFF  DS    H                         len. difference btwn old new
RECLENN  DS    H                         len of record in new buf
*
OLDCATL  DC    X'00'                     len of old catalog name
OLDCAT   DS    0CL44                     set the max len
         DC    44C' '                    old catalog name
NEWCATL  DC    X'00'                     len of new catalog name
NEWCAT   DC    0CL44                     set the max len
         DC    44C' '                    new catalog name
*
WK_WARN  DS    0CL80                     work area
         DC    80C' '                    initiate it as blanks
*
BLANKS   DC    80C' '                    blanks
*
UPDT_MSG DC    C'Really updating the catalog name in the VVDS!'
OPEN_ERR DC    C'Failed to open the VVDS.!'
PARM_ERR DC    C'Please provide the New Catalog Name as Parm.!'
LOG_ERR  DC    C'Some logical errors happened !'
PHY_ERR  DC    C'Some physical errors happened !'
SPC_ERR  DC    C'Record Space not enough, Action Aborted !'
*
FLAG     DC   X'00'
         ORG  FLAG
F_PARMERR DS   0XL(B'10000000')
F_UPDATE  DS   0XL(B'01000000')
         ORG
*
OL_REC   DS    CL4096          original (old) data read from vvds
*
NW_REC   DS    CL4096          updated (new) data to write to vvds
*
         END
/*
//L.SYSLMOD  DD  DISP=SHR,DSN=SYS1.SS.LINKLIB(VVDSUPDT)
//G.WARN     DD  SYSOUT=*,LRECL=80
//G.PRINT    DD  SYSOUT=*,LRECL=4096
//G.MASDS    DD   DISP=SHR,DSN=SYS1.VVDS.VZ12LGR,
//       UNIT=SYSDA,VOL=SER=Z12LGR,AMP=AMORG
//
