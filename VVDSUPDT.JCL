//ANDREWJV JOB  IBM,SP,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEP1    EXEC PGM=VVDSUPDT,                                
//   PARM='CATALOG.Z22MSTA,CATALOG.Z33MSTABC'                
//STEPLIB  DD   DISP=SHR,DSN=SYS1.SS.LINKLIB                 
//MASDS    DD   DISP=SHR,DSN=SYS1.VVDS.VZ12LGR,              
//        UNIT=SYSDA,VOL=SER=Z12LGR,AMP=AMORG                
//PRINT    DD   SYSOUT=*,LRECL=4096                          
//WARN     DD   SYSOUT=*,LRECL=80                            
//                                                           