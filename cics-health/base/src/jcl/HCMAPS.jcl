//HCMAPS JOB ,'MAPGEN',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
//*
//*
//*-------------------------------------------------------------------*
//*  COPY MAP TO A TEMPORARY DATASET                                  *
//*-------------------------------------------------------------------*
//COPY     EXEC PGM=IEBGENER
//*
//SYSPRINT DD DUMMY
//SYSIN    DD DUMMY
//SYSUT1   DD DISP=SHR,DSN=IBMUSER.GIT.ZMOBILE.BMS(HCMAPS)
//SYSUT2   DD DISP=(,PASS),DSN=&&TEMPM,
//            DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB),
//            UNIT=SYSDA,SPACE=(CYL,(1,1))
//*-------------------------------------------------------------------*
//*  INVOKE THE IBM MVS HIGH LEVEL ASSEMBLER                          *
//*  TO CREATE THE EXECUTABLE OBJECT MODULE OF THE MAP (SYSPARM(MAP)) *
//*-------------------------------------------------------------------*
//ASMMAP   EXEC PGM=ASMA90,
// PARM='SYSPARM(MAP),DECK,NOOBJECT'
//SYSPRINT DD SYSOUT=*
//SYSLIB   DD DISP=SHR,DSN=DFH530.CICS.SDFHMAC
//         DD DISP=SHR,DSN=IBMUSER.GIT.ZMOBILE.BMS
//         DD DISP=SHR,DSN=SYS1.MODGEN
//         DD DISP=SHR,DSN=SYS1.MACLIB
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSPUNCH DD DISP=(,PASS),DSN=&&MAP,
//            UNIT=SYSDA,DCB=BLKSIZE=3120,
//            SPACE=(CYL,(1,1))
//SYSIN    DD DISP=(OLD,PASS),DSN=&&TEMPM
//*-------------------------------------------------------------------*
//*  INVOKE THE MVS LINKAGE-EDITOR PROGRAM                            *
//*-------------------------------------------------------------------*
//LINKMAP  EXEC PGM=IEWL,COND=(7,LT,ASMMAP),
// PARM='LIST,LET,XREF'
//*
//SYSPRINT DD SYSOUT=*
//SYSLMOD  DD DISP=SHR,DSN=IBMUSER.GIT.ZMOBILE.LOAD(HCMAPS)
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,
//            SPACE=(CYL,(1,1))
//SYSLIN   DD DISP=(OLD,DELETE),DSN=&&MAP
//*-------------------------------------------------------------------*
//*  INVOKE THE IBM MVS HIGH LEVEL ASSEMBLER                          *
//*  TO CREATE THE ASSEMBLER DSECT OF THE MAP (SYSPARM(DSECT))        *
//*  REQUIRED, AT ASSEMBLY TIME, BY PROGRAMS THAT USE THE MAP         *
//*-------------------------------------------------------------------*
//ASMDSECT EXEC PGM=ASMA90,
// PARM='SYSPARM(DSECT),DECK,NOOBJECT'
//*
//SYSPRINT DD SYSOUT=*
//SYSLIB   DD DISP=SHR,DSN=DFH530.CICS.SDFHMAC
//         DD DISP=SHR,DSN=IBMUSER.GIT.ZMOBILE.BMS
//         DD DISP=SHR,DSN=SYS1.MODGEN
//         DD DISP=SHR,DSN=SYS1.MACLIB
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSPUNCH DD DISP=SHR,DSN=IBMUSER.GIT.ZMOBILE.COPYBOOK(HCMAPS)
//SYSIN    DD DISP=(OLD,DELETE),DSN=&&TEMPM
//