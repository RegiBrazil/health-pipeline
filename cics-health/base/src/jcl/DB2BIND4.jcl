//HCAZBIND JOB 241901,'BIND FOR CICS 54',CLASS=A,MSGCLASS=H
//*
//*     Bind for CICSTS54
// SET DB2HLQ=DSNB10
//*
//BIND    EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD  DSN=&DB2HLQ..SDSNLOAD,DISP=SHR
//DBRMLIB  DD  DSN=EMPOT.ZMOBILE.TEST.DBRM,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSIN  DD *
/*
//SYSTSIN DD *
DSN SYSTEM(DBBG)
BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCIPDB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCAPDB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCAPDB02)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCITDB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCATDB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCIVDB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCAVDB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCIMDB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCMADB01)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PACKAGE (HCZMSA2)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(HCMADB02)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)

BIND PLAN (HCZPLA2)                                       -
     PKLIST(*.HCZMSA2.*)                                  -
     CURRENTDATA(NO)                                      -
     ISO(CS)                                              -
     ACTION (REP)                                         -
     OWNER(IBMUSER)                                       -
     QUALIFIER(HCZMSA1)                                   -
     REL(DEALLOCATE)                                      -
     ACQUIRE(USE)                                         -
     RETAIN                                               -
     NOREOPT(VARS)                                        -
     VALIDATE(BIND)

RUN PROGRAM(DSNTIAD) PLAN(DSNTIA11) -
     LIB('DSNB10.DBBG.RUNLIB.LOAD')
END
/*