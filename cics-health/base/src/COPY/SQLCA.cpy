       01  SQLCA.                                                       00010000
           05  SQLCAID         PIC X(8).                                00020000
           05  SQLCABC         PIC S9(9) COMP.                          00030000
           05  SQLCODE         PIC S9(9) COMP.                          00040000
           05  SQLERRM.                                                 00050000
               49  SQLERRML        PIC S9(4) COMP.                      00060000
               49  SQLERRMC        PIC X(70).                           00070000
           05  SQLERRP         PIC X(8).                                00080000
           05  SQLERRD         OCCURS 6 TIMES PIC S9(9) COMP.           00090000
           05  SQLWARN.                                                 00100000
               10  SQLWARN0    PIC X(1).                                00110000
               10  SQLWARN1    PIC X(1).                                00120000
               10  SQLWARN2    PIC X(1).                                00130000
               10  SQLWARN3    PIC X(1).                                00140000
               10  SQLWARN4    PIC X(1).                                00150000
               10  SQLWARN5    PIC X(1).                                00160000
               10  SQLWARN6    PIC X(1).                                00170000
               10  SQLWARN7    PIC X(1).                                00180000
           05  SQLEXT.                                                  00190000
               10 SQLWARN8     PIC X(1).                                00200000
               10 SQLWARN9     PIC X(1).                                00210000
               10 SQLWARNA     PIC X(1).                                00220000
               10 SQLSTATE     PIC X(5).                                00230000