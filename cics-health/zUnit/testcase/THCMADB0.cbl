       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU)
      *+---------------------------------------------------------------+
      *| THCMADB0                                                      |
      *| PRODUCT: IBM DEVELOPER FOR Z/OS                               |
      *| COMPONENT: IBM Z/OS AUTOMATED UNIT TESTING FRAMEWORK (ZUNIT)  |
      *|   FOR ENTERPRISE COBOL AND PL/I                               |
      *| PROGRAM: ENTERPRISE COBOL ZUNIT TEST CASE FOR DYNAMIC RUNNER  |
      *| DATE GENERATED: 03/11/2021 18:56                              |
      *| ID: de19a73a-28dc-4d52-9664-d2edb21b7aab                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| TEST_TEST2                                                    |
      *|     THIS PROGRAM IS FOR TEST TEST2                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'HCMADB02'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       1 AZ-TEST-EXPECTED-DATA-VALUE.
          3 ZUT00000000.
            5 PIC X(6) DISPLAY VALUE '01AMED'.
          3 ZUT00000001.
            5 PIC X(6) DISPLAY VALUE 'DRUGXX'.
            5 PIC X(4) DISPLAY VALUE SPACES.
          3 ZUT00000002.
            5 PIC X(10) DISPLAY VALUE SPACES.
          3 ZUT00000003.
            5 PIC X(10) DISPLAY VALUE SPACES.
          3 ZUT00000004.
            5 PIC X(10) DISPLAY VALUE SPACES.
          3 ZUT00000005.
            5 PIC X(10) DISPLAY VALUE SPACES.
          3 ZUT00000006.
            5 PIC X(20) DISPLAY VALUE '50MG                '.
            5 PIC X(20) DISPLAY VALUE '  0ORAL             '.
            5 PIC X(20) DISPLAY VALUE '   1                '.
            5 PIC X(20) DISPLAY VALUE '   HCU              '.
            5 PIC X(20) DISPLAY VALUE '   HR2021-01-022021-'.
            5 PIC X(20) DISPLAY VALUE '03-21000100108600196'.
            5 PIC X(20) DISPLAY VALUE '27811234Ralph     DA'.
            5 PIC X(20) DISPLAY VALUE 'lmeida            19'.
            5 PIC X(20) DISPLAY VALUE '80-07-1134 Main Stre'.
            5 PIC X(20) DISPLAY VALUE 'et      Toronto     '.
            5 PIC X(20) DISPLAY VALUE '        M5H 1T1   07'.
            5 PIC X(20) DISPLAY VALUE '7-123-9987        Ra'.
            5 PIC X(20) DISPLAY VALUE 'lphD@ibm.com        '.
            5 PIC X(20) DISPLAY VALUE '                    '.
            5 PIC X(14) DISPLAY VALUE '        ralphd'.
            5 PIC X(32138) DISPLAY VALUE SPACES.
       01 AZ-COMPARE.
         03 AZ-COMPARE-ITEM-NAME-PTR  POINTER.
         03 AZ-COMPARE-ITEM-NAME-LEN  PIC S9(9) COMP-5.
         03 AZ-COMPARE-ITEM-VALUE-PTR POINTER.
         03 AZ-COMPARE-ITEM-VALUE-LEN PIC S9(9) COMP-5.
         03 AZ-COMPARE-ITEM-EXP-VALUE-PTR POINTER.
         03 AZ-COMPARE-ITEM-EXP-VALUE-LEN PIC S9(9) COMP-5.
       LOCAL-STORAGE SECTION.
       1 AZ-COMPARE-ITEM-NAMES.
         3 ZUT00000007.
            5 PIC X(20) DISPLAY VALUE 'CA-REQUEST-ID OF DFH'.
            5 PIC X(8) DISPLAY VALUE 'COMMAREA'.
         3 ZUT00000009.
            5 PIC X(20) DISPLAY VALUE 'CA-RETURN-CODE OF DF'.
            5 PIC X(9) DISPLAY VALUE 'HCOMMAREA'.
         3 ZUT0000000B.
            5 PIC X(20) DISPLAY VALUE 'CA-PATIENT-ID OF DFH'.
            5 PIC X(8) DISPLAY VALUE 'COMMAREA'.
         3 ZUT0000000D.
            5 PIC X(20) DISPLAY VALUE 'CA-VISIT-DATE OF CA-'.
            5 PIC X(20) DISPLAY VALUE 'VISIT-REQUEST OF DFH'.
            5 PIC X(8) DISPLAY VALUE 'COMMAREA'.
         3 ZUT0000000F.
            5 PIC X(20) DISPLAY VALUE 'CA-VISIT-TIME OF CA-'.
            5 PIC X(20) DISPLAY VALUE 'VISIT-REQUEST OF DFH'.
            5 PIC X(8) DISPLAY VALUE 'COMMAREA'.
         3 ZUT00000011.
            5 PIC X(20) DISPLAY VALUE 'CA-HEART-RATE OF CA-'.
            5 PIC X(20) DISPLAY VALUE 'VISIT-REQUEST OF DFH'.
            5 PIC X(8) DISPLAY VALUE 'COMMAREA'.
         3 ZUT00000013.
            5 PIC X(20) DISPLAY VALUE 'CA-BLOOD-PRESSURE OF'.
            5 PIC X(20) DISPLAY VALUE ' CA-VISIT-REQUEST OF'.
            5 PIC X(12) DISPLAY VALUE ' DFHCOMMAREA'.
         3 ZUT00000015.
            5 PIC X(20) DISPLAY VALUE 'CA-MENTAL-STATE OF C'.
            5 PIC X(20) DISPLAY VALUE 'A-VISIT-REQUEST OF D'.
            5 PIC X(10) DISPLAY VALUE 'FHCOMMAREA'.
         3 ZUT00000017.
            5 PIC X(20) DISPLAY VALUE 'CA-ADDITIONAL-DATA O'.
            5 PIC X(20) DISPLAY VALUE 'F CA-VISIT-REQUEST O'.
            5 PIC X(13) DISPLAY VALUE 'F DFHCOMMAREA'.
       1 AZ-COMPARE-WORK-ITEMS.
          3 ZUT00000008 PIC X(6) OCCURS 2.
          3 ZUT0000000A PIC 9(2) OCCURS 2.
          3 ZUT0000000C PIC 9(10) OCCURS 2.
          3 ZUT0000000E PIC X(10) OCCURS 2.
          3 ZUT00000010 PIC X(10) OCCURS 2.
          3 ZUT00000012 PIC X(10) OCCURS 2.
          3 ZUT00000014 PIC X(10) OCCURS 2.
          3 ZUT00000016 PIC X(10) OCCURS 2.
          3 ZUT00000018 PIC X(32432) OCCURS 2.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-COMPARE-ITEM-NAME      PIC X(254).
       01 AZ-COMPARE-ITEM-VALUE     PIC X(254).
       01 AZ-COMPARE-ITEM-EXP-VALUE PIC X(254).
       1 DFHEIBLK.
         2 EIBTIME PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBDATE PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRNID PICTURE X(4).
         2 EIBTASKN PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRMID PICTURE X(4).
         2 DFHEIGDI PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCPOSN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCALEN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBAID PICTURE X(1).
         2 EIBFN PICTURE X(2).
         2 EIBRCODE PICTURE X(6).
         2 EIBDS PICTURE X(8).
         2 EIBREQID PICTURE X(8).
         2 EIBRSRCE PICTURE X(8).
         2 EIBSYNC PICTURE X.
         2 EIBFREE PICTURE X.
         2 EIBRECV PICTURE X.
         2 EIBSEND PICTURE X.
         2 EIBATT PICTURE X.
         2 EIBEOC PICTURE X.
         2 EIBFMH PICTURE X.
         2 EIBCOMPL PICTURE X(1).
         2 EIBSIG PICTURE X(1).
         2 EIBCONF PICTURE X(1).
         2 EIBERR PICTURE X(1).
         2 EIBERRCD PICTURE X(4).
         2 EIBSYNRB PICTURE X.
         2 EIBNODAT PICTURE X.
         2 EIBRESP PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRESP2 PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRLDBK PICTURE X(1).
       1 DFHCOMMAREA.
         3 CA-REQUEST-ID PIC X(6).
         3 CA-RETURN-CODE PIC 9(2).
         3 CA-PATIENT-ID PIC 9(10).
         3 CA-REQUEST-SPECIFIC PIC X(32482).
         3 CA-PATIENT-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-INS-CARD-NUM PIC X(10).
         5 CA-FIRST-NAME PIC X(10).
         5 CA-LAST-NAME PIC X(20).
         5 CA-DOB PIC X(10).
         5 CA-ADDRESS PIC X(20).
         5 CA-CITY PIC X(20).
         5 CA-POSTCODE PIC X(10).
         5 CA-PHONE-MOBILE PIC X(20).
         5 CA-EMAIL-ADDRESS PIC X(50).
         5 CA-USERID PIC X(10).
         5 CA-ADDITIONAL-DATA PIC X(32302).
         3 CA-PATIENT-USER-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-USERNAME PIC X(10).
         5 CA-USERPASSWORD PIC X(14).
         5 CA-ADDITIONAL-DATA PIC X(32458).
         3 CA-MEDICATION-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-DRUG-NAME PIC X(50).
         5 CA-STRENGTH PIC X(20).
         5 CA-AMOUNT PIC 9(03).
         5 CA-ROUTE PIC X(20).
         5 CA-FREQUENCY PIC X(20).
         5 CA-IDENTIFIER PIC X(20).
         5 CA-BIOMED-TYPE PIC X(2).
         5 CA-START-DATE PIC X(10).
         5 CA-END-DATE PIC X(10).
         5 CA-PRESCRIPTION-ID PIC 9(10).
         5 CA-ADDITIONAL-DATA PIC X(32317).
         3 CA-MEDITATION-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-MEDITATION-NAME PIC X(50).
         5 CA-MEDITATION-TYPE PIC X(20).
         5 CA-RELIEF PIC X(20).
         5 CA-POSTURE PIC X(20).
         5 CA-HOW-OFTEN PIC X(20).
         5 CA-ADDITIONAL-DATA PIC X(32352).
         3 CA-THRESHOLD-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-HR-THRESHOLD PIC X(10).
         5 CA-BP-THRESHOLD PIC X(10).
         5 CA-MS-THRESHOLD PIC X(10).
         5 CA-ADDITIONAL-DATA PIC X(32452).
         3 CA-VISIT-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-VISIT-DATE PIC X(10).
         5 CA-VISIT-TIME PIC X(10).
         5 CA-HEART-RATE PIC X(10).
         5 CA-BLOOD-PRESSURE PIC X(10).
         5 CA-MENTAL-STATE PIC X(10).
         5 CA-ADDITIONAL-DATA PIC X(32432).
       PROCEDURE DIVISION USING AZ-TEST
           DFHEIBLK DFHCOMMAREA.
      * START
           DISPLAY 'TEST_TEST2 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
           PERFORM INITIALIZE-PARM
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'CALL HCMADB02'
           CALL PROGRAM-NAME
           USING DFHEIBLK DFHCOMMAREA
           .
      * EVALUATE OUTPUT VALUE
           MOVE 0 TO RETURN-CODE
           IF CA-REQUEST-ID OF DFHCOMMAREA = ZUT00000000 THEN
             CONTINUE
           ELSE
             MOVE CA-REQUEST-ID OF DFHCOMMAREA TO ZUT00000008(1)
             MOVE ZUT00000000 TO ZUT00000008(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000007
             MOVE LENGTH OF ZUT00000007 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000008(1)
             MOVE 6 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000008(2)
             MOVE 6 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF (CA-RETURN-CODE OF DFHCOMMAREA IS NUMERIC)
               AND (CA-RETURN-CODE OF DFHCOMMAREA = 0) THEN
             CONTINUE
           ELSE
             MOVE CA-RETURN-CODE OF DFHCOMMAREA TO ZUT0000000A(1)
             MOVE 0 TO ZUT0000000A(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000009
             MOVE LENGTH OF ZUT00000009 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT0000000A(1)
             MOVE 2 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT0000000A(2)
             MOVE 2 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF (CA-PATIENT-ID OF DFHCOMMAREA IS NUMERIC)
               AND (CA-PATIENT-ID OF DFHCOMMAREA = 1) THEN
             CONTINUE
           ELSE
             MOVE CA-PATIENT-ID OF DFHCOMMAREA TO ZUT0000000C(1)
             MOVE 1 TO ZUT0000000C(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT0000000B
             MOVE LENGTH OF ZUT0000000B TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT0000000C(1)
             MOVE 10 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT0000000C(2)
             MOVE 10 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF CA-VISIT-DATE OF CA-VISIT-REQUEST OF DFHCOMMAREA =
           ZUT00000001 THEN
             CONTINUE
           ELSE
             MOVE CA-VISIT-DATE OF CA-VISIT-REQUEST OF DFHCOMMAREA TO
           ZUT0000000E(1)
             MOVE ZUT00000001 TO ZUT0000000E(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT0000000D
             MOVE LENGTH OF ZUT0000000D TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT0000000E(1)
             MOVE 10 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT0000000E(2)
             MOVE 10 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF CA-VISIT-TIME OF CA-VISIT-REQUEST OF DFHCOMMAREA =
           ZUT00000002 THEN
             CONTINUE
           ELSE
             MOVE CA-VISIT-TIME OF CA-VISIT-REQUEST OF DFHCOMMAREA TO
           ZUT00000010(1)
             MOVE ZUT00000002 TO ZUT00000010(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT0000000F
             MOVE LENGTH OF ZUT0000000F TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000010(1)
             MOVE 10 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000010(2)
             MOVE 10 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF CA-HEART-RATE OF CA-VISIT-REQUEST OF DFHCOMMAREA =
           ZUT00000003 THEN
             CONTINUE
           ELSE
             MOVE CA-HEART-RATE OF CA-VISIT-REQUEST OF DFHCOMMAREA TO
           ZUT00000012(1)
             MOVE ZUT00000003 TO ZUT00000012(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000011
             MOVE LENGTH OF ZUT00000011 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000012(1)
             MOVE 10 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000012(2)
             MOVE 10 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF CA-BLOOD-PRESSURE OF CA-VISIT-REQUEST OF DFHCOMMAREA =
           ZUT00000004 THEN
             CONTINUE
           ELSE
             MOVE CA-BLOOD-PRESSURE OF CA-VISIT-REQUEST OF DFHCOMMAREA
           TO ZUT00000014(1)
             MOVE ZUT00000004 TO ZUT00000014(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000013
             MOVE LENGTH OF ZUT00000013 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000014(1)
             MOVE 10 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000014(2)
             MOVE 10 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF CA-MENTAL-STATE OF CA-VISIT-REQUEST OF DFHCOMMAREA =
           ZUT00000005 THEN
             CONTINUE
           ELSE
             MOVE CA-MENTAL-STATE OF CA-VISIT-REQUEST OF DFHCOMMAREA
           TO ZUT00000016(1)
             MOVE ZUT00000005 TO ZUT00000016(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000015
             MOVE LENGTH OF ZUT00000015 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000016(1)
             MOVE 10 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000016(2)
             MOVE 10 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF CA-ADDITIONAL-DATA OF CA-VISIT-REQUEST OF DFHCOMMAREA =
           ZUT00000006 THEN
             CONTINUE
           ELSE
             MOVE CA-ADDITIONAL-DATA OF CA-VISIT-REQUEST OF
           DFHCOMMAREA TO ZUT00000018(1)
             MOVE ZUT00000006 TO ZUT00000018(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000017
             MOVE LENGTH OF ZUT00000017 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000018(1)
             MOVE 254 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000018(2)
             MOVE 254 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             PERFORM THROW-ASSERTION
           END-IF
      * END
           DISPLAY 'TEST_TEST2 SUCCESSFUL.'
           GOBACK.
       INITIALIZE-PARM.
           EXIT.
       THROW-ASSERTION.
           MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
           STRING 'COMPARE FAILED IN PROCEDURE DIVISION.'
             DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
             WITH POINTER MESSAGE-LEN OF BZ-ASSERT
           END-STRING
           SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
           SET ADDRESS OF AZ-COMPARE-ITEM-NAME TO
           AZ-COMPARE-ITEM-NAME-PTR.
           SET ADDRESS OF AZ-COMPARE-ITEM-VALUE TO
           AZ-COMPARE-ITEM-VALUE-PTR.
           SET ADDRESS OF AZ-COMPARE-ITEM-EXP-VALUE TO
           AZ-COMPARE-ITEM-EXP-VALUE-PTR.
           DISPLAY '****************************************************
      -    '****************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY ' DATA ITEM NAME : '
           AZ-COMPARE-ITEM-NAME(1:AZ-COMPARE-ITEM-NAME-LEN)
           DISPLAY '  VALUE         : '
           AZ-COMPARE-ITEM-VALUE(1:AZ-COMPARE-ITEM-VALUE-LEN)
           DISPLAY '  EXPECTED VALUE: '
           AZ-COMPARE-ITEM-EXP-VALUE(1:AZ-COMPARE-ITEM-EXP-VALUE-LEN)
           DISPLAY '****************************************************
      -    '****************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           MOVE 1 TO TRACE-LEN OF BZ-TRACE
           STRING 'ITEM NAME='
           AZ-COMPARE-ITEM-NAME(1:AZ-COMPARE-ITEM-NAME-LEN)
               DELIMITED BY SIZE INTO TRACE-TXT OF BZ-TRACE
               WITH POINTER TRACE-LEN OF BZ-TRACE
             END-STRING
           SUBTRACT 1 FROM TRACE-LEN OF BZ-TRACE
           SET AZ-TRACE-PTR TO ADDRESS OF TRACE-TXT OF BZ-TRACE
           CALL BZUTRACE USING BZ-TRACE
           MOVE 1 TO TRACE-LEN OF BZ-TRACE
           STRING 'VALUE='
           AZ-COMPARE-ITEM-VALUE(1:AZ-COMPARE-ITEM-VALUE-LEN)
               DELIMITED BY SIZE INTO TRACE-TXT OF BZ-TRACE
               WITH POINTER TRACE-LEN OF BZ-TRACE
             END-STRING
           SUBTRACT 1 FROM TRACE-LEN OF BZ-TRACE
           CALL BZUTRACE USING BZ-TRACE
           MOVE 1 TO TRACE-LEN OF BZ-TRACE
           STRING 'EXPECTED VALUE='
           AZ-COMPARE-ITEM-EXP-VALUE(1:AZ-COMPARE-ITEM-EXP-VALUE-LEN)
               DELIMITED BY SIZE INTO TRACE-TXT OF BZ-TRACE
               WITH POINTER TRACE-LEN OF BZ-TRACE
             END-STRING
           SUBTRACT 1 FROM TRACE-LEN OF BZ-TRACE
           CALL BZUTRACE USING BZ-TRACE
           EXIT.
       END PROGRAM TEST_TEST2.
      *+---------------------------------------------------------------+
      *| BZU_TEST                                                      |
      *|     THIS PROGRAM IS CALLBACK DEFINITION FOR TEST              |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'HCMADB02'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       1 DFHEIBLK.
         2 EIBTIME PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBDATE PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRNID PICTURE X(4).
         2 EIBTASKN PICTURE S9(7) USAGE COMPUTATIONAL-3.
         2 EIBTRMID PICTURE X(4).
         2 DFHEIGDI PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCPOSN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBCALEN PICTURE S9(4) USAGE COMPUTATIONAL-5.
         2 EIBAID PICTURE X(1).
         2 EIBFN PICTURE X(2).
         2 EIBRCODE PICTURE X(6).
         2 EIBDS PICTURE X(8).
         2 EIBREQID PICTURE X(8).
         2 EIBRSRCE PICTURE X(8).
         2 EIBSYNC PICTURE X.
         2 EIBFREE PICTURE X.
         2 EIBRECV PICTURE X.
         2 EIBSEND PICTURE X.
         2 EIBATT PICTURE X.
         2 EIBEOC PICTURE X.
         2 EIBFMH PICTURE X.
         2 EIBCOMPL PICTURE X(1).
         2 EIBSIG PICTURE X(1).
         2 EIBCONF PICTURE X(1).
         2 EIBERR PICTURE X(1).
         2 EIBERRCD PICTURE X(4).
         2 EIBSYNRB PICTURE X.
         2 EIBNODAT PICTURE X.
         2 EIBRESP PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRESP2 PICTURE S9(8) USAGE COMPUTATIONAL.
         2 EIBRLDBK PICTURE X(1).
       1 DFHCOMMAREA.
         3 CA-REQUEST-ID PIC X(6).
         3 CA-RETURN-CODE PIC 9(2).
         3 CA-PATIENT-ID PIC 9(10).
         3 CA-REQUEST-SPECIFIC PIC X(32482).
         3 CA-PATIENT-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-INS-CARD-NUM PIC X(10).
         5 CA-FIRST-NAME PIC X(10).
         5 CA-LAST-NAME PIC X(20).
         5 CA-DOB PIC X(10).
         5 CA-ADDRESS PIC X(20).
         5 CA-CITY PIC X(20).
         5 CA-POSTCODE PIC X(10).
         5 CA-PHONE-MOBILE PIC X(20).
         5 CA-EMAIL-ADDRESS PIC X(50).
         5 CA-USERID PIC X(10).
         5 CA-ADDITIONAL-DATA PIC X(32302).
         3 CA-PATIENT-USER-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-USERNAME PIC X(10).
         5 CA-USERPASSWORD PIC X(14).
         5 CA-ADDITIONAL-DATA PIC X(32458).
         3 CA-MEDICATION-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-DRUG-NAME PIC X(50).
         5 CA-STRENGTH PIC X(20).
         5 CA-AMOUNT PIC 9(03).
         5 CA-ROUTE PIC X(20).
         5 CA-FREQUENCY PIC X(20).
         5 CA-IDENTIFIER PIC X(20).
         5 CA-BIOMED-TYPE PIC X(2).
         5 CA-START-DATE PIC X(10).
         5 CA-END-DATE PIC X(10).
         5 CA-PRESCRIPTION-ID PIC 9(10).
         5 CA-ADDITIONAL-DATA PIC X(32317).
         3 CA-MEDITATION-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-MEDITATION-NAME PIC X(50).
         5 CA-MEDITATION-TYPE PIC X(20).
         5 CA-RELIEF PIC X(20).
         5 CA-POSTURE PIC X(20).
         5 CA-HOW-OFTEN PIC X(20).
         5 CA-ADDITIONAL-DATA PIC X(32352).
         3 CA-THRESHOLD-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-HR-THRESHOLD PIC X(10).
         5 CA-BP-THRESHOLD PIC X(10).
         5 CA-MS-THRESHOLD PIC X(10).
         5 CA-ADDITIONAL-DATA PIC X(32452).
         3 CA-VISIT-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
         5 CA-VISIT-DATE PIC X(10).
         5 CA-VISIT-TIME PIC X(10).
         5 CA-HEART-RATE PIC X(10).
         5 CA-BLOOD-PRESSURE PIC X(10).
         5 CA-MENTAL-STATE PIC X(10).
         5 CA-ADDITIONAL-DATA PIC X(32432).
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_HCMADB02" USING AZ-TEST AZ-INFO-BLOCK
           DFHEIBLK DFHCOMMAREA.
           DISPLAY 'PGM_INPT_HCMADB02 CHECK VALUES...'.
           MOVE 0 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * EVALUATE OUTPUT VALUE
           ENTRY "PGM_OUTP_HCMADB02" USING AZ-TEST AZ-INFO-BLOCK
           DFHEIBLK DFHCOMMAREA.
           DISPLAY 'PGM_OUTP_HCMADB02 INPUT VALUES...'.
           MOVE 4 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST2'
             MOVE 0 TO RETURN-CODE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'BZU_TEST SUCCESSFUL.'
           GOBACK.
       END PROGRAM BZU_TEST.
      *+---------------------------------------------------------------+
      *| BZU_INIT                                                      |
      *|     INITIAL PROCEDURE                                         |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_INIT'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       01 AZ-TESTCASE-ID        PIC X(36)
           VALUE 'de19a73a-28dc-4d52-9664-d2edb21b7aab'.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-TEST-ID            PIC X(80).
       PROCEDURE DIVISION USING AZ-TEST AZ-TEST-ID.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'BZU_INIT : ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           MOVE AZ-TESTCASE-ID TO AZ-TEST-ID
           GOBACK.
       END PROGRAM BZU_INIT.
      *+---------------------------------------------------------------+
      *| BZU_TERM                                                      |
      *|     TERMINATION PROCEDURE                                     |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TERM'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       PROCEDURE DIVISION USING AZ-TEST.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'BZU_TERM : ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           GOBACK.
       END PROGRAM BZU_TERM.
      *+---------------------------------------------------------------+
      *| EVALOPT                                                       |
      *|   FUNCTION TO EVALUATE THAT THE BIT OF OPTION DATA            |
      *|   (1) TAKE AND OF GROUP COMMON MASK AND OPTION IN ARG0        |
      *|   (2) CHECK IF THE GROUP MASK IS EQUAL TO (1)                 |
      *|       IF EQUAL,    RTN01 IS 0                                 |
      *|       IF NO EQUAL, RTN01 IS 1                                 |
      *+---------------------------------------------------------------+
       ID DIVISION.
       PROGRAM-ID. EVALOPT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OUT1-REC.
         05 OUT1-DATA                PIC X(1) OCCURS 8.
       01 OUT1-DATA-R REDEFINES OUT1-REC.
         05 OUT1-DATA-UP             PIC X(4).
         05 OUT1-DATA-DOWN           PIC X(4).
       01  OUT2-REC.
         05  OUT2-DATA               PIC X(1) OCCURS 8.
       01  OUT2-DATA-R REDEFINES OUT2-REC.
         05 OUT2-DATA-UP             PIC X(4).
         05 OUT2-DATA-DOWN           PIC X(4).
       01  WORK1-REC.
         05  WORK1-DATA              PIC X(1) OCCURS 8.
       01  WORK1-DATA-R REDEFINES WORK1-REC.
         05 WORK1-DATA-UP            PIC X(4).
         05 WORK1-DATA-DOWN          PIC X(4).
       01  WORK-AREA.
         05  WORK-HEX-UP             PIC 9(4)  COMP.
         05  WORK-HEX-DOWN           PIC 9(4)  COMP.
       01  HEX-CHG-BEF.
         05  HEX-CHANGE-LV           PIC X(1) VALUE LOW-VALUE.
         05  HEX-CHANGE-BEFORE       PIC X(1).
       01  HEX-CHG-AFT      REDEFINES  HEX-CHG-BEF.
         05  HEX-CHANGE-AFTER        PIC 9(4)  COMP.
       01  TBL-CHANGE-DATA.
          05  FILLER                 PIC  X(004) VALUE '0000'.
          05  FILLER                 PIC  X(001) VALUE '0'.
          05  FILLER                 PIC  X(004) VALUE '0001'.
          05  FILLER                 PIC  X(001) VALUE '1'.
          05  FILLER                 PIC  X(004) VALUE '0010'.
          05  FILLER                 PIC  X(001) VALUE '2'.
          05  FILLER                 PIC  X(004) VALUE '0011'.
          05  FILLER                 PIC  X(001) VALUE '3'.
          05  FILLER                 PIC  X(004) VALUE '0100'.
          05  FILLER                 PIC  X(001) VALUE '4'.
          05  FILLER                 PIC  X(004) VALUE '0101'.
          05  FILLER                 PIC  X(001) VALUE '5'.
          05  FILLER                 PIC  X(004) VALUE '0110'.
          05  FILLER                 PIC  X(001) VALUE '6'.
          05  FILLER                 PIC  X(004) VALUE '0111'.
          05  FILLER                 PIC  X(001) VALUE '7'.
          05  FILLER                 PIC  X(004) VALUE '1000'.
          05  FILLER                 PIC  X(001) VALUE '8'.
          05  FILLER                 PIC  X(004) VALUE '1001'.
          05  FILLER                 PIC  X(001) VALUE '9'.
          05  FILLER                 PIC  X(004) VALUE '1010'.
          05  FILLER                 PIC  X(001) VALUE 'A'.
          05  FILLER                 PIC  X(004) VALUE '1011'.
          05  FILLER                 PIC  X(001) VALUE 'B'.
          05  FILLER                 PIC  X(004) VALUE '1100'.
          05  FILLER                 PIC  X(001) VALUE 'C'.
          05  FILLER                 PIC  X(004) VALUE '1101'.
          05  FILLER                 PIC  X(001) VALUE 'D'.
          05  FILLER                 PIC  X(004) VALUE '1110'.
          05  FILLER                 PIC  X(001) VALUE 'E'.
          05  FILLER                 PIC  X(004) VALUE '1111'.
          05  FILLER                 PIC  X(001) VALUE 'F'.
          01  TBL-DATA REDEFINES TBL-CHANGE-DATA.
           05  TBL-CHG  OCCURS  16 TIMES.
             10  TBL-BIT-CHAR        PIC  X(004).
             10  TBL-HEX-CHAR        PIC  X(001).
       01 BIT-COUNT                  PIC 9(1).
       01 I                          PIC S9(8) COMP.
       LINKAGE SECTION.
       01 G-MASK.
         03 D-G-MASK                 PIC X(1) OCCURS 19.
       01 COM-MASK.
         03 D-COM-MASK               PIC X(1) OCCURS 19.
       01 O-ARG0.
         03 D-O-ARG0                 PIC X(1) OCCURS 19.
       01 BYTE-COUNT                 PIC S9(8) COMP.
       01 RTN01                      PIC 9(1).
       PROCEDURE DIVISION USING G-MASK COM-MASK O-ARG0 BYTE-COUNT
            RTN01.
            MOVE 0 TO RTN01
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > BYTE-COUNT
              PERFORM ANDCOMMASK
              IF RTN01 = 1 THEN
                GOBACK
              END-IF
            END-PERFORM.
            EXIT PROGRAM.
       ANDCOMMASK.
      * CONVERT GROUP COMMON MASK TO BIT
            MOVE D-COM-MASK(I) TO HEX-CHANGE-BEFORE.
            DIVIDE 16 INTO HEX-CHANGE-AFTER GIVING WORK-HEX-UP
                                         REMAINDER WORK-HEX-DOWN.
            MOVE TBL-BIT-CHAR(WORK-HEX-UP + 1)   TO OUT1-DATA-UP.
            MOVE TBL-BIT-CHAR(WORK-HEX-DOWN + 1) TO OUT1-DATA-DOWN.
      * CONVERT OPTION IN ARG0 TO BIT
            MOVE D-O-ARG0(I) TO HEX-CHANGE-BEFORE.
            DIVIDE 16 INTO HEX-CHANGE-AFTER GIVING WORK-HEX-UP
                                         REMAINDER WORK-HEX-DOWN.
            MOVE TBL-BIT-CHAR(WORK-HEX-UP + 1)   TO OUT2-DATA-UP.
            MOVE TBL-BIT-CHAR(WORK-HEX-DOWN + 1) TO OUT2-DATA-DOWN.
      * CREATE EVAL BIT FROM GROUP COMMON MASK BIT AND ARG0 BIT
            PERFORM VARYING BIT-COUNT FROM 1 BY 1 UNTIL BIT-COUNT > 8
              IF OUT1-DATA(BIT-COUNT) = '1' AND
                 OUT2-DATA(BIT-COUNT) = '1' THEN
                MOVE '1' TO WORK1-DATA(BIT-COUNT)
              ELSE
                MOVE '0' TO WORK1-DATA(BIT-COUNT)
              END-IF
            END-PERFORM.
      * CONVERT GROUP MASK TO BIT DATA
            MOVE D-G-MASK(I) TO HEX-CHANGE-BEFORE.
            DIVIDE 16 INTO HEX-CHANGE-AFTER GIVING WORK-HEX-UP
                                         REMAINDER WORK-HEX-DOWN.
            MOVE TBL-BIT-CHAR(WORK-HEX-UP + 1)   TO OUT1-DATA-UP.
            MOVE TBL-BIT-CHAR(WORK-HEX-DOWN + 1) TO OUT1-DATA-DOWN.
      * CHECK IF EQUAL BETWEEN EVAL BIT AND GROUP MASK BIT
            IF WORK1-DATA-UP = OUT1-DATA-UP AND
               WORK1-DATA-DOWN = OUT1-DATA-DOWN THEN
              CONTINUE
            ELSE
              MOVE 1 TO RTN01
            END-IF
            EXIT.
       END PROGRAM 'EVALOPT'.
      *+---------------------------------------------------------------+
      *| GTMEMRC                                                       |
      *|     GET DATA AREA FOR RECORD COUNT OF SUBSYSTEM GROUP         |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'GTMEMRC'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZUGTMEM            PIC X(8) VALUE 'BZUGTMEM'.
       01 DATA-SIZE           PIC 9(8) COMP-4.
       LINKAGE SECTION.
       01 TC-WORK-AREA        PIC X(256).
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RECORD-PTR-VALUE
            REDEFINES AZ-RECORD-PTR  PIC S9(9) COMP-5.
       01 DATA-PTR            POINTER.
       01 DATA-PTR-VALUE
            REDEFINES DATA-PTR  PIC S9(9) COMP-5.
       01 DATA-AREA.
         03 RECORD-COUNT-IO OCCURS 6.
           05 RECORD-COUNT-OT PIC 9(5) COMP-5.
           05 RECORD-COUNT-IN PIC 9(5) COMP-5.
       01 WK-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION USING TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN
           AZ-RECORD-PTR.
           SET ADDRESS OF DATA-PTR TO ADDRESS OF TC-WORK-AREA.
           IF DATA-PTR-VALUE = 0 THEN
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 6
             CALL BZUGTMEM USING DATA-SIZE RETURNING DATA-PTR
             SET ADDRESS OF DATA-AREA TO DATA-PTR
             DISPLAY 'AREA ALLOCATED FOR RECORD COUNT:' DATA-SIZE
           END-IF
           SET AZ-RECORD-PTR TO DATA-PTR
           COMPUTE AZ-RECORD-PTR-VALUE = AZ-RECORD-PTR-VALUE +
                 LENGTH OF WK-RECORD-COUNT * 2 * (AZ-GRP-INDEX - 1)
           IF AZ-FLAG-IN = 1 THEN
             ADD LENGTH OF WK-RECORD-COUNT TO AZ-RECORD-PTR-VALUE
           END-IF
           SET ADDRESS OF WK-RECORD-COUNT TO AZ-RECORD-PTR
           GOBACK.
       END PROGRAM 'GTMEMRC'.
      *+---------------------------------------------------------------+
      *| AZU_GENERIC_CICS                                              |
      *|   GENERIC CICS CALLBACK EXIT POINT                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_CICS'.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * CICS_INPT.
           ENTRY 'CICS_INPT'.
           DISPLAY 'CICS_INPT ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * CICS_OUTP.
           ENTRY 'CICS_OUTP'.
           DISPLAY 'CICS_OUTP ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * CICS_INPT_0E08 FOR RETURN.
           ENTRY 'CICS_INPT_0E08'.
           DISPLAY 'CICS_INPT_0E08 ...'
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_CICS'.
      *+---------------------------------------------------------------+
      *| AZU_GENERIC_DB2                                               |
      *|   GENERIC DB2 CALLBACK EXIT POINT                             |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_DB2'.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT.
           ENTRY 'DB2_INPT'.
           DISPLAY 'DB2_INPT ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * DB2_OUTP.
           ENTRY 'DB2_OUTP'.
           DISPLAY 'DB2_OUTP ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_DB2'.
      *+---------------------------------------------------------------+
      *| PROGRAM FOR EXEC CICS RETURN                                  |
      *|    FUNCTION CODE: 0E08                                        |
      *|                                                               |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'CICS_0E08_HCMADB02'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM CICS CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-OPT-MASK-DATA2  PIC X(2).
       01 AZ-OPT-MASK-DATA9  PIC X(9).
       01 AZ-OPT-MASK-DATA11 PIC X(11).
       01 AZ-OPT-MASK-DATA19 PIC X(19).
       01 AZ-OPT-BYTECOUNT   PIC S9(8) COMP.
       01 AZ-OPT-RC          PIC 9(1) VALUE 0.
       01 AZ-OPT-COMMASK.
         03  AZ-OPT-COMMASK-DATA2  PIC X(2) OCCURS 1.
         03  AZ-OPT-COMMASK-DATA9  PIC X(9) OCCURS 1.
         03  AZ-OPT-COMMASK-DATA11 PIC X(11) OCCURS 1.
         03  AZ-OPT-COMMASK-DATA19 PIC X(19) OCCURS 1.
       01 AZ-LINE-BYTE      PIC S9(2) COMP.
       01 AZ-LINE-NUM       PIC 9(5).
       01 AZ-LINE-NUM-R   REDEFINES AZ-LINE-NUM.
         03 AZ-LINE-NUM-RD  PIC 9(1) OCCURS 5.
       01 AZ-LINE-I         PIC S9(8) COMP.
       01 AZ-LINE-J         PIC S9(8) COMP.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 AZ-MODX                   PIC X(4).
       01 AZ-DFHEIBLK.
         03 FILLER                  PIC X(85).
       01 AZ-DFHCOMMAREA.
         03 FILLER                  PIC X(1).
       01 ARG0.
         03 ARG0-1        PIC X(2).
         03 ARG0-A        PIC X(28).
         03 ARG0-B REDEFINES ARG0-A.
           05 ARG0-2      PIC X(9).
           05 FILLER      PIC X(19).
         03 ARG0-C REDEFINES ARG0-A.
           05 FILLER      PIC X(6).
           05 ARG0-D.
             07 ARG0-3    PIC 9(1) OCCURS 22.
       01 ARG1            POINTER.
       01 ARG2            POINTER.
       01 ARG3            POINTER.
       01 ARG4            POINTER.
       01 ARG5            POINTER.
       01 ARG6            POINTER.
       01 ARG7            POINTER.
       01 ARG8            POINTER.
       01 ARG9            POINTER.
       01 ARG10           POINTER.
       01 AZ-CICS-TARGET-NAME-DEF4 PIC X(4).
       01 AZ-CICS-TARGET-NAME-DEF7 PIC X(7).
       01 AZ-CICS-TARGET-NAME-DEF8 PIC X(8).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * CICS_INPT_0E08_HCMADB02.
           ENTRY 'CICS_INPT_0E08_HCMADB02' USING AZ-TEST
           AZ-INFO-BLOCK AZ-DFHEIBLK AZ-DFHCOMMAREA ARG0 ARG1 ARG2
           ARG3 ARG4 ARG5 ARG6 ARG7 ARG8 ARG9 ARG10.
           DISPLAY 'CICS_0E08_HCMADB02 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET GROUP OPTION COMMON MASK IN CICS GROUP
           MOVE X'0000' TO AZ-OPT-COMMASK-DATA2(1).
      * EXEC CICS RETURN X'0000'
           IF ARG0-1 = X'0E08'
             MOVE X'0000' TO AZ-OPT-MASK-DATA2
             MOVE 2 TO AZ-OPT-BYTECOUNT
             CALL 'EVALOPT' USING AZ-OPT-MASK-DATA2
               AZ-OPT-COMMASK-DATA2(1) ARG0-2
               AZ-OPT-BYTECOUNT AZ-OPT-RC
             IF AZ-OPT-RC = 0 THEN
               MOVE 3 TO AZ-LINE-BYTE
               PERFORM GETLINENUM
               DISPLAY 'EXEC CICS RETURN X''0000'''
                ' L=' AZ-LINE-NUM
               MOVE 2 TO AZ-GRP-INDEX
               MOVE 0 TO AZ-FLAG-IN
               CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
                 AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
               SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
               ADD 1 TO AZ-WK-RECORD-COUNT
               MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
               EVALUATE AZ-TEST(1:AZ-TEST-LEN)
                 WHEN SPACE
                   CONTINUE
                 WHEN 'TEST2'
                   PERFORM O0E080-TEST2
                   CONTINUE
                 WHEN OTHER
                   CONTINUE
                 END-EVALUATE
             END-IF
           END-IF.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
      * CICS_OUTP_0E08_HCMADB02.
           ENTRY 'CICS_OUTP_0E08_HCMADB02' USING AZ-TEST
           AZ-INFO-BLOCK AZ-DFHEIBLK AZ-DFHCOMMAREA ARG0 ARG1 ARG2
           ARG3 ARG4 ARG5 ARG6 ARG7 ARG8 ARG9 ARG10.
           DISPLAY 'CICS_0E08_HCMADB02 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET GROUP OPTION COMMON MASK IN CICS GROUP
           MOVE X'0000' TO AZ-OPT-COMMASK-DATA2(1).
      * EXEC CICS RETURN X'0000'
           IF ARG0-1 = X'0E08'
             MOVE X'0000' TO AZ-OPT-MASK-DATA2
             MOVE 2 TO AZ-OPT-BYTECOUNT
             CALL 'EVALOPT' USING AZ-OPT-MASK-DATA2
               AZ-OPT-COMMASK-DATA2(1) ARG0-2
               AZ-OPT-BYTECOUNT AZ-OPT-RC
             IF AZ-OPT-RC = 0 THEN
               MOVE 3 TO AZ-LINE-BYTE
               PERFORM GETLINENUM
               DISPLAY 'EXEC CICS RETURN X''0000'''
                ' L=' AZ-LINE-NUM
               MOVE 2 TO AZ-GRP-INDEX
               MOVE 1 TO AZ-FLAG-IN
               CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
                 AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
               SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
               ADD 1 TO AZ-WK-RECORD-COUNT
               MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
               EVALUATE AZ-TEST(1:AZ-TEST-LEN)
                 WHEN SPACE
                   CONTINUE
                 WHEN 'TEST2'
                   CONTINUE
                 WHEN OTHER
                   CONTINUE
                 END-EVALUATE
             END-IF
           END-IF.
           PERFORM TEARDOWN.
       O0E080-TEST2.
           IF AZ-RECORD-COUNT-OT(1) = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
       GETLINENUM.
           MOVE 1 TO AZ-LINE-J
           PERFORM VARYING AZ-LINE-I FROM AZ-LINE-BYTE BY 1
             UNTIL AZ-LINE-I > AZ-LINE-BYTE + 5
             MOVE ARG0-3(AZ-LINE-I) TO AZ-LINE-NUM-RD(AZ-LINE-J)
             ADD 1 TO AZ-LINE-J
           END-PERFORM
           EXIT.
       TEARDOWN.
           DISPLAY 'CICS_0E08_HCMADB02 SUCCESSFUL.'
           GOBACK.
       END PROGRAM 'CICS_0E08_HCMADB02'.
