      *****************************************************************
      * Used on CICS trx HCAZ
      * invoked hen selecting option 2 - Inquire Patient
      * Changed Dec 17 2020 -   12:15
      *  Look for %bug to introduce bugs
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCIPDB01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(26)
                                        VALUE 'HCIPDB01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.
      *----------------------------------------------------------------*
       COPY HCERRSWS.
      *----------------------------------------------------------------*
      * Fields to be used to calculate if commarea is large enough
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.
      *----------------------------------------------------------------*
      * Definitions required by SQL statement                          *
      *   DB2 datatypes to COBOL equivalents                           *
      *     SMALLINT    :   PIC S9(4) COMP                             *
      *     INTEGER     :   PIC S9(9) COMP                             *
      *     DATE        :   PIC X(10)                                  *
      *     TIMESTAMP   :   PIC X(26)                                  *
      *----------------------------------------------------------------*
      * Host variables for input to DB2 integer types
       01  DB2-IN.
           03 DB2-PATIENT-ID           PIC S9(9) COMP.
      *----------------------------------------------------------------*
      *    DB2 CONTROL
      *----------------------------------------------------------------*
      * SQLCA DB2 communications area
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           EXEC SQL
      *    %bug1 add a new field
             INCLUDE HCCMAREA
           END-EXEC.
      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAINLINE SECTION.
      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE WS-HEADER.
      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('HCCA') NODUMP END-EXEC
           END-IF
      * initialize commarea return code to zero
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.
      * initialize DB2 host variables
      *    INITIALIZE DB2-IN-INTEGERS.
      *----------------------------------------------------------------*
      * Process incoming commarea                                      *
      *----------------------------------------------------------------*
      * check commarea length - meets minimum requirement
           ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN
      * if less set error return code and return to caller
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF
      * Convert commarea patient id to DB2 integer format
           MOVE CA-PATIENT-ID TO DB2-PATIENT-ID
      * and save in error msg field incase required
           MOVE CA-PATIENT-ID TO EM-PATNUM
      *----------------------------------------------------------------*
      * Obtain details from DB2                                        *
      *----------------------------------------------------------------*
      *    Call routine to issue SQL to obtain info from DB2
           PERFORM GET-PATIENT-INFO.
      *----------------------------------------------------------------*
      * END PROGRAM and return to caller                               *
      *----------------------------------------------------------------*
       MAINLINE-END.
           EXEC CICS RETURN END-EXEC.
       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       GET-PATIENT-INFO.
           EXEC SQL
               SELECT FIRSTNAME,
                      LASTNAME,
                      DATEOFBIRTH,
                      insCardNumber,
                      ADDRESS,
                      CITY,
                      POSTCODE,
                      PHONEMOBILE,
                      EMAILADDRESS,
                      USERNAME
               INTO  :CA-FIRST-NAME,
                     :CA-LAST-NAME,
                     :CA-DOB,
                     :CA-INS-CARD-NUM,
                     :CA-ADDRESS,
                     :CA-CITY,
                     :CA-POSTCODE,
                     :CA-PHONE-MOBILE,
                     :CA-EMAIL-ADDRESS,
                     :CA-USERID
               FROM PATIENT
               WHERE PATIENTID = :DB2-PATIENT-ID
               END-EXEC.
           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
             When 100
               MOVE '01' TO CA-RETURN-CODE
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.
      * %bug2 -- the line below will introduce a BUG
      *----------------------------------------------------------------*
      *         IF DB2-PATIENT-ID = 1
      *              MOVE "BAD NAME" to CA-USERID
      *         END-IF
      *           MOVE "02" to CA-NEWFIELD
      *----------------------------------------------------------------*
           EXIT.
      *----------------------------------------------------------------*
       COPY HCERRSPD.