       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMAIN.
      *================================================================*
      * CUSTOMER MAINTENANCE PROGRAM
      * Handles customer add, update, delete operations
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-KEY
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
           COPY CUSTREC.

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS                  PIC XX.
           88  WS-FILE-OK                  VALUE '00'.
           88  WS-FILE-NOT-FOUND           VALUE '23'.
           88  WS-FILE-DUP-KEY             VALUE '22'.

       01  WS-WORK-CUSTOMER.
           COPY CUSTREC.

       01  WS-INPUT-BUFFER.
           05  WS-INPUT-RECORD             PIC X(300).

       01  WS-INPUT-PARSED REDEFINES WS-INPUT-BUFFER.
           05  WS-INP-ACTION               PIC X(1).
               88  WS-ACTION-ADD           VALUE 'A'.
               88  WS-ACTION-UPDATE        VALUE 'U'.
               88  WS-ACTION-DELETE        VALUE 'D'.
               88  WS-ACTION-INQUIRY       VALUE 'I'.
           05  WS-INP-CUST-ID              PIC 9(8).
           05  WS-INP-DATA                 PIC X(291).

       01  WS-NUMERIC-WORK.
           05  WS-EDIT-AMOUNT              PIC 9(7)V99.
           05  WS-CALC-AMOUNT              PIC S9(9)V99.
           05  WS-PERCENT                  PIC V999.

       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC X VALUE 'N'.
               88  WS-EOF                  VALUE 'Y'.
               88  WS-NOT-EOF              VALUE 'N'.
           05  WS-VALID-FLAG               PIC X VALUE 'Y'.
               88  WS-VALID                VALUE 'Y'.
               88  WS-INVALID              VALUE 'N'.

       01  WS-COUNTERS.
           05  WS-RECORDS-READ             PIC 9(7) VALUE 0.
           05  WS-RECORDS-ADDED            PIC 9(7) VALUE 0.
           05  WS-RECORDS-UPDATED          PIC 9(7) VALUE 0.
           05  WS-RECORDS-DELETED          PIC 9(7) VALUE 0.
           05  WS-RECORDS-ERROR            PIC 9(7) VALUE 0.

           COPY RPTFLDS.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS SECTION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-INPUT UNTIL WS-EOF
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE SECTION.

       1000-INIT.
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-FLAGS
           MOVE 0 TO RPT-PAGE-NUM
           MOVE 0 TO RPT-LINE-NUM
           MOVE 0 TO RPT-RECORD-COUNT
           OPEN I-O CUSTOMER-FILE
           IF NOT WS-FILE-OK
               DISPLAY 'ERROR OPENING CUSTOMER FILE: ' WS-FILE-STATUS
               MOVE 'Y' TO WS-EOF-FLAG
           END-IF.

       2000-PROCESS-INPUT SECTION.

       2000-PROCESS.
           ACCEPT WS-INPUT-BUFFER FROM CONSOLE
           IF WS-INPUT-BUFFER = SPACES
               SET WS-EOF TO TRUE
           ELSE
               ADD 1 TO WS-RECORDS-READ
               EVALUATE TRUE
                   WHEN WS-ACTION-ADD
                       PERFORM 3000-ADD-CUSTOMER
                   WHEN WS-ACTION-UPDATE
                       PERFORM 4000-UPDATE-CUSTOMER
                   WHEN WS-ACTION-DELETE
                       PERFORM 5000-DELETE-CUSTOMER
                   WHEN WS-ACTION-INQUIRY
                       PERFORM 6000-INQUIRY-CUSTOMER
                   WHEN OTHER
                       ADD 1 TO WS-RECORDS-ERROR
                       DISPLAY 'INVALID ACTION CODE'
               END-EVALUATE
           END-IF.

       3000-ADD-CUSTOMER SECTION.

       3000-ADD.
           INITIALIZE WS-WORK-CUSTOMER
           MOVE WS-INP-CUST-ID TO CUST-ID OF WS-WORK-CUSTOMER
           PERFORM 3100-PARSE-CUSTOMER-DATA
           IF WS-VALID
               WRITE CUSTOMER-RECORD FROM WS-WORK-CUSTOMER
               IF WS-FILE-OK
                   ADD 1 TO WS-RECORDS-ADDED
                   ADD 1 TO RPT-RECORD-COUNT
               ELSE
                   ADD 1 TO WS-RECORDS-ERROR
                   ADD 1 TO RPT-ERROR-COUNT
               END-IF
           END-IF.

       3100-PARSE-CUSTOMER-DATA.
           SET WS-VALID TO TRUE
           MOVE WS-INP-DATA(1:25) TO CUST-FIRST-NAME
                                     OF WS-WORK-CUSTOMER
           MOVE WS-INP-DATA(26:30) TO CUST-LAST-NAME
                                      OF WS-WORK-CUSTOMER
           MOVE WS-INP-DATA(56:1) TO CUST-MIDDLE-INIT
                                     OF WS-WORK-CUSTOMER
           MOVE WS-INP-DATA(57:40) TO CUST-STREET
                                      OF WS-WORK-CUSTOMER
           MOVE WS-INP-DATA(97:25) TO CUST-CITY
                                      OF WS-WORK-CUSTOMER
           MOVE WS-INP-DATA(122:2) TO CUST-STATE
                                      OF WS-WORK-CUSTOMER
           MOVE 10000.00 TO CUST-CREDIT-LIMIT OF WS-WORK-CUSTOMER
           MOVE 0 TO CUST-BALANCE OF WS-WORK-CUSTOMER.

       4000-UPDATE-CUSTOMER SECTION.

       4000-UPDATE.
           MOVE WS-INP-CUST-ID TO CUST-ID OF CUSTOMER-RECORD
           READ CUSTOMER-FILE INTO WS-WORK-CUSTOMER
           IF WS-FILE-OK
               PERFORM 4100-APPLY-UPDATES
               REWRITE CUSTOMER-RECORD FROM WS-WORK-CUSTOMER
               IF WS-FILE-OK
                   ADD 1 TO WS-RECORDS-UPDATED
               ELSE
                   ADD 1 TO WS-RECORDS-ERROR
               END-IF
           ELSE
               ADD 1 TO WS-RECORDS-ERROR
               DISPLAY 'CUSTOMER NOT FOUND: ' WS-INP-CUST-ID
           END-IF.

       4100-APPLY-UPDATES.
           IF WS-INP-DATA(1:25) NOT = SPACES
               MOVE WS-INP-DATA(1:25) TO CUST-FIRST-NAME
                                         OF WS-WORK-CUSTOMER
           END-IF
           IF WS-INP-DATA(26:30) NOT = SPACES
               MOVE WS-INP-DATA(26:30) TO CUST-LAST-NAME
                                          OF WS-WORK-CUSTOMER
           END-IF
           COMPUTE CUST-BALANCE OF WS-WORK-CUSTOMER =
               CUST-BALANCE OF WS-WORK-CUSTOMER + WS-CALC-AMOUNT.

       5000-DELETE-CUSTOMER SECTION.

       5000-DELETE.
           MOVE WS-INP-CUST-ID TO CUST-ID OF CUSTOMER-RECORD
           READ CUSTOMER-FILE
           IF WS-FILE-OK
               DELETE CUSTOMER-FILE RECORD
               IF WS-FILE-OK
                   ADD 1 TO WS-RECORDS-DELETED
               ELSE
                   ADD 1 TO WS-RECORDS-ERROR
               END-IF
           ELSE
               ADD 1 TO WS-RECORDS-ERROR
           END-IF.

       6000-INQUIRY-CUSTOMER SECTION.

       6000-INQUIRY.
           MOVE WS-INP-CUST-ID TO CUST-ID OF CUSTOMER-RECORD
           READ CUSTOMER-FILE INTO WS-WORK-CUSTOMER
           IF WS-FILE-OK
               DISPLAY 'CUSTOMER: ' CUST-FULL-NAME OF WS-WORK-CUSTOMER
               MOVE CUST-BALANCE OF WS-WORK-CUSTOMER
                   TO RPT-AMT-NUMERIC
               DISPLAY 'BALANCE: ' RPT-AMT-DISPLAY
           ELSE
               DISPLAY 'CUSTOMER NOT FOUND'
           END-IF.

       9000-FINALIZE SECTION.

       9000-FINAL.
           CLOSE CUSTOMER-FILE
           DISPLAY 'RECORDS READ:    ' WS-RECORDS-READ
           DISPLAY 'RECORDS ADDED:   ' WS-RECORDS-ADDED
           DISPLAY 'RECORDS UPDATED: ' WS-RECORDS-UPDATED
           DISPLAY 'RECORDS DELETED: ' WS-RECORDS-DELETED
           DISPLAY 'RECORDS ERROR:   ' WS-RECORDS-ERROR.
