       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROC-DIV-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-EMPLOYEE-RECORD.
          05 WS-EMP-ID            PIC 9(5).
          05 WS-EMP-NAME          PIC X(30).
          05 WS-EMP-SALARY        PIC 9(7)V99.
          05 WS-EMP-STATUS        PIC X(1).
             88 EMP-ACTIVE        VALUE 'A'.
             88 EMP-INACTIVE      VALUE 'I'.
             88 EMP-TERMINATED    VALUE 'T'.

       01 WS-COUNTERS.
          05 WS-LOOP-CTR          PIC 9(3) VALUE 0.
          05 WS-TOTAL-CTR         PIC 9(5) VALUE 0.
          05 WS-ERROR-CTR         PIC 9(3) VALUE 0.

       01 WS-FLAGS.
          05 WS-EOF-FLAG          PIC X VALUE 'N'.
             88 EOF-REACHED       VALUE 'Y'.
             88 NOT-EOF           VALUE 'N'.
          05 WS-VALID-FLAG        PIC X VALUE 'Y'.
             88 DATA-VALID        VALUE 'Y'.
             88 DATA-INVALID      VALUE 'N'.

       01 WS-REPORT-LINE          PIC X(80).
       01 WS-RETURN-CODE          PIC 9(4) VALUE 0.
       01 WS-PROGRAM-NAME         PIC X(8).

       LINKAGE SECTION.

       01 LS-PARAMETER-AREA.
          05 LS-FUNCTION-CODE     PIC X(2).
          05 LS-DATA-BUFFER       PIC X(100).

       PROCEDURE DIVISION.

       MAIN-SECTION SECTION.

       MAIN-PARA.
           PERFORM INIT-PARA
           PERFORM PROCESS-PARA THRU PROCESS-EXIT
               UNTIL EOF-REACHED
           PERFORM REPORT-SECTION
           PERFORM CLEANUP-PARA
           STOP RUN.

       INIT-PARA.
           INITIALIZE WS-EMPLOYEE-RECORD
           MOVE 0 TO WS-LOOP-CTR
           MOVE 0 TO WS-TOTAL-CTR
           SET NOT-EOF TO TRUE
           SET DATA-VALID TO TRUE.

       VALIDATION-SECTION SECTION.

       VALIDATE-PARA.
           IF WS-EMP-ID = ZERO
               SET DATA-INVALID TO TRUE
               ADD 1 TO WS-ERROR-CTR
           ELSE
               IF WS-EMP-NAME = SPACES
                   SET DATA-INVALID TO TRUE
                   ADD 1 TO WS-ERROR-CTR
               END-IF
           END-IF.

       PROCESS-SECTION SECTION.

       PROCESS-PARA.
           ADD 1 TO WS-LOOP-CTR
           PERFORM VALIDATE-PARA
           IF DATA-VALID
               EVALUATE WS-EMP-STATUS
                   WHEN 'A'
                       PERFORM PROCESS-ACTIVE
                   WHEN 'I'
                       MOVE 0 TO WS-EMP-SALARY
                   WHEN OTHER
                       ADD 1 TO WS-ERROR-CTR
               END-EVALUATE
               ADD 1 TO WS-TOTAL-CTR
           END-IF.

       PROCESS-ACTIVE.
           COMPUTE WS-EMP-SALARY = WS-EMP-SALARY * 1.05
           MOVE WS-EMP-NAME TO WS-REPORT-LINE.

       PROCESS-EXIT.
           EXIT.

       REPORT-SECTION SECTION.

       REPORT-PARA.
           DISPLAY "TOTAL PROCESSED: " WS-TOTAL-CTR
           DISPLAY "ERRORS: " WS-ERROR-CTR
           IF WS-ERROR-CTR > 0
               GO TO ERROR-REPORT-PARA
           END-IF.

       SUMMARY-PARA.
           MOVE WS-TOTAL-CTR TO WS-REPORT-LINE
           DISPLAY WS-REPORT-LINE.

       ERROR-REPORT-PARA.
           DISPLAY "ERROR REPORT FOLLOWS"
           MOVE WS-ERROR-CTR TO WS-REPORT-LINE
           DISPLAY WS-REPORT-LINE.

       EXTERNAL-SECTION SECTION.

       CALL-STATIC-PARA.
           CALL 'DATEVAL' USING WS-EMP-ID
                                WS-RETURN-CODE.

       CALL-DYNAMIC-PARA.
           MOVE 'RPTGEN' TO WS-PROGRAM-NAME
           CALL WS-PROGRAM-NAME USING WS-EMPLOYEE-RECORD
                                      WS-REPORT-LINE.

       CLEANUP-SECTION SECTION.

       CLEANUP-PARA.
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-FLAGS
           DISPLAY "PROCESSING COMPLETE".
