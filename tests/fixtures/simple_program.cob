       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-EMPLOYEE-RECORD.
          05 WS-EMP-ID          PIC 9(5).
          05 WS-EMP-NAME        PIC X(30).
          05 WS-EMP-SALARY      PIC 9(7)V99.
          05 WS-EMP-DEPT        PIC X(10).

       01 WS-COUNTERS.
          05 WS-LOOP-CTR        PIC 9(3) VALUE 0.
          05 WS-TOTAL-CTR       PIC 9(5) VALUE 0.

       01 WS-FLAGS.
          05 WS-EOF-FLAG        PIC X VALUE 'N'.
             88 EOF-REACHED     VALUE 'Y'.
             88 NOT-EOF         VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN-SECTION SECTION.

       MAIN-PARA.
           PERFORM INIT-PARA
           PERFORM PROCESS-PARA UNTIL EOF-REACHED
           PERFORM CLEANUP-PARA
           STOP RUN.

       INIT-PARA.
           INITIALIZE WS-EMPLOYEE-RECORD
           MOVE 0 TO WS-LOOP-CTR
           MOVE 0 TO WS-TOTAL-CTR
           SET NOT-EOF TO TRUE.

       PROCESS-PARA.
           ADD 1 TO WS-LOOP-CTR
           MOVE 12345 TO WS-EMP-ID
           MOVE "JOHN DOE" TO WS-EMP-NAME
           COMPUTE WS-EMP-SALARY = WS-EMP-SALARY * 1.05
           ADD 1 TO WS-TOTAL-CTR
           IF WS-LOOP-CTR > 100
               SET EOF-REACHED TO TRUE
           END-IF.

       CLEANUP-PARA.
           DISPLAY "TOTAL PROCESSED: " WS-TOTAL-CTR.
