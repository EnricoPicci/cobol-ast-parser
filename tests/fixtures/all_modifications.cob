       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALL-MODS-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-NUMERIC-FIELDS.
          05 WS-NUM-A           PIC 9(5) VALUE 100.
          05 WS-NUM-B           PIC 9(5) VALUE 50.
          05 WS-NUM-C           PIC 9(5) VALUE 0.
          05 WS-NUM-D           PIC 9(5) VALUE 0.

       01 WS-TEXT-FIELDS.
          05 WS-TEXT-A          PIC X(20).
          05 WS-TEXT-B          PIC X(20).
          05 WS-TEXT-C          PIC X(40).
          05 WS-TEXT-D          PIC X(10).
          05 WS-TEXT-E          PIC X(10).

       01 WS-TABLE-AREA.
          05 WS-TABLE-ITEM OCCURS 10 TIMES.
             10 WS-TABLE-KEY    PIC 9(3).
             10 WS-TABLE-VALUE  PIC X(20).

       01 WS-INDEX-FIELD        PIC 9(3).
       01 WS-TALLY-FIELD        PIC 9(5).
       01 WS-INPUT-FIELD        PIC X(50).

       PROCEDURE DIVISION.

       TEST-SECTION SECTION.

       TEST-MOVE-PARA.
           MOVE 100 TO WS-NUM-A
           MOVE WS-NUM-A TO WS-NUM-B, WS-NUM-C
           MOVE "HELLO" TO WS-TEXT-A.

       TEST-COMPUTE-PARA.
           COMPUTE WS-NUM-C = WS-NUM-A + WS-NUM-B
           COMPUTE WS-NUM-D = WS-NUM-A * 2 + WS-NUM-B / 2.

       TEST-ADD-PARA.
           ADD 10 TO WS-NUM-A
           ADD WS-NUM-A TO WS-NUM-B.

       TEST-SUBTRACT-PARA.
           SUBTRACT 5 FROM WS-NUM-A
           SUBTRACT WS-NUM-B FROM WS-NUM-A.

       TEST-MULTIPLY-PARA.
           MULTIPLY 2 BY WS-NUM-A
           MULTIPLY WS-NUM-A BY WS-NUM-B.

       TEST-DIVIDE-PARA.
           DIVIDE 2 INTO WS-NUM-A
           DIVIDE WS-NUM-A INTO WS-NUM-B.

       TEST-STRING-PARA.
           STRING WS-TEXT-A DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  WS-TEXT-B DELIMITED BY SPACE
                  INTO WS-TEXT-C.

       TEST-UNSTRING-PARA.
           UNSTRING WS-TEXT-C DELIMITED BY SPACE
                    INTO WS-TEXT-D, WS-TEXT-E.

       TEST-INSPECT-PARA.
           INSPECT WS-TEXT-A TALLYING WS-TALLY-FIELD
                   FOR ALL "A"
           INSPECT WS-TEXT-B REPLACING ALL "X" BY "Y".

       TEST-INITIALIZE-PARA.
           INITIALIZE WS-NUMERIC-FIELDS
           INITIALIZE WS-TEXT-FIELDS.

       TEST-SET-PARA.
           SET WS-INDEX-FIELD TO 1.

       TEST-ACCEPT-PARA.
           ACCEPT WS-INPUT-FIELD FROM CONSOLE.
