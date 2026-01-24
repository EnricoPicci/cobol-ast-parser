       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATEUTIL.
      *================================================================*
      * DATE UTILITY PROGRAM
      * Demonstrates REDEFINES for date format conversions
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-DATE-WORK-AREA.
           05  WS-DATE-YYYYMMDD            PIC 9(8).
           05  WS-DATE-PARTS REDEFINES WS-DATE-YYYYMMDD.
               10  WS-DATE-YEAR            PIC 9(4).
               10  WS-DATE-MONTH           PIC 9(2).
               10  WS-DATE-DAY             PIC 9(2).

       01  WS-DATE-MMDDYYYY.
           05  WS-US-MONTH                 PIC 9(2).
           05  WS-US-DAY                   PIC 9(2).
           05  WS-US-YEAR                  PIC 9(4).

       01  WS-DATE-DDMMYYYY.
           05  WS-EU-DAY                   PIC 9(2).
           05  WS-EU-MONTH                 PIC 9(2).
           05  WS-EU-YEAR                  PIC 9(4).

       01  WS-JULIAN-DATE.
           05  WS-JULIAN-YEAR              PIC 9(4).
           05  WS-JULIAN-DAY               PIC 9(3).

       01  WS-FORMATTED-DATE.
           05  WS-FMT-MONTH-NAME           PIC X(9).
           05  FILLER                      PIC X VALUE SPACE.
           05  WS-FMT-DAY                  PIC Z9.
           05  FILLER                      PIC X(2) VALUE ', '.
           05  WS-FMT-YEAR                 PIC 9(4).

       01  WS-MONTH-TABLE.
           05  FILLER PIC X(9) VALUE 'JANUARY  '.
           05  FILLER PIC X(9) VALUE 'FEBRUARY '.
           05  FILLER PIC X(9) VALUE 'MARCH    '.
           05  FILLER PIC X(9) VALUE 'APRIL    '.
           05  FILLER PIC X(9) VALUE 'MAY      '.
           05  FILLER PIC X(9) VALUE 'JUNE     '.
           05  FILLER PIC X(9) VALUE 'JULY     '.
           05  FILLER PIC X(9) VALUE 'AUGUST   '.
           05  FILLER PIC X(9) VALUE 'SEPTEMBER'.
           05  FILLER PIC X(9) VALUE 'OCTOBER  '.
           05  FILLER PIC X(9) VALUE 'NOVEMBER '.
           05  FILLER PIC X(9) VALUE 'DECEMBER '.

       01  WS-MONTH-NAMES REDEFINES WS-MONTH-TABLE.
           05  WS-MONTH-NAME               PIC X(9) OCCURS 12 TIMES.

       01  WS-DAYS-IN-MONTH-TABLE.
           05  FILLER PIC 9(2) VALUE 31.
           05  FILLER PIC 9(2) VALUE 28.
           05  FILLER PIC 9(2) VALUE 31.
           05  FILLER PIC 9(2) VALUE 30.
           05  FILLER PIC 9(2) VALUE 31.
           05  FILLER PIC 9(2) VALUE 30.
           05  FILLER PIC 9(2) VALUE 31.
           05  FILLER PIC 9(2) VALUE 31.
           05  FILLER PIC 9(2) VALUE 30.
           05  FILLER PIC 9(2) VALUE 31.
           05  FILLER PIC 9(2) VALUE 30.
           05  FILLER PIC 9(2) VALUE 31.

       01  WS-DAYS-TABLE REDEFINES WS-DAYS-IN-MONTH-TABLE.
           05  WS-DAYS-IN-MONTH            PIC 9(2) OCCURS 12 TIMES.

       01  WS-CALCULATION-FIELDS.
           05  WS-LEAP-YEAR-FLAG           PIC X VALUE 'N'.
               88  IS-LEAP-YEAR            VALUE 'Y'.
               88  NOT-LEAP-YEAR           VALUE 'N'.
           05  WS-WORK-DAYS                PIC 9(3).
           05  WS-MONTH-IDX                PIC 9(2).
           05  WS-REMAINDER                PIC 9(4).

       PROCEDURE DIVISION.

       0000-MAIN SECTION.

       0000-START.
           PERFORM 1000-TEST-CONVERSIONS
           STOP RUN.

       1000-TEST-CONVERSIONS SECTION.

       1000-TEST.
           MOVE 20240315 TO WS-DATE-YYYYMMDD
           MOVE 01 TO WS-DATE-MONTH
           PERFORM 2000-CONVERT-TO-US-FORMAT
           PERFORM 3000-CONVERT-TO-EU-FORMAT
           PERFORM 4000-CONVERT-TO-JULIAN
           PERFORM 5000-FORMAT-LONG-DATE
           DISPLAY 'ISO FORMAT:    ' WS-DATE-YYYYMMDD
           DISPLAY 'US FORMAT:     ' WS-DATE-MMDDYYYY
           DISPLAY 'EU FORMAT:     ' WS-DATE-DDMMYYYY
           DISPLAY 'JULIAN:        ' WS-JULIAN-DATE
           DISPLAY 'LONG FORMAT:   ' WS-FORMATTED-DATE.

       2000-CONVERT-TO-US-FORMAT SECTION.

       2000-TO-US.
           MOVE WS-DATE-MONTH TO WS-US-MONTH
           MOVE WS-DATE-DAY TO WS-US-DAY
           MOVE WS-DATE-YEAR TO WS-US-YEAR.

       3000-CONVERT-TO-EU-FORMAT SECTION.

       3000-TO-EU.
           MOVE WS-DATE-DAY TO WS-EU-DAY
           MOVE WS-DATE-MONTH TO WS-EU-MONTH
           MOVE WS-DATE-YEAR TO WS-EU-YEAR.

       4000-CONVERT-TO-JULIAN SECTION.

       4000-TO-JULIAN.
           MOVE WS-DATE-YEAR TO WS-JULIAN-YEAR
           PERFORM 4100-CHECK-LEAP-YEAR
           MOVE 0 TO WS-WORK-DAYS
           PERFORM VARYING WS-MONTH-IDX FROM 1 BY 1
               UNTIL WS-MONTH-IDX >= WS-DATE-MONTH
               ADD WS-DAYS-IN-MONTH(WS-MONTH-IDX) TO WS-WORK-DAYS
               IF WS-MONTH-IDX = 2 AND IS-LEAP-YEAR
                   ADD 1 TO WS-WORK-DAYS
               END-IF
           END-PERFORM
           ADD WS-DATE-DAY TO WS-WORK-DAYS
           MOVE WS-WORK-DAYS TO WS-JULIAN-DAY.

       4100-CHECK-LEAP-YEAR.
           SET NOT-LEAP-YEAR TO TRUE
           DIVIDE WS-DATE-YEAR BY 4 GIVING WS-REMAINDER
               REMAINDER WS-REMAINDER
           IF WS-REMAINDER = 0
               DIVIDE WS-DATE-YEAR BY 100 GIVING WS-REMAINDER
                   REMAINDER WS-REMAINDER
               IF WS-REMAINDER NOT = 0
                   SET IS-LEAP-YEAR TO TRUE
               ELSE
                   DIVIDE WS-DATE-YEAR BY 400 GIVING WS-REMAINDER
                       REMAINDER WS-REMAINDER
                   IF WS-REMAINDER = 0
                       SET IS-LEAP-YEAR TO TRUE
                   END-IF
               END-IF
           END-IF.

       5000-FORMAT-LONG-DATE SECTION.

       5000-FORMAT.
           MOVE WS-MONTH-NAME(WS-DATE-MONTH) TO WS-FMT-MONTH-NAME
           MOVE WS-DATE-DAY TO WS-FMT-DAY
           MOVE WS-DATE-YEAR TO WS-FMT-YEAR.
