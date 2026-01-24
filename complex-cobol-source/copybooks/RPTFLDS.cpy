      *================================================================*
      * REPORT FIELDS COPYBOOK - Report formatting structures
      *================================================================*
       01  REPORT-DATE-FIELDS.
           05  RPT-DATE-NUMERIC            PIC 9(8).
           05  RPT-DATE-FORMATTED REDEFINES RPT-DATE-NUMERIC.
               10  RPT-DATE-YYYY           PIC 9(4).
               10  RPT-DATE-MM             PIC 9(2).
               10  RPT-DATE-DD             PIC 9(2).
           05  RPT-DATE-DISPLAY            PIC X(10).

       01  REPORT-AMOUNT-FIELDS.
           05  RPT-AMT-NUMERIC             PIC S9(9)V99.
           05  RPT-AMT-DISPLAY             PIC $$$,$$$,$$9.99-.

       01  REPORT-COUNTERS.
           05  RPT-PAGE-NUM                PIC 9(4).
           05  RPT-LINE-NUM                PIC 9(3).
           05  RPT-RECORD-COUNT            PIC 9(7).
           05  RPT-ERROR-COUNT             PIC 9(5).
