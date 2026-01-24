You have developed this app following the strategy defined in "claude_generated_docs/cobol-analyzer-strategy.md".

Then you have updated the logic of the app as described in "claude_generated_docs/subordinate-redefines-strategy.md".

Now you have to adjust the input considering my real requirement.
Let's make an example.
Consider the file "complex-cobol-source/DATEUTIL.cbl" at line 106, which belongs to the paragraph 2000-TO-US, where you find the instruction:
MOVE WS-DATE-MONTH TO WS-US-MONTH

This instruction changed with a MOVE statement the value of WS-US-MONTH.

If you look at lines 11-16 you see the following data definitions:

       01  WS-DATE-WORK-AREA.
           05  WS-DATE-YYYYMMDD            PIC 9(8).
           05  WS-DATE-PARTS REDEFINES WS-DATE-YYYYMMDD.
               10  WS-DATE-YEAR            PIC 9(4).
               10  WS-DATE-MONTH           PIC 9(2).
               10  WS-DATE-DAY             PIC 9(2).

This means that changing the value of WS-DATE-MONTH changes also the value of WS-DATE-YYYYMMDD and changes the record WS-DATE-WORK-AREA.

The output you produce should show that under the paragraph 2000-TO-US a structure that contains the following logical info:
- The modification_type is MOVE
- the line is 106 (it is important to refer to the line number of the original source)
- the affected variables are WS-DATE-MONTH and WS-DATE-YYYYMMDD
- the affected record is WS-DATE-WORK-AREA (in this case just one since there are no REDEFINE at 01 level)

If there are REDEFINE at level 01, then in the list of affected variables I want to see all the variables under the REDEFINE which change because of the change in WS-DATE-MONTH.

Add other data that you may find important to register (for instance the level of each data structure or field) but do not pollute the output with unnecessary information.