If you run the command:
cobol-analyzer paragraph-variables-map complex-cobol-source/ORDERMGMT.cbl -c complex-cobol-source/copybooks -o output-x

you see that the *.paragraph-variables.json file does not show that the variable CUSTOMER-STATUS can change value because of the MOVE instruction at line 24.

This is probably because line 24 is not within any PARAGRAPH or SECTION.

This is a bug, since the *.paragraph-variables.json file must contain all the variables that may change because of the execution of the program.

Fix the bus adding a key "PROCEDURE DIVISION" to the *.paragraph-variables.json file where you store a dictionary that contain the variables that may change because of the execution of statements in the PROCEDURE DIVISION which are not in any PARAGRAPH or SECTION.