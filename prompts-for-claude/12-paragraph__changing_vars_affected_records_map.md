Now we need to generate another output json.

This outputjson is a dictionary where:
- the key is the name of each PARAGRAPH or SECTION is in the Cobol source code.
- the value is a dictionary where:
    - keys are all the variables that may change withinthe PARAGRAPH or SECTION
    - values are objects that contain the following property:
        - the record level 01 under which the variable is defined
        - the level 01 record that is not a REDEFINE, in other words if MYVAR is defined in the level 01 structure MYREC-REDEFINE which is the REDEFINE of MYREC, in this property I want to have MYREC

Tell me if there is any weakness in this reasoning and provide with the strategy you suggest to achieve this goal.

Here my clarifications
1. affected_variables from REDEFINES are to be considered as "variables that may change"
2. ancestor modifications (e.g., INITIALIZE of parent) cause child variables to appear
3. for 77-level items both properties should have the same value as the name of the variable and, to ensure easier human read, there should be an addtional property "77-level-var" set to true (other variables should not have such property)
4. this should be a new command

Make sure you read .claude/claude.md for guidelines, in particular for guidelines about updating documentation and adding relevant tests


Consider the program "complex-cobol-source/TRANPROC.cbl" and the copybooks in "complex-cobol-source/copybooks".
In line 132 of the program there this instrction:
MOVE 'abcd' TO TRAN-CUSTOMER-ID

This means that "TRAN-CUSTOMER-ID" is changed.
"PAYMENT-DETAIL" redefines "TRANSACTION-RECORD" which is where "TRAN-CUSTOMER-ID" is defined, therefore some of the variables defined by "PAYMENT-DETAIL" change, but not all of the variables.
To calculate the affected variables you have to consider the position of the variables in the record definition at level 01.
If I run the command "python -m src paragraph-variables-map complex-cobol-source/TRANPROC.cbl -c complex-cobol-source/copybooks -o output" I see that you list all of the variables defined in "PAYMENT-DETAIL" as affected variables, which is not what I want. I want to see only the variables that are changed considering the position of "TRAN-CUSTOMER-ID" in "TRANSACTION-RECORD".
Let me know if there is something not clear, otherwise update the strategy document and then implement the change.