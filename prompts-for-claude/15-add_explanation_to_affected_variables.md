The CLI of this app generated 2 json files.
In particular look at the json file whose name is *-paragraph-variables.json
This json file lists, for each paragraph, the variables that may change value during the execution of that paragraph logic.
The reasons they may change variables are:
- direct change due to the fact that a Cobol instruction changes the value (e.g. MOVE 'abc' TO MY-VAR changes directly the value of MY-VAR)
- indirect change due to Cobol REDEFINE logic (e.g. if MY-SECOND-VAR is defined in a record that REDEFINEs the record where MY-VAR is defined and all or parts of MY-SECOND-VAR overlap all or parts of MY-VAR, then changing MY-VAR changes also MY-SECOND-VAR)

I would like to have an additional property added to objects that are the values for the keys representing the variable that may change (affected variable).
For instance look at the following json snippet which represents what I would like to achieve:
    "2000-PROCESS": {
      "PAY-BANK-CODE": {
        "base_record": "TRANSACTION-RECORD",
        "defined_in_record": "PAYMENT-DETAIL",
        "explanation": "changes because ...."
      },

The reasons for the change can be:
- "MY-VAR changes because of MOVE at line 133"
- "MY-SECOND-VAR changes because it occupy positions xx-yy in record MY-SECOND-REC which is redefined by MY-RECORD where MY-VAR occupies positions xx-zz"

Review this requirement, highligh any gap and then write the strategy and the plan to implement it in a document to be places in "claude_generated_docs" folder

# Refinement
- Position numbering is 1-indexed like in COBOL
- in case of multiple modifications:
    - the explanation should state something like "MY-VAR is affected by multiple modifications that occur at lines xx, yy, zz"
    - if some modifications are due to REDEFINE, then the text should also say that "some modifications are due to REDEFINE" and then add the details about the overlapping for one example or REDEFINE
- do not add a compact mode
- Backward compatibility is not required

Update the strategy and plan document accordingly
