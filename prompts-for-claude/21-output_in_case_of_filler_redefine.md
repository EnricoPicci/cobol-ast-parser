If you run the command:
cobol-analyzer paragraph-variables-map complex-cobol-source/ORDERMGMT.cbl -c complex-cobol-source/copybooks -o output-x

the app generates a *.paragraph-variables.json file.
This file, for each PARAGRAPH or SECTION or PROCEDURE DIVISION, contains a dictionary whose keys are the variables that may change value because of the execution of the PARAGRAPH or SECTION or PROCEDURE DIVISION.

Let's consider the following json snippet
    "INIT-CUSTOMER": {
      "CUSTOMER-ID": {
        "base_record": "ORDER-BUFFER",
        "defined_in_record": "FILLER$1",
        "explanation": "direct modification: MOVE at line 27"
      },
      ...
    },

This snippet says that the variable "CUSTOMER-ID" may change in the paragraph "INIT-CUSTOMER".
Imagine I have a log where I save the data that are in the area defined by "ORDER-BUFFER" at the beginning and at the end of the execution of the paragraph "INIT-CUSTOMER".
My objective is to know:
- which is the position of "CUSTOMER-ID" so that I can extract the value of "CUSTOMER-ID" at the beginning and at the end of the execution of the paragraph "INIT-CUSTOMER" using the logged data I already have
- that "CUSTOMER-ID" is defined in the copybook CUSTINFO under the FILLER that REDEFINEs ORDER-BUFFER - this information must be saved in the property "defined_in_record"

Your task is to devise a strategy and a plan to implement this new requirement.
Do not change the behavior of the app in cases where there is a normal REDEFINE without FILLERs involved.

Read .claude/claude.md for instructions.

# Bugs
There are errors.
If you run the command:
cobol-analyzer paragraph-variables-map complex-cobol-source/ORDERMGMT.cbl -c complex-cobol-source/copybooks -o output-x

you will see the following errors
## Error 1
The output json has this snippet
      "CUSTOMER-STATUS": {
        "base_record": "ORDER-BUFFER",
        "defined_in_record": "FILLER",
        "explanation": "direct modification: MOVE at line 24",
        "position": {
          "end": 61,
          "start": 61
        }
      },

the property "defined_in_record" should contain "FILLER (CUSTINFO copybook)" and not simply "FILLER".

## Error 2
The output json has this snippet
      "CONTRACT-DATE": {
        "base_record": "ORDER-BUFFER",
        "defined_in_record": "FILLER (COPYBOOK copybook)",
        "explanation": "direct modification: MOVE at line 30",
        "position": {
          "end": 81,
          "start": 74
        }
      },

the property "defined_in_record" should contain "FILLER (CONTRACT copybook)" and not "FILLER (COPYBOOK copybook)".

## Error 3
The output json has many occurrences of this snippet
      "ORDER-BUFFER": {
        "77-level-var": true,
        "base_record": "ORDER-BUFFER",
        "defined_in_record": "ORDER-BUFFER",
        "explanation": "ORDER-BUFFER occupies positions 1-30000 in record ORDER-BUFFER which REDEFINEs FILLER$1 where CUSTOMER-ID (modified at line 27) occupies positions 1-10"
      }

"ORDER-BUFFER" is not a level-77 variable so the property "77-level-var" should not be in the object.

Fix these bugs

# Refinement of requirement
The "position" property should be added to all objects, not only to the objjects that derive from a FILLER REDEFINE patterns.
For instance, if you run the command 
cobol-analyzer paragraph-variables-map complex-cobol-source/TRANPROC.cbl -c complex-cobol-source/copybooks -o output-y

You see that the objects in the output json do not have the "position".
This is due probably to the fact that the original requirement said not to touch the behavior in cases of normal REDEFINE without FILLER.
But now I have changed the requirement and all objects must have the "position" property correctly calculated.
Please update the implementation and also the strategy and plan document.
When you update the strategy and plan document do not say that the requirement has changed, but assume that the new requirement was also the original requirement.