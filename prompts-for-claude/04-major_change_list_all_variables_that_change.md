You have developed this app following the strategy defined in "claude_generated_docs/cobol-analyzer-strategy.md".

Now you have to make a potentially big change.
What I want to achive is the following:
- any time a variable value change I want to list which are the variables that could potentially chage
This means that REDEFINEs at levels lower than 01 should be considered.

For instance, consider MYVAR_10 which is at level 10, defined in MYLEVEL_05 at level 05 defined in MYLEVEL_01 at level 01.
There is a REDEFINE for MYLEVEL_05.
If MYVAR_10 changes its value, than I want to know which are the possible variables defined in the REDEFINE of level 05 that may have changed.

Tell me if this is possible and which strategy you would suggest.
Write the strategy in the "claude_generated_docs" folder.