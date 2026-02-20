If you launch the command "python3 sample-input-data/run_analyze_with_tree.py" you launch the api "analyze_with_tree" and produce 2 json files that represent:
- *-data-division-tree.json: the structure of the variables defined in the DATA DIVISION of the Cobol program
- *-variable-index.json: a dictionary where keys are the variable names of all variables thar can be either accessed or modifies within the program

The issue is that it seems that some fields are actually accessed or modified are not present in *-variable-index.json

For instance, the field FLAG-AUTO (a level 77) is actually modified by the instruction:
           MOVE SPACES            TO  FLAG-PC                        
                                      FLAG-AUTO                      
                                      FLAG-TA.   

but FLG-MOTOR is not present in *-variable-index.json.

Explain why and plan a way to fix it without breaking anything.