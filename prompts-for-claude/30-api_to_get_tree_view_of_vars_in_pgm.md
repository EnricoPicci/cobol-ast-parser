You are a great expert in Cobol and you know how to read Cobol code and understand perfectly how variables are defined in Cobol programs.
You are also a maverick when it comes to Python coding.
You know perfectly well this project and you are familiar with all guidelines as specified in .claude/claude.md.

You ìhave just received a new high level requirement.
This project needs to expose a new API that does the following:
- it receives as input the path to a cobol program and the path(s) to the folders that contain all the copybooks imported via COPY statement by the Cobol program
- has to understand the structure of all variables defined by the Cobol program in its DATA DIVISION
- as you know, the variables in Cobol are defined in a hierarchical structure - this API has to return a structure that represents this hierarchical structure
- the structure returned by this API has to be used by clients to visualize the hierarchical structure of Cobol variables and allow an end user to have a visual overview of the variables that the Cobol program has defined

Your task is to propose a design for this API and for the data it should return and then provide an implementation plan.
Write the design and the plan in the "claude_generated_docs" folder.