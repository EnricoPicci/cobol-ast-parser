# Role
You are an expert Cobol programmer with 30 years of experience and you know very well how Cobol programs are structured and how Cobol source code is structured.

# Problem description
I need to analyze Cobol source code and find some information about it.

**Cobol code structure**
Cobol code source files have the following characteristics that are interesting for my problem:
- there can be "COPY" statements that embed into the source code the content of other source files - in particular there are:
    - "COPY" that import data structure definitions
    - procedural "COPY" that import in the PROCEDURAL DIVISION Cobol programmatical logic
- the "REDEFINE" statement allows Cobol programs to interpret the same memory area with different overlaying structures
- The logic is often split in SECTIONs or PARAGRAPHs

**The Goal**
I want to identify for each PARAGRAPH or SECTION:
- which are the variables that may change value, for instance because of a "MOVE" statement and, for each variable:
    - the record description (i.e. the level 1 in the DATA DIVISION) this variable is defined
The logic has to consider the REDEFINE statements. This means that if a variable MYVAR is defined in the record description ABC but there is a REDEFINE that says that the same memory area can also be interpreted with the record description XYZ, changing the value of MYVAR affects both ABC and XYZ.
The result must be something like a dictionary with these characteristics:
- the key is the PARAGRAPH or SECTION name
- the value is a data structure that contains:
    - tha name of the variable
    - a list of all level 1 record descriptions that change because of the change of the value of the variable
The analysis has to be preceded by a step of resolution of all the "COPY" statements.

# Your task
Your task is to suggest the best strategy to achieve this goal.
Save this strategy in a markdown document in a folder called claude_generated_docs

=====================================================================================

# 1 Refinement
Before starting the implementation, write a "claude.md" file with the following logical sections:
- general python best practices and guidelines to follow to make sure that the generated code is pythonic
- project specific guidelines

After this step, clear context and proceed with the implementation of the strategy defined bypassing permissions