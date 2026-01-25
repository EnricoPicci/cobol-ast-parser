You have developed this app following the strategy defined in "claude_generated_docs/cobol-analyzer-strategy.md".

The app has evolved and now generates a json file which which group for each PARAGRAPH and SECTION the operations that change variables.
For each operation, the json contains an object that has (among other things) the name of the variable changed by the operation and the list of "affected_variables" that, because of the Cobol REDEFINE mechanism, chage together with that variable.
The json is generated with a cli defined in "./src/main.py".

Now you need to add a step to the processing. The logic is this:
- first you produce the json that you already produce
- then you take an array of variable names that has been passed as input to the CLI command and generate a new json that for each variable name passed as input groups the PARAGRAPHs and SECTIONs where the variable is changed

Ideally I would like to have 2 different commands:
- one to perform what the CLI currently does
- another to perform the logic described above

Define a strategy to implement this new requirement and write this strategy into a new document under the "claude_generated_docs" folder.