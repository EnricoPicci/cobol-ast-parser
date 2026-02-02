You are a Python maverick.
You know very well how Cobol works.
You know also ast parsers and the way they work with Cobol.

In this project there is the api "analyze_paragraph_variables" which returns an object with the property "variable_index" which is a Dictionare where:
    - key is the name of a variable whose value may change during the execution of the program
    - value is an object that nested contains an array of the PARAGRAPHs or SECTIONs where there are instructions which can change the value of the variable set as key

I want to extend and modify the content of "variable_index" so that it contains not only the PARAGRAPHs or SECTIONs where there are instructions which can change the value of the variable set as key, but also the PARAGRAPHs or SECTIONs where there are instructions which access the values of the variables (for instance all read operations).
The current array of PARAGRAPHs and SECTIONs must remain, maybe held at a property with a name that clearly indicates these are PARAGRAPHs or SECTIONs where the value of the varible may change.
A new array has to be added which contain all the PARAGRAPHs or SECTIONs that access one way or another the values of the variables.


# Test the implementation
  Consider that to test if the implementation works you must do the following:
  - execute command "python3 kyco-input-data/run_data_division_tree.py" which runs the "get_data_division_tree" api on a complex Cobol program and saves the result (an instance of DataDivisionTree) on a data_division_tree.json file
  - execute command "python3 kyco-input-data/run_analyze_paragraph_variables.py" which runs the "analyze_paragraph_variables" api on the same complex Cobol program and saves "variable_index" on a *-variable_index.json file
  - take all the variables that are the keys in *-variable_index.json and check whether ther is a corresponding ast node in data_division_tree.json through which the arrays with PARAGRAPHs and SECTIONs can be reached via this type of logic: 
  index[node.defined_in_record][f"{node.position.start}:{node.position.end}"]["paragraphs"]  
  
  This is a test to be made only once, at the of the implementation, to validate the result. It must not be included in the tests to be run via Pytest.