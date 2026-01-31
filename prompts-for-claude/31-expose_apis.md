I want to use the APIs defined in this project as APIs exposed by a package, where the package is this project.
Therefore 
- either change "setup.py" adding:                                                                                           
  py_modules=["api", "main"],  # Include standalone modules                                                                 
                                                                                                                    - or move api.py inside the cobol_ast/ package directory.    

                                                                                                                    First, before changing anything, tell me which is the more Pythonic way
                                                                                                                    