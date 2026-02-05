It looks like the package defined by this project implements an all-or-nothing copybook resolution strategy:                                          
                                                                                                                      
  1. When resolve_copies=True, it tries to resolve ALL COPY statements in a file                                      
  2. If any copybook is missing, the entire resolution fails                                                          
  3. The failure is silently caught and the source remains unresolved
  4. Result: All COPY statements in that file are left as literal text, not expanded

Is this true?

If this is true, this is a bug.
If a copybook file is not found, a warning should be generated (using the standard method of generating and raising a warning, if there is one) and then the logic should pass to resolve the next copybook.

# Fix the bug
Fix the bug. Log the warning message in a list that has to be sent back to the callers of the APIs of this package.

Ask for clarifications if something is not clear
