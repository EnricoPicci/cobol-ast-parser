You are a very knowledgeable expert of Cobol, with 30 years experience.
You have to suggest a strategy for the follwing problem.

The api "analyze_paragraph_variables" returns the following json:
- result.analysis
- result.paragraph-variables

The api "get_data_division_tree" returns an instance of DataDivisionTree.

Both result.paragraph-variables and DataDivisionTree contain objects that describe single fields defined in a Cobol program.

For instance, result.analysis can contain the following object:
      "CLIENTS": {
        "base_record": "AREA-KCLI",
        "defined_in_record": "AREA-KCLI",
        "position": {
          "start": 80,
          "end": 91
        },
        "explanation": "direct modification: MOVE at line 141"
      },
which describes a field named "CLIENTS" which starts at position 80 and ends at position 91 of the record "AREA-KCLI".
You can get a result.analysis with the value above by running the command "python3 kyco-input-data/run_analyze_paragraph_variables.py".
In this case you will find 2 instances representing the same variable in result.analysis:
- one is in the dictionary found at key "MANAGE-CLI" of the dictionary held by property "paragraphs"
- one is in the dictionary found at key "CLI-VALUE-123" of the dictionary held by property "paragraphs"
These 2 instances mean that the value of this field can be changed by operations contained in the paragraphs:
- "MANAGE-CLI"
- "CLI-VALUE-123"

The same field in DataDivisionTree is represented by the object
                {
                  "name": "CLIENTS",
                  "level": 7,
                  "picture": "S9(09)V9(03)",
                  "line_number": 464,
                  "copybook_source": "KCLI123",
                  "position": {
                    "start": 80,
                    "end": 91,
                    "size": 12
                  }
                }

You can get a DataDivisionTree with the value above by running the command "python3 kyco-input-data/run_data_division_tree.py".

Imagine a client of these 2 APIs calls these APIs sequentially.
Once it holds the instance of result.analysis and the intance of DataDivisionTree returned by the 2 APIs, he wants to be able to select somehow a variable in DataDivionTree and, from there, retrieve the names of the paragraphs in which the value of that variable may be changed.

Suggest a strategy to achieve this.

# Answer
Based on my exploration of the codebase, here's my recommended strategy:                                        
                                                                                                                  
  Strategy for Linking DataDivisionTree Variables to Paragraph Modifications                                      
                                                                                                                  
  The Core Problem                                                                                                
                                                                                                                  
  Both APIs represent the same variables differently:                                                             
  - DataDivisionTree: Hierarchical structure with full COBOL attributes                                           
  - paragraph_variables: Flat mapping of modifications per paragraph                                              
                                                                                                                  
  Recommended Unique Key                                                                                          
                                                                                                                  
  Composite Key: (base_record, position.start, position.end)                                                      
                                                                                                                  
  This is the most reliable linking strategy because:                                                             
                                                                                                                  
  1. Position is immutable - Memory positions are calculated the same way by both APIs                            
  2. base_record resolves REDEFINES - Both APIs track the "ultimate" Level 01 record                              
  3. Handles name collisions - Same variable names in different records are disambiguated                         
                                                                                                                  
  Implementation Approach                                                                                         
                                                                                                                  
  ┌─────────────────────────────────────┐                                                                         
  │  Client Application                 │                                                                         
  ├─────────────────────────────────────┤                                                                         
  │                                     │                                                                         
  │  1. Call get_data_division_tree()   │                                                                         
  │     → Build index by (record,       │                                                                         
  │        start_pos, end_pos)          │                                                                         
  │                                     │                                                                         
  │  2. Call analyze_paragraph_vars()   │                                                                         
  │     → Each variable has base_record │                                                                         
  │        and position                 │                                                                         
  │                                     │                                                                         
  │  3. Reverse lookup:                 │                                                                         
  │     DataDivisionTree node           │                                                                         
  │       → extract (record, pos)       │                                                                         
  │       → search paragraph_variables  │                                                                         
  │       → return list of paragraphs   │                                                                         
  │                                     │                                                                         
  └─────────────────────────────────────┘                                                                         
                                                                                                                  
  Concrete Steps                                                                                                  
                                                                                                                  
  Step 1: Build a modification index from paragraph_variables                                                     
  # Structure: {(base_record, start, end): [paragraph_names]}                                                     
  modification_index = {}                                                                                         
                                                                                                                  
  for para_name, variables in result.paragraph_variables['paragraphs'].items():                                   
      for var_name, var_info in variables.items():                                                                
          key = (var_info['base_record'],                                                                         
                 var_info['position']['start'],                                                                   
                 var_info['position']['end'])                                                                     
          modification_index.setdefault(key, []).append(para_name)                                                
                                                                                                                  
  Step 2: Query from DataDivisionTree                                                                             
  def get_modifying_paragraphs(data_item_node, parent_record_name):                                               
      key = (parent_record_name,                                                                                  
             data_item_node.position['start'],                                                                    
             data_item_node.position['end'])                                                                      
      return modification_index.get(key, [])                                                                      
                                                                                                                  
  Why Not Use Variable Name Alone?                                                                                
                                                                                                                  
  COBOL allows the same variable name in different records:                                                       
  01 CUSTOMER-RECORD.                                                                                             
     05 ACCOUNT-ID  PIC X(10).                                                                                    
                                                                                                                  
  01 VENDOR-RECORD.                                                                                               
     05 ACCOUNT-ID  PIC X(10).  *> Same name, different variable!                                                 
                                                                                                                  
  Using position within record guarantees uniqueness.                                                             
                                                                                                                  
  Edge Cases to Handle                                                                                            
  ┌────────────────────┬──────────────────────────────────────────────────────────────────────┐                   
  │        Case        │                               Solution                               │                   
  ├────────────────────┼──────────────────────────────────────────────────────────────────────┤                   
  │ 77-level variables │ Use ("77-LEVEL", start, end) as key since they have no parent record │                   
  ├────────────────────┼──────────────────────────────────────────────────────────────────────┤                   
  │ FILLER items       │ Match by position only; name is synthetic                            │                   
  ├────────────────────┼──────────────────────────────────────────────────────────────────────┤                   
  │ REDEFINES chains   │ Both APIs use base_record to normalize to the root                   │                   
  ├────────────────────┼──────────────────────────────────────────────────────────────────────┤                   
  │ Copybook variables │ Position is calculated post-expansion, so it matches                 │                   
  └────────────────────┴──────────────────────────────────────────────────────────────────────┘                   
  Alternative: Add Linking Support to the API                                                                     
                                                                                                                  
  A cleaner long-term solution would be to add a helper method to the API:                                        
                                                                                                                  
  def get_variable_modifications(                                                                                 
      data_division_tree: DataDivisionTree,                                                                       
      analysis_result: AnalysisResult,                                                                            
      variable_name: str,                                                                                         
      parent_record: Optional[str] = None                                                                         
  ) -> Dict[str, List[str]]:                                                                                      
      """                                                                                                         
      Returns {variable_name: [paragraph_names]} for a variable                                                   
      selected from the DataDivisionTree.                                                                         
      """                                                                                                         
                                                                                                                  
  This would encapsulate the linking logic within the library.          

  # Refinement
  What about transforming result.paragraph-variables into a new object with the following structure:
  {(base_record, start, end, variable_name, defined_in_record): [paragraph_names]}  
  and return to the client this structure together with the DataDivisionTree.

  variable_name and defined_record would be added to the key only to make it easier to understand which is the variable during inspection

  # Answer
  Evaluation of the Proposed Structure                                                                            
                                                                                                                  
  Advantages                                                                                                      
                                                                                                                  
  1. Direct lookup - Client can go straight from variable → paragraphs (O(1) lookup)                              
  2. Self-documenting keys - Including variable_name and defined_in_record makes debugging and inspection much    
  easier                                                                                                          
  3. Minimal redundancy - The composite key uniquely identifies each variable while the value is just the list of 
  paragraphs                                                                                                      
  4. Clean separation - DataDivisionTree provides structure; this new object provides modification context        
                                                                                                                  
  Suggested Refinement                                                                                            
                                                                                                                  
  Since JSON doesn't support tuple keys, the structure would need to be either:                                   
                                                                                                                  
  Option A: Nested dict (recommended for JSON)                                                                    
  {                                                                                                               
    "KCLI123": {                                                                                            
      "80:91": {                                                                                              
        "variable_name": "CLI-VALUE-123",                                                               
        "defined_in_record": "KCLI123",                                                                     
        "paragraphs": ["MANAGE-CLI", "CLI-VALUE-123"]                                    
      }                                                                                                           
    }                                                                                                             
  }                                                                                                               
                                                                                                                  
  Option B: List of entries (flatter, easier to iterate)                                                          
  [                                                                                                               
    {                                                                                                             
      "base_record": "KCLI123",                                                                             
      "position": {"start": 80, "end": 91},                                                                   
      "variable_name": "CLI-VALUE-123",                                                                 
      "defined_in_record": "KCLI123",                                                                       
      "paragraphs": ["MANAGE-CLI", "CLI-VALUE-123"]                                      
    }                                                                                                             
  ]                                                                                                               
                                                                                                                  
  Option C: String key with separator                                                                             
  {                                                                                                               
    "KCLI123|80|91": {                                                                                  
      "variable_name": "CLI-VALUE-123",                                                                 
      "defined_in_record": "KCLI123",                                                                       
      "paragraphs": ["MANAGE-CLI", "CLI-VALUE-123"]                                      
    }                                                                                                             
  }                                                                                                               
                                                                                                                  
  My Recommendation                                                                                               
                                                                                                                  
  Option A (nested dict) is best because:                                                                         
  - Natural JSON structure                                                                                        
  - Efficient lookup: index[base_record][f"{start}:{end}"]                                                        
  - Easy to traverse by record                                                                                    
  - Client can build a flat index if needed                                                                       
                                                                                                                  
  API Design                                                                                                      
                                                                                                                  
  def get_variable_to_paragraphs_index(                                                                           
      source_path: Path,                                                                                          
      options: AnalysisOptions = None                                                                             
  ) -> Tuple[DataDivisionTree, Dict]:                                                                             
      """                                                                                                         
      Returns:                                                                                                    
          - DataDivisionTree: Full data structure hierarchy                                                       
          - variable_index: {base_record: {start:end: {variable_name, defined_in_record, paragraphs}}}            
      """                                                                                                         
                                                                                                                  
  Or add it as a new field in the existing AnalysisResult:                                                        
  @dataclass                                                                                                      
  class AnalysisResult:                                                                                           
      analysis: Dict                                                                                              
      paragraph_variables: Dict                                                                                   
      variable_index: Dict  # NEW: inverted index for lookup      

# Refinement
The requirement is a bit more strict.
Assume that there is a level 01 (base record) names CLIENT with field NAME starting at 5 long 10.
Then there is another level 01 name CLIENT-CHINA which has field MIDDLE-NAME starting at 5 long 10.
In DataTreeStructure the client may select NAME of CLIENT and he wants to see only the paragraphs where NAME of CLIENT may change.
It is understood that, if NAME of CLIENT changes, also MIDDLE-NAME of CLIENT-CHINA changes, but this is not interesting for the client.
He wants to know only the paragraphs where a certain field may be changed explicitely.
In this case, what do you suggest?


# Answer
I understand now. This is an important distinction:                                                             
                                                                                                                  
  The Problem                                                                                                     
                                                                                                                  
  Memory Layout (shared via REDEFINES):                                                                           
  Position:  1    5         15                                                                                    
             |    |---------|                                                                                     
                                                                                                                  
  01 CLIENT.                          ← defined_in_record                                                         
     05 ID        PIC X(4).                                                                                       
     05 NAME      PIC X(10).          ← position 5-14                                                             
                                                                                                                  
  01 CLIENT-CHINA REDEFINES CLIENT.   ← defined_in_record                                                         
     05 CODE      PIC X(4).                                                                                       
     05 MIDDLE-NAME PIC X(10).        ← position 5-14 (same memory!)                                              
                                                                                                                  
  Both share base_record = CLIENT and position = (5, 14), but they are different variables from the user's        
  perspective.                                                                                                    
                                                                                                                  
  Solution: Use defined_in_record as the Discriminator                                                            
                                                                                                                  
  The unique key must be:                                                                                         
                                                                                                                  
  (defined_in_record, start, end)                                                                                 
                                                                                                                  
  NOT (base_record, start, end)                                                                                   
                                                                                                                  
  This way:                                                                                                       
  - ("CLIENT", 5, 14) → paragraphs modifying NAME                                                                 
  - ("CLIENT-CHINA", 5, 14) → paragraphs modifying MIDDLE-NAME                                                    
                                                                                                                  
  Revised Structure                                                                                               
                                                                                                                  
  {                                                                                                               
    "CLIENT": {                                                                                                   
      "5:14": {                                                                                                   
        "variable_name": "NAME",                                                                                  
        "base_record": "CLIENT",                                                                                  
        "paragraphs": ["UPDATE-CLIENT-NAME", "INIT-CLIENT"]                                                       
      }                                                                                                           
    },                                                                                                            
    "CLIENT-CHINA": {                                                                                             
      "5:14": {                                                                                                   
        "variable_name": "MIDDLE-NAME",                                                                           
        "base_record": "CLIENT",                                                                                  
        "paragraphs": ["PROCESS-CHINA-DATA"]                                                                      
      }                                                                                                           
    }                                                                                                             
  }                                                                                                               
                                                                                                                  
  Client Lookup Flow                                                                                              
                                                                                                                  
  DataDivisionTree node selected:                                                                                 
    - name: "NAME"                                                                                                
    - position: {start: 5, end: 14}                                                                               
    - parent record: "CLIENT"        ← This is the defined_in_record                                              
                                                                                                                  
  Lookup key: ("CLIENT", "5:14")                                                                                  
  Result: ["UPDATE-CLIENT-NAME", "INIT-CLIENT"]                                                                   
                                                                                                                  
  Key Insight                                                                                                     
  ┌───────────────────┬───────────────────────────────────────────────────────────────────────────────────┐       
  │       Field       │                                      Purpose                                      │       
  ├───────────────────┼───────────────────────────────────────────────────────────────────────────────────┤       
  │ defined_in_record │ Uniqueness - which record defines this variable                                   │       
  ├───────────────────┼───────────────────────────────────────────────────────────────────────────────────┤       
  │ base_record       │ Informational - root of REDEFINES chain (useful for understanding memory overlap) │       
  ├───────────────────┼───────────────────────────────────────────────────────────────────────────────────┤       
  │ position          │ Uniqueness - location within the record                                           │       
  ├───────────────────┼───────────────────────────────────────────────────────────────────────────────────┤       
  │ variable_name     │ Informational - human readability                                                 │       
  └───────────────────┴───────────────────────────────────────────────────────────────────────────────────┘       
  The client gets explicit modifications only - not the side effects through REDEFINES.        


# Refinement
But the nodes of DataDivisionTree do not have "defined_in_record" property.
If I select a node of DataDivisionTree how do I find the "defined_in_record" info?

# Answer
You're right - DataItemNode has hierarchical structure but no direct defined_in_record field.  
                                                                                                                  
  Current DataItemNode Structure                                                                                  
                                                                                                                  
  DataItemNode:                                                                                                   
    - name                                                                                                        
    - level                                                                                                       
    - position                                                                                                    
    - children: [DataItemNode...]  # nested structure                                                             
    # NO parent reference, NO defined_in_record                                                                   
                                                                                                                  
  Options to Solve This                                                                                           
                                                                                                                  
  Option 1: Add defined_in_record to Each DataItemNode                                                            
                                                                                                                  
  Modify get_data_division_tree to populate this field during tree construction:                                  
                                                                                                                  
  DataItemNode:                                                                                                   
    - name                                                                                                        
    - level                                                                                                       
    - position                                                                                                    
    - defined_in_record: str  # ← NEW: name of the Level 01 ancestor                                              
    - children: [...]                                                                                             
                                                                                                                  
  Pros: Direct lookup, no client-side traversal needed                                                            
  Cons: Requires modifying the DataDivisionTree output                                                            
                                                                                                                  
  Option 2: Client Traverses to Find Level 01 Ancestor                                                            
                                                                                                                  
  The client must walk up the tree to find the Level 01 parent. But currently DataItemNode has no parent pointer -
   it's a tree with only downward references.                                                                     
                                                                                                                  
  Cons: Client must maintain parent mapping during traversal, cumbersome                                          
                                                                                                                  
  Option 3: Flatten the Index Key Using Variable Name                                                             
                                                                                                                  
  Since within a single defined_in_record, variable names are unique, we could use:                               
                                                                                                                  
  Key: (defined_in_record, variable_name)                                                                         
                                                                                                                  
  And in DataDivisionTree, the client knows which Level 01 record they're browsing (it's the root of the subtree  
  they're in).                                                                                                    
                                                                                                                  
  ---                                                                                                             
  My Recommendation: Option 1                                                                                     
                                                                                                                  
  Add defined_in_record to each DataItemNode. This is the cleanest solution because:                              
                                                                                                                  
  1. Self-contained - Each node carries its own context                                                           
  2. Direct match - Same field exists in both structures                                                          
  3. Simple client code - No tree traversal needed                                                                
                                                                                                                  
  # In DataDivisionTree output                                                                                    
  {                                                                                                               
    "name": "CLI-VALUE-123",                                                                            
    "level": 7,                                                                                                   
    "position": {"start": 80, "end": 91},                                                                     
    "defined_in_record": "KCLI123",  # ← NEW                                                                
    ...                                                                                                           
  }                                                                                                               
                                                                                                                  
  # In variable-to-paragraphs index                                                                               
  {                                                                                                               
    "KCLI123": {                                                                                            
      "80:91": {                                                                                              
        "variable_name": "CLI-VALUE-123",                                                               
        "paragraphs": [...]                                                                                       
      }                                                                                                           
    }                                                                                                             
  }                                                                                                               
                                                                                                                  
  Client lookup becomes trivial:                                                                                  
  node = selected_data_item_node                                                                                  
  paragraphs = index[node.defined_in_record][f"{node.position.start}:{node.position.end}"]["paragraphs"]   

  # Refinement
  Now you have to adjust the api "analyze_paragraph_variables" so that it returns the "variable_index", as discussed before, compatible with the client lookup strategy defined:
  Client lookup becomes trivial:                                                                                  
  node = selected_data_item_node                                                                                  
  paragraphs = index[node.defined_in_record][f"{node.position.start}:{node.position.end}"]["paragraphs"] 

  # Refinement
  variable_index has some keys derived from fillers that have the format like these:
  - FILLER (COPYBOOK copybook)
  - FILLER (WS-DATE)
  This format does not work since DataDivisionTree has the format "defined_in_record": "FILLER$xyz".

  Change the implementation of the logic that calculates variable_index so that the keys are in the format "FILLER$xyz".
  Remove the logic that changes the format "FILLER$xyz" from anywhere in the api "analyze_paragraph_variables".