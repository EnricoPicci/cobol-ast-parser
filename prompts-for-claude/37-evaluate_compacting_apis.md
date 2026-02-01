You are a Python maverick.
You know very well how Cobol works.
You know also ast parsers and the way they work with Cobol.

In this project there are 2 main apis:
- get_data_division_tree which returns an instance of class DataDivisionTree
- analyze_paragraph_variables which returns an object with the property "variable_index" which is a Dictionare where:
    - key is the name of a variable whise value may change during the execution of the program
    - value is an object that nested contains an array of the PARAGRAPHs or SECTIONs where there are instructions which can change the value of the variable set as key

Currently DataDivisionTree and "variable_index" are calculated separately. Which probably means that the ast tree is calculated twice.

Your task is to evaluate if it is possible to create the exact same output in a more efficient way, with one new api that builds the ast tree once and then use it to create both output.

Provide your opinion with a summary of the reasons that bring you to the result.

# Answer
Based on this thorough analysis, I can now provide my evaluation.                                                                
                                                                                                                                   
  ---                                                                                                                              
  Evaluation: Combining APIs for Efficiency                                                                                        
                                                                                                                                   
  Current Situation                                                                                                                
                                                                                                                                   
  Both APIs share an identical pipeline up to the AST construction:                                                                
                                                                                                                                   
  Source File                                                                                                                      
      ↓                                                                                                                            
  Read & Detect Format → Resolve COPY → Normalize → Parse (ANTLR4) → Build AST                                                     
      ↓                                                                   ↓                                                        
                              [DIVERGENCE POINT]                                                                                   
                                      ↓                                                                                            
  ┌───────────────────────────────────┴────────────────────────────────────┐                                                       
  │                                                                        │                                                       
  ▼                                                                        ▼                                                       
  analyze_paragraph_variables()                    get_data_division_tree()                                                        
     - DataStructureAnalyzer                          - DataStructureAnalyzer ONLY                                                 
     - RedefinesAnalyzer                              - Transform to DataItemNode tree                                             
     - ProcedureAnalyzer                              - Group by sections                                                          
     - Generate analysis JSON                         - Return DataDivisionTree                                                    
     - Map to paragraph_variables                                                                                                  
     - Build variable_index                                                                                                        
     - Return AnalysisResult                                                                                                       
                                                                                                                                   
  When you call both APIs separately, you pay twice for:                                                                           
  - File read                                                                                                                      
  - Format detection                                                                                                               
  - COPY statement resolution                                                                                                      
  - Source normalization                                                                                                           
  - ANTLR4 parsing                                                                                                                 
  - AST construction                                                                                                               
  - DataStructureAnalyzer execution                                                                                                
                                                                                                                                   
  ---                                                                                                                              
  Verdict: YES, combining is feasible and beneficial                                                                               
                                                                                                                                   
  A unified API can produce both outputs from a single AST construction.                                                           
                                                                                                                                   
  ---                                                                                                                              
  How It Would Work                                                                                                                
                                                                                                                                   
  def get_full_analysis(                                                                                                           
      source_path: Path,                                                                                                           
      options: FullAnalysisOptions  # Combines both option sets                                                                    
  ) -> FullAnalysisResult:                                                                                                         
      """Single-pass API returning both DataDivisionTree and AnalysisResult."""                                                    
                                                                                                                                   
      # 1. Shared pipeline (ONCE)                                                                                                  
      source = read_file(source_path)                                                                                              
      format_type = detect_format(source)                                                                                          
      resolved, line_mapping = resolve_copy(source, copybook_paths)                                                                
      normalized = normalize_source(resolved)                                                                                      
      parse_tree = parse(normalized)                                                                                               
      ast = build_ast(parse_tree)                                                                                                  
                                                                                                                                   
      # 2. Run full analysis (includes DataStructureAnalyzer)                                                                      
      analyzer = ImpactAnalyzer(ast)                                                                                               
      analyzer.analyze()                                                                                                           
                                                                                                                                   
      # 3. Generate AnalysisResult outputs                                                                                         
      analysis_output = analyzer.generate_output()                                                                                 
      paragraph_variables = map_paragraph_variables(analysis_output)                                                               
      variable_index = build_variable_index(paragraph_variables)                                                                   
                                                                                                                                   
      # 4. Generate DataDivisionTree (reuse analyzer._memory_regions)                                                              
      data_tree = transform_to_data_division_tree(                                                                                 
          ast,                                                                                                                     
          analyzer._data_analyzer._memory_regions,  # REUSE!                                                                       
          line_mapping                                                                                                             
      )                                                                                                                            
                                                                                                                                   
      return FullAnalysisResult(                                                                                                   
          analysis_result=AnalysisResult(...),                                                                                     
          data_division_tree=data_tree                                                                                             
      )                                                                                                                            
                                                                                                                                   
  ---                                                                                                                              
  Key Reasons This Works                                                                                                           
                                                                                                                                   
  1. DataStructureAnalyzer is used by both - The memory region calculations from analyze_paragraph_variables() can be reused       
  directly for DataDivisionTree.position fields.                                                                                   
  2. The AST is identical - Both APIs transform the exact same CobolProgram AST. No additional parsing needed.                     
  3. variable_index already exists - The current AnalysisResult.variable_index was specifically designed to link DataItemNode      
  entries back to paragraphs. The fields defined_in_record and position.start:end form a unique key that works across both         
  structures.                                                                                                                      
  4. No conflicting requirements - The two outputs are complementary, not mutually exclusive. One is a flat paragraph-centric view,
   the other is a hierarchical data-centric view.                                                                                  
                                                                                                                                   
  ---                                                                                                                              
  Efficiency Gains                                                                                                                 
  ┌───────────────────────┬───────────────────┬───────────────────┐                                                                
  │       Operation       │ Current (2 calls) │ Combined (1 call) │                                                                
  ├───────────────────────┼───────────────────┼───────────────────┤                                                                
  │ File reads            │ 2                 │ 1                 │                                                                
  ├───────────────────────┼───────────────────┼───────────────────┤                                                                
  │ COPY resolution       │ 2                 │ 1                 │                                                                
  ├───────────────────────┼───────────────────┼───────────────────┤                                                                
  │ ANTLR parsing         │ 2                 │ 1                 │                                                                
  ├───────────────────────┼───────────────────┼───────────────────┤                                                                
  │ AST construction      │ 2                 │ 1                 │                                                                
  ├───────────────────────┼───────────────────┼───────────────────┤                                                                
  │ DataStructureAnalyzer │ 2                 │ 1                 │                                                                
  ├───────────────────────┼───────────────────┼───────────────────┤                                                                
  │ RedefinesAnalyzer     │ 1                 │ 1                 │                                                                
  ├───────────────────────┼───────────────────┼───────────────────┤                                                                
  │ ProcedureAnalyzer     │ 1                 │ 1                 │                                                                
  └───────────────────────┴───────────────────┴───────────────────┘                                                                
  ~40% reduction in computational work for the common use case of needing both outputs.                                            
                                                                                                                                   
  ---                                                                                                                              
  Recommended Implementation Approach                                                                                              
                                                                                                                                   
  1. Add a new function get_combined_analysis() that returns a new dataclass:                                                      
  @dataclass                                                                                                                       
  class CombinedAnalysisResult:                                                                                                    
      analysis: AnalysisResult                                                                                                     
      data_tree: DataDivisionTree                                                                                                  
  2. Keep existing APIs for backward compatibility - they can internally call the combined function if both outputs would be       
  needed, or remain separate for cases where only one is needed.                                                                   
  3. Share the analyzer state - The ImpactAnalyzer already contains _data_analyzer with _memory_regions. Expose this for reuse in  
  DataDivisionTree construction.                                                                                                   
                                                                                                                                   
  ---                                                                                                                              
  Summary                                                                                                                          
  ┌────────────────────────────┬────────────────────────────────────────┐                                                          
  │          Question          │                 Answer                 │                                                          
  ├────────────────────────────┼────────────────────────────────────────┤                                                          
  │ Is combining feasible?     │ Yes                                    │                                                          
  ├────────────────────────────┼────────────────────────────────────────┤                                                          
  │ Is output identical?       │ Yes - same data structures             │                                                          
  ├────────────────────────────┼────────────────────────────────────────┤                                                          
  │ Performance improvement?   │ Significant (~40% less work)           │                                                          
  ├────────────────────────────┼────────────────────────────────────────┤                                                          
  │ Breaking changes required? │ No - additive new API                  │                                                          
  ├────────────────────────────┼────────────────────────────────────────┤                                                          
  │ Complexity increase?       │ Low - mostly refactoring existing code │                                                          
  └────────────────────────────┴────────────────────────────────────────┘                                                          
  The current architecture was thoughtfully designed with variable_index specifically to enable linking between the two views. A   
  combined API is a natural evolution that the codebase is already prepared for.  

  # Refinement
  Plan the implementation of the combined API.

  Consider that to test if the implementation works you must do the following:
  - execute command "python3 kyco-input-data/run_data_division_tree.py" which runs the "get_data_division_tree" api on a complex Cobol program and saves the result (an instance of DataDivisionTree) on a data_division_tree.json file
  - execute command "python3 kyco-input-data/run_analyze_paragraph_variables.py" which runs the "analyze_paragraph_variables" api on the same complex Cobol program and saves "variable_index" on a *-variable_index.json file
  - run the new api
  - compare the instance of DataDivisionTree obtained by the "get_data_division_tree" api with the one obtained by the new api. they must be equal.
  - compare the "variable_index" obtained by the "analyze_paragraph_variables" api with the one obtained by the new api. they must be equal.
  This is a test to be made only once, at the of the implementation, to validate the result. It must not be included in the tests to be run via Pytest.