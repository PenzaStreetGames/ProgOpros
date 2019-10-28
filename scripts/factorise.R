factoring_cols <- c("gender", "status", "processor", "microboard", "destop_os", "mobile_os",       
                    "editor_theme", "cycle_recursion", "cycle", "java_kotlin", "zero_division", "indexing",     
                    "typing", "slow_python", "list_mutable", "sugar", "list_expressions", "ternar_module",  
                    "patterns", "mobile_desctop", "web", "back_front_end", "flask_django", "python",         
                    "cpp", "javascript", "pascal", "csharp", "java", "c",             
                    "php", "kotlin", "lua", "scratch", "basic", "go",           
                    "ruby", "fasm", "bf", "haskel", "pycharm", "vscode",      
                    "idle", "notepad", "notepadpp", "wing", "sublime", "jupiter",     
                    "atom", "console", "machine_learning", "big_data", "metaprog", "quantum",     
                    "cryptography", "math")
colnames(opros)
for (string in factoring_cols) {
  opros[[string]] <- factor(opros[[string]])
}
quality_vars <- factoring_cols
numeric_vars <- c("languages_number", "editors_number", "future_number")
