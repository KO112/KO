To-Do List:

- Check if client can run on own computer, check address of client on server, check if running server from own computer works
- Add hash to GLM Helper (based on sorted formula?)
- Create sample data, names from mtcars
- Filter/select model matrix (e.g. partial column matching, etc.
- replace_missing: fix for factors (relevel?)
- Select function to match columns, case/columns insensitive
- Head/tail assignment (`head<-`, `tail<-`)
- Allow select.matrix to accept variables (e.g. `a <- c("mpg", "cyl"); select(as.matrix(mtcars), a)`)
  - Maybe use `with = F` or just search up if not found, or implement `..a`
- Build file/function calling map
- compare_dfs: print message if identical, or all.equal, or no differences
  - Don't just return empty list
- Pretty JSON printing (takes in CSV file, aligns values, adds colors(?))

XGBoost Output Analysis:
  - Feature importance graphs
  - Feature interaction analysis

Variable EDA:
  - Distribution based on type
  - Histogram, plot, smoothed plot
  - Quantiles/averages

Add-In:
- Printing selected variable
- Showing option values (what does this even mean?)
- Make DLL specifically for activating/moving tcl/tk window, maybe faster?
  - Or load once and activate somehow?
- Add end bracket at next comma
  - Extend to quotes & other characters as well
  - Use general function, parameterize later
- Pop-up to select column names
  - Maybe full preview popup, (semi-)random sampling of rows?
  - Capitalize previous word (loop through upper/lower/title case)
- Move left/right until you hit a comma/bracket/etc.
- Should Shift+Backspace check for pipe too?
- Clean code formatting (treat comments differently, run on current line if nothing is selected)
  - Space around operators (==, !=, ~, %>%, ^, +, -, *, /, =, <-, |, &, etc.)
  - Remove space at end of line
  - Space after if/else/for

Data Dictionary:
- Make class for single column summary, and a list of them(?)
- For printing, change from tibble to something easier to read in bulk, maybe multiple columns
- Test new values in dataDict
- Add colHash/colMode, maybe use constructor for all col(...)
- Progress bar (progress::progress_bar)
- .DollarNames


Tests:
- plotly tests for BDP
- Finish compare_df testing (test with dates, one col date one character, messages, etc.)
- Test colored output (info + color_list)
- Test all add-in functions (specifically text removal, send to console)
