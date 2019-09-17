To-Do List:

- Check if client can run on own computer, check address of client on server, check if running server from own computer works
- Add hash to GLM Helper (based on sorted formula?)
- Create sample data, names from mtcars
- Filter/select model matrix (e.g. partial column matching, etc.
- plotly tests for BDP
- replace_missing: fix for factors (relevel?)
- Finish compare_df testing (test with dates, one col date one character, messages, etc.)
- Test colored output
- Select function to match columns, case/columns insensitive
- Head/tail assignment (`head<-`, `tail<-`)
- Allow select.matrix to accept variables (e.g. `a <- c("mpg", "cyl"); select(as.matrix(mtcars), a)`)
  - Maybe use `with = F` or just search up if not found, or implement `..a`
- 

Add-In:
- Printing selected variable
- Showing option values
- Make DLL specifically for activating/moving
- Add end bracket at next comma
  - Extend to quotes & other characters as well
  - Use general function, parameterize later
- Pop-up to select column names
  - Maybe full preview popup, (semi) random sampling of rows?

Data Dictionary:
- Make class for single column summary, and a list of them(?)
- For printing, change from tibble to something easier to read in bulk, maybe multiple columns
- Test new values in dataDict
- Add colHash/colMode, maybe use constructor for all col(...)
- Progress bar (progress::progress_bar)
- .DollarNames
