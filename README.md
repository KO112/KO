To-Do List:

- Create sample data, names from mtcars
- Add hash to GLM Helper (based on sorted formula?)
- Filter/select model matrix (e.g. partial column matching, etc.
- Make mode_stat ignore blanks
- Check if client can run on own computer, check address of client on server, check if running server from own computer works.
- plotly tests for BDP
- Add value option to replace_missing, fix for factors (relevel?)


# Calculate the stars for p-values
pvalue_stars <- function(p_values) {
  case_when(
    p_values <= 0.001 ~ "***",
    p_values <= 0.01 ~ "**",
    p_values <= 0.05 ~ "*",
    p_values <= 0.1 ~ ".",
    TRUE ~ "---"
  ) %>%
    KO::pad_vector(vec = ., alignment = "R") %>%
    return()
}
