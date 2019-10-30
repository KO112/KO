test1 <- c(
  "orange %>% select(apple, banana, cherry)",
  "orange %>% select(apple, banana, ",
  "orange %>% select(apple, ",
  "orange %>% select(",
  "orange %>% ",
  "orange",
  ""
)

test2 <- c(
  "apple + banana + cherry",
  "apple + banana + ",
  "apple + banana",
  "apple + ",
  "apple",
  ""
)

test3 <- c(
  "apple() %>% berry() %>% cherry()",
  "apple() %>% berry() %>% ",
  "apple() %>% berry()",
  "apple() %>% ",
  "apple()",
  ""
)

test4 <- c(
  "apple(arg1, arg2) %>% berry()",
  "apple(arg1, arg2) %>% ",
  "apple(arg1, arg2)",
  "apple(arg1, ",
  "apple(",
  ""
)

test5 <- c(
  "a <- m %>% select()",
  "a <- m %>% ",
  "a <- m",
  "a <- ",
  "a",
  ""
)

test6 <- c(
  "m %>% select(apple,",
  "m %>% select(",
  "m %>% ",
  "m",
  ""
)


for (i in 1:6) {
  txt <- get(paste0("test", i))
  cat("\ntest", i, "\n", sep = "")
  for (j in seq_along(txt) %>% head(-1)) {
    res <- substr(txt[j], 1, find_pos(txt[j]))
    match <- res == txt[j + 1]
    ifelse(match, "#00FF00", "#FF0000") %>% crayon::make_style(.) %>% .("match:", match, "\n") %>% cat()
  }
}
