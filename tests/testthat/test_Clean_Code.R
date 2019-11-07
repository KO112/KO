# # context("Clean Code")
# # 
# # 
# # # Test clean_code
# # test_that("clean_code", {
# #   expect_equal(clean_code(),
# #                )
# # })
# 
# 
# source("./R/Clean_Code.R")
# 
# 
# old <- c(
#   "a=5", "a =5", "a= 5"
#   , "b<-5", "b <-5", "b<- 5"
#   , "c==5", "c ==5", "c== 5"
#   , "  d=5", "   d =5", "    d= 5"
#   , "-2"
#   , "e|f", "e&f"
#   , "10+20", "10-20", "10*2", "10/20", "20^10"
#   , "apple=5;banana<-1;cherry==100;1+2;10+20;1-2;1*2;1/2;1^2;1|2;1&2"
#   , "mean(1,2,3, 4)"
#   , "  spaces after  "
#   , "mtcars%>%select()", "mtcars%>%", "%>%mtcars", "%>%"
#   , "#apple", "hello #world", "##apple", "hello##world", "# apple", "#  apple"
#   , "if(TRUE)", "else{TRUE}", "for(i in 1:10)", "while(TRUE)", "repeat{break}"
#   , "if (credit_adjustment_check){"
#   , "ifelse (TRUE, 1, 2)"
# )
# 
# new <- c(
#   "a = 5", "a = 5", "a = 5"
#   , "b <- 5", "b <- 5", "b <- 5"
#   , "c == 5", "c == 5", "c == 5"
#   , "  d = 5", "   d = 5", "    d = 5"
#   , "-2"
#   , "e | f", "e & f"
#   , "10 + 20", "10 - 20", "10 * 2", "10 / 20", "20 ^ 10"
#   , "apple = 5; banana <- 1; cherry == 100; 1 + 2; 10 + 20; 1 - 2; 1 * 2; 1 / 2; 1 ^ 2; 1 | 2; 1 & 2"
#   , "mean(1, 2, 3, 4)"
#   , "  spaces after"
#   , "mtcars %>% select()", "mtcars %>%", "%>% mtcars", "%>%"
#   , "# apple", "hello # world", "## apple", "hello ## world", "# apple", "# apple"
#   , "if (TRUE)", "else {TRUE}", "for (i in 1:10)", "while (TRUE)", "repeat {break}"
#   , "if (credit_adjustment_check) {"
#   , "ifelse(TRUE, 1, 2)"
# )
# 
# # expect_equal(clean_code(old), new)
# # purrr::walk2(old, new, ~ testthat::expect_equal(clean_code(.x), .y))
# 
# # purrr::walk2(old, new, ~ cat("'", .x, "'\t'", .y, "'\t'", clean_code(.x), "'\t", (clean_code(.x) == .y) %>% {
# #     match <- .; ifelse(match, "#00FF00", "#FF0000") %>% crayon::make_style(.) %>% .(match)
# #   }, "\n", sep = "")
# # )
# 
# purrr::walk2(old, new, function(.x, .y) {
#   
#   match <- clean_code(.x) == .y
#   if (!match) cat(
#     "Old:\t'", .x, "'\nNew:\t'", .y, "'\nActual:\t'", clean_code(.x), "'\nMatch:\t",
#     ifelse(match, "#00FF00", "#FF0000") %>% crayon::make_style(.) %>% .(match)
#     , "\n\n", sep = ""
#   )
#   
# })
