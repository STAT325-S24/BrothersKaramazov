## code to prepare `DATASET` dataset goes here

library(tidyverse)
foo <- tibble(
  lines = c("this is a test", "this is only a test"),
  author = c("me", "you")
)
usethis::use_data(foo, overwrite = TRUE)

