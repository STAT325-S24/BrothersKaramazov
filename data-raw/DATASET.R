library(tidyverse)
library(gutenbergr)
library(stringr)
library(dplyr)

karamazov_data_original <- gutenberg_download(28054)

karamazov_data <- karamazov_data_original |> 
  filter(text != "")

BrothersKaramazov <- karamazov_data |>
  mutate(linenumber = row_number(),
         part = cumsum(
                  str_detect(text, regex("^PART [\\DIVXLC]+$", ignore_case = FALSE))
                  ),
         book = cumsum(
                  str_detect(text, regex("^Book [\\DIVXLC]+\\.", ignore_case = FALSE))
                  ),
         chapter = cumsum(
                  str_detect(text, regex("^Chapter [\\DIVXLC]+", ignore_case = FALSE))
                  )
         ) |>
  ungroup() |>
  group_by(book) |>
  mutate(min = min(chapter),
         book_chapter = chapter - min) |>
  select(-min)

usethis::use_data(BrothersKaramazov, overwrite = TRUE)
