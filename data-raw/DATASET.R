library(tidyverse)
library(gutenbergr)
library(stringr)
library(dplyr)
library(cleanNLP)
library(reticulate)

karamazov_data_original <- gutenberg_download(28054)
  
karamazov_data <- karamazov_data_original |> 
  mutate(paragraph = cumsum(text == "")) |>
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
  select(-min) |>
  ungroup() |>
  mutate(
    text = gsub("’", "'", text)
  ) |>
  select(gutenberg_id, text, part, book, chapter, book_chapter, paragraph, linenumber)


usethis::use_data(BrothersKaramazov, overwrite = TRUE)


py_config()
reticulate::import("cleannlp") 
cnlp_init_spacy(model_name = "en_core_web_sm")
AnnotatedBK <- cnlp_annotate(BrothersKaramazov)

usethis::use_data(AnnotatedBK, overwrite = TRUE)
