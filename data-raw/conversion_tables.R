
library(dplyr)

bar_to_char <- readr::read_csv(
  "data-raw/bar_to_character.csv",
  col_types = "icici"
)

codeword_to_char_1 <- readr::read_csv(
  "data-raw/codeword_to_character_1.csv",
  col_types = "ici"
)

codeword_to_char_2 <- readr::read_csv(
  "data-raw/codeword_to_character_2.csv",
  col_types = "ici"
) %>%
  dplyr::mutate(codeword = codeword + 1287L)

codeword_to_char <- dplyr::bind_rows(
  codeword_to_char_1,
  codeword_to_char_2
)

usethis::use_data(
  bar_to_char,
  codeword_to_char,
  internal = TRUE,
  overwrite = TRUE
)

