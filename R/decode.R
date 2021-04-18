
bars_to_codewords <- function(barcode) {
  checkmate::check_string(barcode, pattern = "^[ADTF]{65,65}")

  bar_bits <- tibble::tibble(
    bar = 1:65,
    char = stringr::str_split(barcode, "")[[1]],
    asc = char %in% c("A", "F"),
    desc = char %in% c("D", "F")
  ) %>%
    dplyr::left_join(bar_to_char, by = "bar")

  chars <- dplyr::bind_rows(
    dplyr::select(bar_bits, char = desc_char, bit_num = desc_bit, bit = desc),
    dplyr::select(bar_bits, char = asc_char, bit_num = asc_bit, bit = asc)
  ) %>%
    dplyr::arrange(char, dplyr::desc(bit_num)) %>%
    dplyr::group_by(char) %>%
    dplyr::summarise(bits = list(c(bit))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      bin = list(binaryLogic::as.binary(bits, logic = TRUE)),
      bin_negate = list(!bin),
      bin_int = as.integer(bin),
      bin_negate_int = as.integer(bin_negate)
    ) %>%
    dplyr::ungroup()

  codeword_df <- chars %>%
    dplyr::left_join(
      dplyr::select(codeword_to_char, codeword_1 = codeword, base_10),
      by = c("bin_int" = "base_10")
    ) %>%
    dplyr::left_join(
      dplyr::select(codeword_to_char, codeword_2 = codeword, base_10),
      by = c("bin_negate_int" = "base_10")
    ) %>%
    dplyr::mutate(codeword = dplyr::coalesce(codeword_1, codeword_2)) %>%
    dplyr::mutate(
      codeword = dplyr::case_when(
        char == "J" ~ codeword / 2,
        char == "A" & codeword >= 659 ~ codeword - 659,
        TRUE ~ as.numeric(codeword)
      )
    )

  codeword_df
}

decode_codewords <- function(codewords, mailer_id_len = 6) {
  checkmate::assert_numeric(codewords)
  checkmate::assert_number(mailer_id_len)
  checkmate::assert(mailer_id_len %in% c(6, 9))

  bin_data <- Rmpfr::mpfr(0, precBits = 104)
  for (digit in codewords[1:9]) {
    bin_data <- bin_data * 1365 + digit
  }
  bin_data <- bin_data * 636 + codewords[10]

  bin_string <- Rmpfr::formatMpfr(bin_data, drop0trailing = TRUE)

  len <- stringr::str_length(bin_string)

  routing <- stringr::str_sub(bin_string, end = len - 18)
  other <- stringr::str_sub(bin_string, start = len - 17)


  routing_mpfr <- Rmpfr::mpfr(routing)
  barcode_2 <- as.integer(routing_mpfr %% 5)
  routing_mpfr <- (routing_mpfr - barcode_2) / 5
  barcode_1 <- as.integer(routing_mpfr %% 10)
  routing_mpfr <- (routing_mpfr - barcode_1) / 10

  if (routing_mpfr > 1000000000) {
    zip <- Rmpfr::formatMpfr(routing_mpfr - 1000000000 - 100000 - 1, drop0trailing = TRUE)
    zip <- stringr::str_pad(zip, width = 11, pad = "0")
  } else if (routing_mpfr > 100000) {
    zip <- Rmpfr::formatMpfr(routing_mpfr - 100000 - 1, drop0trailing = TRUE)
    zip <- stringr::str_pad(zip, width = 9, pad = "0")
  } else {
    zip <- Rmpfr::formatMpfr(routing_mpfr - 1, drop0trailing = TRUE)
    zip <- stringr::str_pad(zip, width = 5, pad = "0")
  }

  list(
    barcode_id = paste0(barcode_1, barcode_2),
    service_type = stringr::str_sub(other, end = 3),
    mailer_id = stringr::str_sub(other, start = 4, end = 3 + mailer_id_len),
    serial_num = stringr::str_sub(other, start = 4 + mailer_id_len),
    zip = zip
  )
}

#' Decode an encoded Intelligent Mail Barcode
#'
#' @param barcode The 65-character barcode to decode
#' @param mailer_id_len The length of the Mailer ID field (6 or 9 digits)
#'
#' @return A list containing five elements:
#' \itemize{
#' \item `barcode_id`: 2 digits (second digits is 0-4)
#' \item `service_type`: 3 digits
#' \item `mailer_id`: 6 or 9 digits
#' \item `serial_num`: 6 or 9 digits (6 if Mailer ID is 9 digits; 9 if Mailer ID is 6 digits)
#' \item `zip`: 5, 9, or 11 digits
#' }
#'
#' @examples
#' imb_decode("AADTFFDFTDADTAADAATFDTDDAAADDTDTTDAFADADDDTFFFDDTTTADFAAADFTDAADA")
#'
#' @export
imb_decode <- function(barcode, mailer_id_len = 6) {
  checkmate::check_string(barcode, pattern = "^[ADTF]{65,65}")
  checkmate::assert_number(mailer_id_len)
  checkmate::assert(mailer_id_len %in% c(6, 9))
  codewords <- bars_to_codewords(barcode)
  decode_codewords(codewords$codeword, mailer_id_len)
}

