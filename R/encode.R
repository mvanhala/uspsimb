
parse_input <- function(barcode_id, service_type, mailer_id, serial_num, zip) {
  checkmate::assert_scalar(barcode_id)
  checkmate::assert_scalar(service_type)
  checkmate::assert_scalar(mailer_id)
  checkmate::assert_scalar(serial_num)
  checkmate::assert_scalar(zip)

  barcode_id <- stringr::str_pad(barcode_id, width = 2, pad = "0")
  service_type <- stringr::str_pad(service_type, width = 2, pad = "0")
  mailer_id <- stringr::str_pad(mailer_id, width = 2, pad = "0")
  serial_num <- stringr::str_pad(serial_num, width = 2, pad = "0")
  zip <- stringr::str_pad(zip, width = 2, pad = "0")

  checkmate::assert_string(barcode_id, pattern = "^[0-9]{2,2}$")
  checkmate::assert_string(service_type, pattern = "^[0-9]{3,3}$")
  checkmate::assert_string(mailer_id, pattern = "^[0-9]{6,9}$")
  checkmate::assert_string(serial_num, pattern = "^[0-9]{6,9}$")
  checkmate::assert_string(zip, pattern = "^[0-9]{5,11}$")
  checkmate::assert(stringr::str_length(mailer_id) + stringr::str_length(serial_num) == 15)
  checkmate::assert(stringr::str_length(zip) %in% c(5, 9, 11))

  list(
    barcode_id = barcode_id,
    service_type = service_type,
    mailer_id = mailer_id,
    serial_num = serial_num,
    zip = zip
  )
}

create_payload <- function(barcode_id, service_type, mailer_id, serial_num, zip) {
  barcode_1 <- as.integer(stringr::str_sub(barcode_id, 1, 1))
  barcode_2 <- as.integer(stringr::str_sub(barcode_id, 2, 2))

  routing_code <- as.integer(zip)
  if (stringr::str_length(zip) == 5) {
    routing_code <- routing_code + 1
  } else if (stringr::str_length(zip) == 9) {
    routing_code <- routing_code + 100000 + 1
  } else if (stringr::str_length(zip) == 11) {
    routing_code <- routing_code + 1000000000 + 100000 + 1
  }

  start <- (routing_code * 10 + barcode_1) * 5 + barcode_2
  payload <- paste0(start, service_type, mailer_id, serial_num)

  Rmpfr::mpfr(payload)
}

payload_to_codewords <- function(payload) {
  codewords <- numeric(10)
  codewords[10] <- payload %% 636
  remain <- payload %/% 636
  for (i in 9:1) {
    codewords[i] <- remain %% 1365
    remain <- remain %/% 1365
  }
  purrr::map_int(codewords, ~as.integer(as(., "mpfr")))
}

to_bits <- function(num) {
  if (num != 0) return(c(to_bits(num %/% 2), num %% 2)) else return(num)
}

crc_fcs <- function(num) {
  control_1 <- binaryLogic::as.binary(0x400, n = 16)
  control_2 <- binaryLogic::as.binary(0x7FF, n = 16)
  generator_polynomial <- binaryLogic::as.binary(0x0F35, n = 16)
  frame_check_sequence <- control_2

  data <- binaryLogic::shiftLeft(binaryLogic::as.binary(num[1:8], n = 16), 5)
  for (bit in 3:8) {
    if (as.integer(binaryLogic::as.binary(xor(frame_check_sequence, data) & control_1, logic = TRUE))) {
      frame_check_sequence <- xor(binaryLogic::shiftLeft(frame_check_sequence, 1), generator_polynomial)
    } else {
      frame_check_sequence <- binaryLogic::shiftLeft(frame_check_sequence, 1)
    }
    frame_check_sequence <- frame_check_sequence & control_2
    data <- binaryLogic::shiftLeft(data, 1)
  }

  for (byte_index in 2:13) {
    data <- binaryLogic::shiftLeft(binaryLogic::as.binary(num[8 * (byte_index - 1) + 1:8], n = 16), 3)
    for (bit in 1:8) {
      if (as.integer(binaryLogic::as.binary(xor(frame_check_sequence, data) & control_1, logic = TRUE))) {
        frame_check_sequence <- xor(binaryLogic::shiftLeft(frame_check_sequence, 1), generator_polynomial)
      } else {
        frame_check_sequence <- binaryLogic::shiftLeft(frame_check_sequence, 1)
      }
      frame_check_sequence <- frame_check_sequence & control_2
      data <- binaryLogic::shiftLeft(data, 1)
    }
  }

  as.integer(binaryLogic::as.binary(frame_check_sequence, logic = TRUE))
}

modify_codewords <- function(codewords, fcs) {
  codewords[10] <- codewords[10] * 2
  if (fcs[1]) codewords[1] <- codewords[1] + 659
  codewords
}

codewords_to_barcode <- function(codewords, fcs) {
  codeword_chars <- tibble::tibble(
    codeword = codewords,
    fcs_digit = as.logical(fcs[11:2])
  ) %>%
    dplyr::left_join(codeword_to_char, by = "codeword") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      char_base_2 = list(
        binaryLogic::as.binary(
          as.numeric(stringr::str_split(base_2, "")[[1]]),
          logic = TRUE
        )
      ),
      char_base_2_fcs = list(if (fcs_digit) !char_base_2 else char_base_2),
      chars = list(as.raw(char_base_2_fcs))
    ) %>%
    dplyr::ungroup()

  char_bits <- codeword_chars %>%
    dplyr::transmute(
      code_letter = LETTERS[1:10],
      bit_num = list(12:0),
      bit = char_base_2_fcs
    ) %>%
    tidyr::unnest(c(bit_num, bit))

  bar_lettters <- bar_to_char %>%
    dplyr::left_join(
      dplyr::rename(char_bits, desc = bit),
      by = c("desc_char" = "code_letter", "desc_bit" = "bit_num")) %>%
    dplyr::left_join(
      dplyr::rename(char_bits, asc = bit),
      by = c("asc_char" = "code_letter", "asc_bit" = "bit_num")
    ) %>%
    dplyr::mutate(
      barcode_letter = dplyr::case_when(
        asc & desc ~ "F",
        asc & !desc ~ "A",
        !asc & desc ~ "D",
        !asc & !desc ~ "T"
      )
    )

  paste(bar_lettters$barcode_letter, collapse = "")
}

#############################

#' Encode an Intelligent Mail Barcode
#'
#' The sum of the number of Mailer ID digits and number of serial number digits
#' must be 15. This means that if the mailer ID is 6 digits, the serial number
#' must be 9 digits, and if the mailer ID is 9 digits, the serial number is 6 digits.
#'
#' Inputs can be passed as either string or numeric. Inputs will be padded will
#' leading zeroes. They will be padded to the minimum length of that field.
#' For example, the ZIP code will be padded to a minimum of 5 digits. If
#' an input has greater than the minimum number of digits and leading zeroes,
#' the input should be provided as a string containing the leading zeroes.
#'
#' For example, if there is a 9-digit ZIP code `012345678`, it should be provided
#' as a string containing the leading zero.
#'
#' @param barcode_id 2 digits
#' @param service_type 3 digits
#' @param mailer_id Mailer ID: 6 or 9 digits
#' @param serial_num Serial number: 6 or 9 digits.
#' @param zip ZIP code: 5, 9, or 11 digits
#'
#' @return A 65-character string made up of A, T, D, and F characters
#'
#' @examples
#' imb_encode("01", "234", "567094", "987654321", "01234567891")
#' imb_encode(1, 234, 567094, 987654321, "01234567891")
#'
#' @export
imb_encode <- function(barcode_id, service_type, mailer_id, serial_num, zip) {
  input <- parse_input(barcode_id, service_type, mailer_id, serial_num, zip)
  payload <- purrr::lift(create_payload)(input)
  codewords <- payload_to_codewords(payload)
  payload_bits <- binaryLogic::as.binary(as.integer(to_bits(payload)), logic = TRUE, n = 104)
  fcs <- crc_fcs(payload_bits)
  fcs_bits <- binaryLogic::as.binary(fcs, n = 11)
  codewords_mod <- modify_codewords(codewords, fcs_bits)
  codewords_to_barcode(codewords_mod, fcs_bits)
}


