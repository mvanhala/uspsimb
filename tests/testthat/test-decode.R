test_that("decoding", {
  expect_equal(
    imb_decode("AADTFFDFTDADTAADAATFDTDDAAADDTDTTDAFADADDDTFFFDDTTTADFAAADFTDAADA"),
    list(
      barcode_id = "01",
      service_type = "234",
      mailer_id = "567094",
      serial_num = "987654321",
      zip = "01234567891"
    )
  )
  expect_equal(
    imb_decode("AADTFFDFTDADTAADAATFDTDDAAADDTDTTDAFADADDDTFFFDDTTTADFAAADFTDAADA", mailer_id_len = 9),
    list(
      barcode_id = "01",
      service_type = "234",
      mailer_id = "567094987",
      serial_num = "654321",
      zip = "01234567891"
    )
  )
})
