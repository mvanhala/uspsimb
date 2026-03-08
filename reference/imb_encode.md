# Encode an Intelligent Mail Barcode

The sum of the number of Mailer ID digits and number of serial number
digits must be 15. This means that if the mailer ID is 6 digits, the
serial number must be 9 digits, and if the mailer ID is 9 digits, the
serial number is 6 digits.

## Usage

``` r
imb_encode(barcode_id, service_type, mailer_id, serial_num, zip)
```

## Arguments

- barcode_id:

  2 digits

- service_type:

  3 digits

- mailer_id:

  Mailer ID: 6 or 9 digits

- serial_num:

  Serial number: 6 or 9 digits.

- zip:

  ZIP code: 5, 9, or 11 digits

## Value

A 65-character string made up of A, T, D, and F characters

## Details

Inputs can be passed as either string or numeric. Inputs will be padded
will leading zeroes. They will be padded to the minimum length of that
field. For example, the ZIP code will be padded to a minimum of 5
digits. If an input has greater than the minimum number of digits and
leading zeroes, the input should be provided as a string containing the
leading zeroes.

For example, if there is a 9-digit ZIP code `012345678`, it should be
provided as a string containing the leading zero.

## Examples

``` r
imb_encode("01", "234", "567094", "987654321", "01234567891")
#> Warning: `lift()` was deprecated in purrr 1.0.0.
#> ℹ The deprecated feature was likely used in the uspsimb package.
#>   Please report the issue to the authors.
#> [1] "AADTFFDFTDADTAADAATFDTDDAAADDTDTTDAFADADDDTFFFDDTTTADFAAADFTDAADA"
imb_encode(1, 234, 567094, 987654321, "01234567891")
#> [1] "AADTFFDFTDADTAADAATFDTDDAAADDTDTTDAFADADDDTFFFDDTTTADFAAADFTDAADA"
```
