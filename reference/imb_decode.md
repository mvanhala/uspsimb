# Decode an encoded Intelligent Mail Barcode

Decode an encoded Intelligent Mail Barcode

## Usage

``` r
imb_decode(barcode, mailer_id_len = 6)
```

## Arguments

- barcode:

  The 65-character barcode to decode

- mailer_id_len:

  The length of the Mailer ID field (6 or 9 digits)

## Value

A list containing five elements:

- `barcode_id`: 2 digits (second digits is 0-4)

- `service_type`: 3 digits

- `mailer_id`: 6 or 9 digits

- `serial_num`: 6 or 9 digits (6 if Mailer ID is 9 digits; 9 if Mailer
  ID is 6 digits)

- `zip`: 5, 9, or 11 digits

## Examples

``` r
imb_decode("AADTFFDFTDADTAADAATFDTDDAAADDTDTTDAFADADDDTFFFDDTTTADFAAADFTDAADA")
#> $barcode_id
#> [1] "01"
#> 
#> $service_type
#> [1] "234"
#> 
#> $mailer_id
#> [1] "567094"
#> 
#> $serial_num
#> [1] "987654321"
#> 
#> $zip
#> [1] "01234567891"
#> 
```
