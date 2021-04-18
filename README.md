# uspsimb: USPS Intelligent Mail Barcode encoder/decoder

`uspsimb` is a R package that provides functions to encode and decode the
United States Postal Service 
[Intelligent Mail barcode](https://en.wikipedia.org/wiki/Intelligent_Mail_barcode).

The encoding/decoding functions are:

* `imb_encode()`: encodes an Intelligent Mail barcode from the input elements

* `imb_decode()`: decodes an encoded barcode string, returning a list of the decoded elements

Intelligent Mail barcode encoding takes five pieces of information (a barcode identifier,
service type identifier, mailer ID, serial number, and ZIP code) and encodes them
into a 65-digit barcode. There are four different bar type characters in the barcode.

## Algorithm

The algorithm used to encode barcodes in this package follows the description in the 
USPS document
[Intelligent Mail Barcode 4-State Specification](https://postalpro.usps.com/node/2190)

* First, a payload of up to 31 digits is created with a series of additions, multiplications,
and concatenations.

* The 31-digit payload is converted to binary digits.

* An 11-bit CRC frame check sequence (FCS) is generated.

* The binary payload is converted to a set of 10 "codewords". Each codeword is an integer.
The first codeword may be modified depending on the FCS.

* Each "codeword" is mapped to a "character" (a 13-bit number). There are a total of 
130 bit (10 codewords, each 13 bits).

* Each character bit is mapped to an ascender/descender for a bar position. There are
65 bars, each with an ascender bit and descender bit, corresponding to the total of 130 bits.

* For each bar, there are four states:

    * ascender bit 1, descender bit 1: full (F)
    * ascender bit 1, descender bit 0: ascender (A)
    * ascender bit 0, descender bit 1: descender (D)
    * ascender bit 0, descender bit 0: tracker (T)

[binaryLogic](https://cran.r-project.org/package=binaryLogic) is used to perform binary
operations, and [Rmpfr](https://cran.r-project.org/package=Rmpfr) is used to store and
format the 31-digit payload.

## USPS resources

[USPS Intelligent Mail barcode landing page](https://postalpro.usps.com/mailing/intelligent-mail-barcode)

USPS maintains a [page](https://postalpro.usps.com/onecodesolution) of Intelligent Mail
barcode encoders and fonts. A C library to encode and decode barcodes, along with
C and Java sample code to call the encoder, is provider. There are also fonts that can
be installed that displays the barcode characters A (ascender), D (descender), T (tracker),
and F (full) as their corresponding bars.

USPS has a [web tool](https://postalpro.usps.com/ppro-tools/encoder-decoder)
for encoding and decoding Intelligent Mail barcodes.

## IMb scan data

You can sign up for a USPS business account in the [Business Customer Gateway](https://gateway.usps.com/)
and get a Mailer ID.

With this Mailer ID, you can create barcodes for letters. You can then view scan data
as the letter pieces get scanned as they move through the USPS system. 

IMb tracing data is accessible via the USPS [Informed Visibility](https://iv.usps.com) portal.

When creating barcodes for ordinary first-class letters, the barcode identifier
should generally be `00`. The service type identifier `040` specifies first class
mail with IMb tracing.

## Other USPS IMb encoder/decoder implementations

* [bobcodes](http://bobcodes.weebly.com/imb.html): Javascript

* [SuperSephy/imb](https://github.com/SuperSephy/imb): Javascript

* [woo-j/OkapiBarcode](https://github.com/woo-j/OkapiBarcode/blob/master/src/main/java/uk/org/okapibarcode/backend/UspsOneCode.java): Java

* [eriklyon/imb](https://github.com/eriklyon/imb): Python


