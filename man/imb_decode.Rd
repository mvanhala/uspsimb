% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decode.R
\name{imb_decode}
\alias{imb_decode}
\title{Decode an encoded Intelligent Mail Barcode}
\usage{
imb_decode(barcode, mailer_id_len = 6)
}
\arguments{
\item{barcode}{The 65-character barcode to decode}

\item{mailer_id_len}{The length of the Mailer ID field (6 or 9 digits)}
}
\value{
A list containing five elements:
\itemize{
\item \code{barcode_id}: 2 digits (second digits is 0-4)
\item \code{service_type}: 3 digits
\item \code{mailer_id}: 6 or 9 digits
\item \code{serial_num}: 6 or 9 digits (6 if Mailer ID is 9 digits; 9 if Mailer ID is 6 digits)
\item \code{zip}: 5, 9, or 11 digits
}
}
\description{
Decode an encoded Intelligent Mail Barcode
}
\examples{
imb_decode("AADTFFDFTDADTAADAATFDTDDAAADDTDTTDAFADADDDTFFFDDTTTADFAAADFTDAADA")

}
