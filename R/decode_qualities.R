########################### decode_qualities function ##########################

#' Title
#'
#' @param qualities  String of sequence Qualities from a fastq format file
#' @param offset Scalar number which is either 33 or 64, the only two offsets used in fastq files. Default value is 33
#'
#' @return Vector of phred scores for each character of the sequence qualities, values cannot be < 0
#' @export
#'
#' @examples decode_qualities("???#;ABAAAH")

decode_qualities <- function(qualities, offset = 33) {

  assertthat::assert_that(assertthat::is.scalar(offset))
  assertthat::assert_that(is.numeric(offset))

  if (!(offset == 33 | offset == 64)) {
    base::stop("Offset can only be 33 or 64")
  }

  as.integer(charToRaw(qualities)) -> ascii_values

  ascii_values - offset -> phred_score

  if (any(phred_score < 1)) {
    base::stop("Some Phred scores are less than 0")
  }

  return(phred_score)
}
