########################### decode_qualities function ##########################

decode_qualities <- function(qualities, offset = 33) {

  assertthat::assert_that(assertthat::is.scalar(offset))
  assertthat::assert_that(is.numeric(offset))

  if (!(offset == 33 | offset == 64)) {
    stop("Offset can only be 33 or 64")
  }

  as.integer(charToRaw(qualities)) -> ascii_values

  ascii_values - offset -> phred_score

  if (any(phred_score < 1)) {
    stop("Some Phred scores are less than 0")
  }

  return(phred_score)
}
