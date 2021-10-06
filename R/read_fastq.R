

########################### gc_content function ################################

#' Calculate GC content of nucleotide sequence
#'
#' @param seq A vector containing strings of nucleotides to use
#'
#' @return Vector of percentage values of sequence GC contents
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples gc_content("GAGAGCGGCTT")

gc_content <- function(seq) {

  assertthat::assert_that(is.character(seq))

  seq <- base::toupper(seq)

  if (any(stringr::str_detect(seq, "[^GATC]"))) {
    base::warning("Non GATC characters found in sequences!")
  }


  stringr::str_replace_all(seq,"[^GC]","") -> GC

  return(100*(base::nchar(GC)/base::nchar(seq)))
}

########################### read_fastq function ################################

#' Read fastq .fq file and calculate GC content
#'
#' @param file A .fq format file
#'
#' @return A tibble with with ID, Bases, Qualities and GC content of the sequences in the fastq file
#' @export
#'
#' @examples read_fastq(system.file("good.fq", package = "fastqR"))

read_fastq <- function(file) {

  assertthat::assert_that(assertthat::is.readable(file) & assertthat::has_extension(file, "fq"))

  base::scan(file, character()) -> file_lines
  file_lines[c(TRUE, FALSE, FALSE, FALSE)] -> ids
  file_lines[c(FALSE, TRUE, FALSE, FALSE)] -> seqs
  file_lines[c(FALSE, FALSE, FALSE, TRUE)] -> qual

  if (!all(startsWith(ids, "@"))) {
    base::stop("Some ID lines didn't start with @")
  }

  stringr::str_sub(ids, 2) -> ids

  if (any(nchar(seqs) != nchar(qual))) {
    base::stop("Some sequences were a different length to the qualities")
  }

  if (any(duplicated(ids))) {
    base::stop("Some ID lines are duplicated")
  }

  tibble::tibble(ID = ids, Bases = seqs, Qualities = qual, GC = gc_content(seqs)) %>%
    return()

}

