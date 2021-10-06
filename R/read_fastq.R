########################### gc_content function ################################

gc_content <- function(seq) {

  assertthat::assert_that(is.character(seq))

  if (any(stringr::str_detect(seq, "[^GATC]"))) {
    warning("Non GATC characters found in sequences!")
  }

  seq <- toupper(seq)

  stringr::str_replace_all(seq,"[^GC]","") -> GC

  return(100*(nchar(GC)/nchar(seq)))
}

########################### read_fastq function ################################

read_fastq <- function(file) {

  assertthat::assert_that(assertthat::is.readable(file) & assertthat::has_extension(file, "fq"))

  scan(file, character()) -> file_lines
  file_lines[c(TRUE, FALSE, FALSE, FALSE)] -> ids
  file_lines[c(FALSE, TRUE, FALSE, FALSE)] -> seqs
  file_lines[c(FALSE, FALSE, FALSE, TRUE)] -> qual

  if (!all(startsWith(ids, "@"))) {
    stop("Some ID lines didn't start with @")
  }

  stringr::str_sub(ids, 2) -> ids

  if (any(nchar(seqs) != nchar(qual))) {
    stop("Some sequences were a different length to the qualities")
  }

  if (any(duplicated(ids))) {
    stop("Some ID lines are duplicated")
  }

  tibble::tibble(ID = ids, Bases = seqs, Qualities = qual, GC = gc_content(seqs)) %>%
    return()

}
