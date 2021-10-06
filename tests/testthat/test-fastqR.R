library(fastqR)


#################### Test GC content function ##################################

test_that("GC content good parameter", {

  good_sequences <- c("GAGAGCGGCTT", "GAGAGCGGCTT", "GAGAGCGGCTT")

  expect_equal(length(gc_content(good_sequences)), 3)
  expect_true(is.numeric(gc_content(good_sequences)))
  expect_equal(gc_content("GGAATTCC"), gc_content("GATC"))
  expect_equal(gc_content("ggaattcc"), gc_content("gatc"))
})

test_that("GC content good parameter", {
  expect_error(gc_content(c(1, 2, 5)))
  expect_warning(gc_content("GAGNATCC"))
})

################### Test read_fastq function ##################################

test_that("Reads .fq file", {
  expect_warning(tibble::is_tibble(read_fastq(system.file("good.fq", package = "fastqR"))))

})
