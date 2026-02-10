test_that("qmatrix_generator reads stemonly codebook and responses", {
  path <- testthat::test_path("..", "..", "inst", "extdata", "codebook_qualtrics_gen.csv")

  out <- qmatrix_generator(
    inpfile = path,
    varcol = "Skalenvariable",
    itemcol = "Items",
    itemformat = "stemonly",
    responses = TRUE,
    leadin = "Instruktion",
    resp = "Antwort",
    resplabel = "Antwortlabel"
  )

  expect_type(out, "list")
  expect_equal(out$scalenames, c("csmc", "conf"))
  expect_length(out$text, 2)
  expect_true(all(grepl("Antwortoptionen", out$text, fixed = TRUE)))

  expect_equal(nrow(out$items), 12)
  expect_equal(out$items$itemno[out$items$scale == "csmc"], 1:8)
  expect_equal(out$items$itemno[out$items$scale == "conf"], 1:4)

  expect_length(out$itemtext_list$csmc, 8)
  expect_length(out$itemtext_list$conf, 4)

  expect_length(out$responses_leadin_list, 2)
  expect_length(out$responses_leadin_list$csmc$leadin, 1)
  expect_length(out$responses_leadin_list$conf$leadin, 1)
  resp_csmc <- out$responses_leadin_list$csmc$responses
  resp_conf <- out$responses_leadin_list$conf$responses
  expect_true(is.data.frame(resp_csmc))
  expect_true(is.data.frame(resp_conf))
  expect_equal(nrow(resp_csmc), 5)
  expect_equal(nrow(resp_conf), 5)
  expect_true("Neutral (3)" %in% resp_csmc$label)
})

test_that("qmatrix_to_txt assembles text with responses", {
  path <- testthat::test_path("..", "..", "inst", "extdata", "codebook_qualtrics_gen.csv")

  out <- qmatrix_to_txt(
    inpfile = path,
    varcol = "Skalenvariable",
    itemcol = "Items",
    itemformat = "stemonly",
    responses = TRUE,
    leadin = "Instruktion",
    resp = "Antwort",
    resplabel = "Antwortlabel"
  )

  expect_equal(out$scalenames, c("csmc", "conf"))
  expect_length(out$text, 2)
  expect_true(grepl("csmc.", out$text[[1]], fixed = TRUE))
  expect_true(any(grepl("Neutral (3)", out$text, fixed = TRUE)))
})
