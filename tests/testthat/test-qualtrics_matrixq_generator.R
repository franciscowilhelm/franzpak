test_that("qmatrix_generator reads stemonly codebook and responses", {
  codebook <- data.frame(
    scale = rep(c("submarine", "lunar"), each = 5),
    item = c(
      "I am licensed to operate nuclear submarines.",
      "I can safely dock a submarine in a pineapple.",
      "I navigate by the stars while underwater.",
      "I have befriended at least one dolphin crew member.",
      "I can calculate torpedo trajectories in my head.",
      "I have walked on the Moon during lunch.",
      "I can fix a rover with duct tape.",
      "I keep a spare spacesuit in my closet.",
      "I speak fluent Martian.",
      "I have a favorite crater."
    ),
    instruction = c(
      "How much do you agree with these statements?",
      rep(NA_character_, 4),
      "How true are these statements about you?",
      rep(NA_character_, 4)
    ),
    response = rep(1:5, 2),
    response_label = rep(
      c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
      2
    ),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".csv")
  utils::write.csv(codebook, path, row.names = FALSE)

  out <- qmatrix_generator(
    inpfile = path,
    varcol = "scale",
    itemcol = "item",
    itemformat = "stemonly",
    responses = TRUE,
    leadin = "instruction",
    resp = "response",
    resplabel = "response_label"
  )

  expect_type(out, "list")
  expect_equal(out$scalenames, c("submarine", "lunar"))
  expect_length(out$text, 2)
  expect_true(grepl("submarine.", out$text[[1]], fixed = TRUE))
  expect_true(any(grepl("1 Strongly disagree", out$text, fixed = TRUE)))

  expect_equal(nrow(out$items), 10)
  expect_equal(out$items$itemno[out$items$scale == "submarine"], 1:5)
  expect_equal(out$items$itemno[out$items$scale == "lunar"], 1:5)

  expect_length(out$itemtext_list$submarine, 5)
  expect_length(out$itemtext_list$lunar, 5)

  expect_length(out$responses_leadin_list, 2)
  expect_length(out$responses_leadin_list$submarine$leadin, 1)
  expect_length(out$responses_leadin_list$lunar$leadin, 1)
  resp_sub <- out$responses_leadin_list$submarine$responses
  resp_lunar <- out$responses_leadin_list$lunar$responses
  expect_true(is.data.frame(resp_sub))
  expect_true(is.data.frame(resp_lunar))
  expect_equal(nrow(resp_sub), 5)
  expect_equal(nrow(resp_lunar), 5)
  expect_true("Strongly disagree" %in% resp_sub$label)
})
